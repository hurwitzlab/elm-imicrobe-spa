module Page.App exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.App as App exposing (App, AppRun)
import Data.Agave as Agave
import Data.Sample as Sample exposing (Sample, SampleFile)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.App
import Request.Agave
import Request.Sample
import Route
import Ports
import Task exposing (Task)
import View.Cart as Cart
import Dict as Dict exposing (Dict)
import List.Extra
import String.Extra
import Util exposing ((=>))
import Set



---- MODEL ----


type alias Model =
    { pageTitle : String
    , app_id : Int
    , app : App
    , agaveApp : Agave.App
    , inputs : Dict String String
    , parameters : Dict String String
    , cart : Cart.Model
    , cartLoaded : Bool
    , samples : List Sample
    , files : List SampleFile
    , showRunDialog : Bool
    , cartDialogInputId : Maybe String
    , dialogError : Maybe String
    , filterFileType : String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadApp =
            Request.App.get id |> Http.toTask

        loadAppFromAgave name =
            Request.Agave.getApp session.token name |> Http.toTask |> Task.map .result

        default val =
            case val of
                Agave.StringValue val -> val

                Agave.ArrayValue arr -> String.join ";" arr

                Agave.BoolValue bool -> ""

        inputs app =
            app.inputs |> List.map (\input -> (input.id, (default input.value.default))) |> Dict.fromList

        params app =
            app.parameters |> List.map (\param -> (param.id, default param.value.default)) |> Dict.fromList

        cart =
            Cart.init session.cart Cart.Selectable
    in
    loadApp |> Task.andThen
        (\app ->
            (loadAppFromAgave app.app_name |> Task.andThen
                (\agaveApp -> Task.succeed (Model "App" id app agaveApp (inputs agaveApp) (params agaveApp) cart False [] [] False Nothing Nothing "All Types"))
            )
        )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetInput String String
    | SetParameter String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))
    | AppRunCompleted (Result Http.Error AppRun)
    | CloseRunDialog
    | OpenFileBrowser String
    | OpenCart String
    | LoadCartCompleted (Result Http.Error ((List Sample), (List SampleFile)))
    | CloseCartDialog
    | CancelCartDialog
    | FilterByFileType String
    | CartMsg Cart.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SetInput id value ->
            let
                newValue =
                    case String.startsWith "/iplant/home" value of
                        True -> String.Extra.replace "/iplant/home" "" value
                        False -> value

                newInputs = Dict.insert id newValue model.inputs

--                exts =
--                    String.split ";" value |> List.map (\a -> String.split "." a |> List.reverse |> List.head)
--
--                aliases =
--                    List.map .alias_ model.app.app_data_types |> List.concatMap (String.split ",")
--
--                isSupported ext =
--                    List.member ext aliases
----                    case ext of
----                        Nothing -> False
----                        Just ext -> List.member ext aliases
--
--                unsupportedExt =
--                    List.filter isSupported aliases
--
--                _ = Debug.log "unsupportedExt" unsupportedExt
--
--                _ = Debug.log "aliases" aliases
--
--                _ = Debug.log "ext" exts
            in
            { model | inputs = newInputs } => Cmd.none

        SetParameter id value ->
            let
                newParams = Dict.insert id value model.parameters
            in
            { model | parameters = newParams } => Cmd.none

        RunJob ->
            let
                jobInputs =
                    Dict.toList model.inputs |> List.map (\(k, v) -> Agave.JobInput k v)

                jobParameters =
                    Dict.toList model.parameters |> List.map (\(k, v) -> Agave.JobParameter k v)

                jobName = "iMicrobe " ++ model.app.app_name --FIXME should be a user-inputted value?

                jobRequest =
                    Agave.JobRequest jobName model.app.app_name True jobInputs jobParameters []

                cmd1 = Request.Agave.launchJob session.token jobRequest
                    |> Http.send RunJobCompleted

                cmd2 = Request.App.run model.app_id session.user_id (Agave.encodeJobRequest jobRequest |> toString) --FIXME user_id hardcoded to mbomhoff
                    |> Http.send AppRunCompleted
            in
            { model | showRunDialog = True } => Cmd.batch [ cmd1, cmd2 ]

        RunJobCompleted (Ok response) ->
            --TODO add job to app_run table
            model => Route.modifyUrl (Route.Job response.result.id)

        RunJobCompleted (Err error) ->
            { model | dialogError = Just (toString error) } => Cmd.none

        AppRunCompleted (Ok response) ->
            model => Cmd.none

        AppRunCompleted (Err error) ->
            model => Cmd.none

        CloseRunDialog ->
            { model | showRunDialog = False } => Cmd.none

        CloseCartDialog ->
            let
                selected =
                    model.cart.selected

                sampleIds =
                    Set.toList selected.contents

                match file =
                    let
                        fileType =
                            String.toLower file.sample_file_type.file_type

                        filterFileType =
                            String.toLower model.filterFileType
                    in
                    if (List.member file.sample_id sampleIds && (fileType == filterFileType || filterFileType == "all types") ) then
                        Just file.file
                    else
                        Nothing

                filesStr =
                    List.filterMap match model.files |> String.join ";"

                msg =
                    SetInput (Maybe.withDefault "" model.cartDialogInputId) filesStr
            in
            update session msg { model | cartDialogInputId = Nothing }

        CancelCartDialog ->
            { model | cartDialogInputId = Nothing } => Cmd.none

        OpenFileBrowser inputId ->
            let
                username =
                    case session.profile of
                        Nothing ->
                            ""

                        Just profile ->
                            profile.username
            in
            model => Ports.createFileBrowser (App.FileBrowser inputId username session.token "")

        OpenCart inputId ->
            let
                id_list =
                    session.cart.contents |> Set.toList

                cmd =
                    Task.attempt LoadCartCompleted <|
                        Task.map2 (\samples files -> (samples, files))
                            (Request.Sample.getSome id_list |> Http.toTask)
                            (Request.Sample.files id_list |> Http.toTask)
            in
            { model | cartDialogInputId = Just inputId } => cmd

        LoadCartCompleted (Ok (samples, files)) ->
            { model | samples = samples, files = files, cartLoaded = True } => Cmd.none

        LoadCartCompleted (Err error) ->
            model => Cmd.none

        FilterByFileType fileType ->
            { model | filterFileType = fileType } => Cmd.none

        CartMsg subMsg ->
            let
                _ = Debug.log "App.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage )  =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd



-- VIEW --


view : Model -> Html Msg
view model =
    let
        showCartDialog =
            case model.cartDialogInputId of
                Nothing -> False
                _ -> True
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.app.app_name ]
                    ]
                ]
            ]
            , viewApp model
            , div [ class "center" ]
                [ hr [] []
                , button [ class "btn btn-primary btn-lg", onClick RunJob ] [ text "Run" ]
                ]
            , Dialog.view
                (if model.showRunDialog then
                    Just (runDialogConfig model)
                 else if showCartDialog then
                    Just (cartDialogConfig model)
                 else
                    Nothing
                )
        ]


viewApp : Model -> Html Msg
viewApp model =
    let
        app = model.app

        agaveApp = model.agaveApp

        inputs =
            case agaveApp.inputs of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] [ tbody [] (List.map viewAppInput (List.Extra.zip (List.sortBy .id agaveApp.inputs) (Dict.values model.inputs))) ] --FIXME simplify this

        parameters =
            case agaveApp.parameters of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] [ tbody [] (List.map viewAppParameter (List.Extra.zip (List.sortBy .id agaveApp.parameters) (Dict.values model.parameters))) ] --FIXME simplify this
    in
    div []
    [ table [ class "table" ]
        [ colgroup []
                [ col [ class "col-md-2" ] [] ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text app.app_name ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text agaveApp.shortDescription ]
            ]
        , tr []
            [ th [] [ text "Help" ]
            , td [] [ a [ href agaveApp.helpURI, target "_blank" ] [ text agaveApp.helpURI ] ]
            ]
        , tr []
            [ th [] [ text "Version" ]
            , td [] [ text agaveApp.version ]
            ]
        , tr []
            [ th [] [ text "Tags" ]
            , td [] [ text (List.map .value app.app_tags |> List.sort |> String.join ", ") ]
            ]
        ]
    , h3 [] [ text "Inputs" ]
    , inputs
    , h3 [] [ text "Parameters" ]
    , parameters
    ]


viewAppInput : (Agave.AppInput, String) -> Html Msg
viewAppInput input =
    let
        agaveAppInput = Tuple.first input

        val = Tuple.second input

        id = agaveAppInput.id

        label =
            case agaveAppInput.value.required of
                True -> agaveAppInput.details.label ++ " *"

                False -> agaveAppInput.details.label
    in
    tr []
    [ th [ class "col-md-3" ] [ text label ]
    , td []
        [ div [ style [("display","flex")] ]
            [ textarea [ class "form-control margin-right", style [("width","30em"),("min-height","2.5em")], rows 1, name id, value val, onInput (SetInput id) ] []
            , button [ class "margin-right btn btn-default btn-sm", style [("max-height","2.8em")], onClick (OpenFileBrowser id) ]
                [ span [ class "gray gylphicon glyphicon-cloud" ] []
                , text " CyVerse"
                ]
            , button [ class "btn btn-default btn-sm", style [("max-height","2.8em")], onClick (OpenCart id) ]
                [ span [ class "gray glyphicon glyphicon-shopping-cart" ] []
                , text " Cart"
                ]
            ]
        ]
    , td [] [ text agaveAppInput.details.description ]
    ]


viewAppParameter : (Agave.AppParameter, String) -> Html Msg
viewAppParameter input =
    let
        param = Tuple.first input

        val = Tuple.second input

        id = param.id

        defaultInput len =
            Html.input [ class "form-control", type_ "text", size len, name id, value val, onInput (SetParameter id) ] []

        checkbox =
            label []
                [ Html.input [ type_ "checkbox", onCheck (toString >> (SetParameter id)) ] []
                ]

        interface =
            case param.value.type_ of
                "number" ->
                    defaultInput 10

                "bool" ->
                    checkbox

                "flag" ->
                    checkbox

                "enumeration" ->
                    case param.value.enum_values of
                        Nothing -> -- an error in the parameter definition
                            defaultInput 40

                        Just enum ->
                            select [ onInput (SetParameter id) ]
                                (enum |> List.map (List.head >> Maybe.withDefault ("error", "error")) |> List.map (\(val, label) -> option [ value val] [ text label ]))
                _ ->
                    defaultInput 40
    in
    tr []
    [ th [ class "col-md-3" ] [ text param.details.label ]
    , td [ class "nowrap" ] [ interface ]
    , td [] [ text param.details.description ]
    ]


runDialogConfig : Model -> Dialog.Config Msg
runDialogConfig model =
    let
        content =
            case model.dialogError of
                Nothing ->
                    div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

                Just error ->
                    text error

        footer =
            case model.dialogError of
                Nothing -> Just (div [] [ text " " ])

                _ ->
                    Just
                        (button
                            [ class "btn btn-success"
                            , onClick CloseRunDialog
                            ]
                            [ text "OK" ]
                        )
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Submitting Job" ])
    , body = Just content
    , footer = footer
    }


cartDialogConfig : Model -> Dialog.Config Msg
cartDialogConfig model =
    let
        content =
            case Cart.size model.cart > 0 of
                True ->
                    case model.cartLoaded of
                        True ->
                            viewCart model

                        False ->
                            div [ class "center" ]
                                [ div [ class "padded-xl spinner" ] [] ]

                False ->
                    text "Your cart is empty"

        count =
            List.length model.samples

        disable =
            not (model.cartLoaded && count > 0)

        closeButton =
            button [ class "btn btn-secondary pull-right margin-right", onClick CancelCartDialog ]
                            [ text "Close" ]

        footer =
            case disable of
                False ->
                    div [ style [("display","inline")] ]
                        [ button [ class "btn btn-default pull-left", onClick Cart.SelectAllInCart ]
                            [ text "Select All" ] |> Html.map CartMsg
                        , button [ class "btn btn-default pull-left", onClick Cart.UnselectAllInCart ]
                            [ text "Unselect All" ] |> Html.map CartMsg
                        , div [ class "pull-left", style [("margin-left","2em")] ] [ viewFileTypeSelector model ]
                        , button [ class "btn btn-primary pull-right" , onClick CloseCartDialog ]
                            [ text "Select" ]
                        , closeButton
                        ]

                True ->
                    closeButton
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Cart" ])
    , body = Just content
    , footer = Just footer
    }


viewCart : Model -> Html Msg
viewCart model =
    case model.samples of
        [] -> text "Your cart is empty"

        _ ->
            div [ class "scrollable-half" ] [ Cart.viewCart model.cart model.samples |> Html.map CartMsg ]


-- This function was copied from Page.File, find a way to merge into a single copy
viewFileTypeSelector : Model -> Html Msg
viewFileTypeSelector model =
    let
        types =
            Set.toList <| Set.fromList <| List.map (.sample_file_type >> .file_type) model.files

        numTypes =
            List.length types

        btn label =
            button [ class "btn btn-default", onClick (FilterByFileType label) ] [ text label ]

        lia label =
            li [] [ a [ onClick (FilterByFileType label) ] [ text label ] ]

        selectedType =
            case model.filterFileType of
                "All Types" -> "File Type "

                _ -> model.filterFileType ++ " "
    in
--    if (numTypes <= 1) then
--        text ""
--    else if (numTypes <= 4) then
--        div [ class "btn-group", attribute "role" "group", attribute "aria-label" "..."]
--           (btn "All" :: List.map (\t -> btn t) types)
--    else
        div [ class "dropup" ]
            [ button
                [ class "btn btn-default dropdown-toggle", id "dropdownMenu1",
                    attribute "type" "button", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "true"
                ]
                [ text selectedType
                , span [ class "caret" ] []
                ]
            , ul [ class "dropdown-menu", style [("overflow-y","scroll"),("max-height","200px")], attribute "aria-labelledby" "dropdownMenu1" ]
                (lia "All Types" :: List.map (\t -> lia t) types)
            ]
