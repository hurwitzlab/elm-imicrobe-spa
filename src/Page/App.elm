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
import Request.PlanB
import Request.Sample
import Route
import Ports
import Json.Decode as Decode
import Task exposing (Task)
import View.Cart as Cart
import DictList exposing (DictList)
import List.Extra
import String.Extra
import Maybe exposing (withDefault)
import Util exposing ((=>))
import Set



---- MODEL ----


type alias Model =
    { pageTitle : String
    , app_id : Int
    , app : App
    , agaveApp : Agave.App
    , inputs : DictList String String
    , parameters : DictList String String
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

        loadAppFromPlanB name =
            Request.PlanB.getApp session.token name |> Http.toTask |> Task.map .result

        loadAppFromProvider app =
            case app.provider_name of
                "plan-b" -> loadAppFromPlanB

                _ -> loadAppFromAgave

        default val =
            case val of
                Agave.StringValue val -> val

                Agave.ArrayValue arr -> String.join ";" arr

                Agave.BoolValue bool -> ""

        inputs app =
            app.inputs |> List.map (\input -> (input.id, (default input.value.default))) |> DictList.fromList

        params app =
            app.parameters |> List.map (\param -> (param.id, default param.value.default)) |> DictList.fromList

        cart =
            Cart.init session.cart Cart.Selectable
    in
    loadApp |> Task.andThen
        (\app ->
            ((loadAppFromProvider app) app.app_name
                |> Task.andThen
                    (\agaveApp ->
                        Task.succeed
                            { pageTitle = "App"
                            , app_id = id
                            , app = app
                            , agaveApp = agaveApp
                            , inputs = (inputs agaveApp)
                            , parameters = (params agaveApp)
                            , cart = cart
                            , cartLoaded = False
                            , samples = []
                            , files = []
                            , showRunDialog = False
                            , cartDialogInputId = Nothing
                            , dialogError = Nothing
                            , filterFileType = "All Types"
                        }
                    )
            )
        )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetInput String String String --FIXME change source (1st arg) to union type
    | SetParameter String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))
    | AppRunCompleted (Result Http.Error AppRun)
    | CloseRunDialog
    | OpenFileBrowser String String
    | OpenCart String
    | LoadCartCompleted (Result Http.Error ((List Sample), (List SampleFile)))
    | CloseCartDialog
    | CancelCartDialog
    | FilterByFileType String
    | CartMsg Cart.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SetInput source id value ->
            let
                newValue =
                    case source of
                        "syndicate" ->
                            "hsyn:///imicrobe/" ++ value

                        _ -> -- "agave"
                            case String.startsWith "/iplant/home" value of
                                True -> String.Extra.replace "/iplant/home" "" value
                                False -> value

                newInputs = DictList.insert id newValue model.inputs

--          Verify file types FIXME
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
                newParams = DictList.insert id value model.parameters
            in
            { model | parameters = newParams } => Cmd.none

        RunJob ->
            let
                jobInputs =
                    DictList.toList model.inputs |> List.map (\(k, v) -> Agave.JobInput k v)

                jobParameters =
                    DictList.toList model.parameters |> List.map (\(k, v) -> Agave.JobParameter k v)

                jobName = "iMicrobe " ++ model.app.app_name --FIXME should be a user-inputted value?

                jobRequest =
                    Agave.JobRequest jobName model.app.app_name True jobInputs jobParameters []

                launchAgave = Request.Agave.launchJob session.token jobRequest
                    |> Http.send RunJobCompleted

                launchPlanB = Request.PlanB.launchJob session.token jobRequest
                    |> Http.send RunJobCompleted

                sendAppRun = Request.App.run model.app_id session.user_id (Agave.encodeJobRequest jobRequest |> toString)
                    |> Http.send AppRunCompleted

                launchApp =
                    case model.app.provider_name of
                        "plan-b" -> launchPlanB

                        _ -> launchAgave
            in
            { model | showRunDialog = True } => Cmd.batch [ launchApp, sendAppRun ]

        RunJobCompleted (Ok response) ->
            --TODO add job to app_run table
            model => Route.modifyUrl (Route.Job response.result.id)

        RunJobCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error)

                errorMsg =
                    case error of
                        Http.BadStatus response ->
                            case response.status.code of
                                412 ->
                                    Just "This app is currently unavailable due to CyVerse maintenance. Please try again later."

                                _ ->
                                    case Decode.decodeString Agave.decoderJobError response.body of
                                        Ok result ->
                                            Just result.message

                                        Err error ->
                                            Just response.body

                        _ -> Just (toString error)
            in
            { model | dialogError = errorMsg } => Cmd.none

        AppRunCompleted (Ok response) ->
            model => Cmd.none

        AppRunCompleted (Err error) ->
            model => Cmd.none

        CloseRunDialog ->
            { model | showRunDialog = False, dialogError = Nothing } => Cmd.none

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
                    SetInput "agave" (withDefault "" model.cartDialogInputId) filesStr
            in
            update session msg { model | cartDialogInputId = Nothing }

        CancelCartDialog ->
            { model | cartDialogInputId = Nothing } => Cmd.none

        OpenFileBrowser source inputId ->
            let
                username =
                    case session.profile of
                        Nothing ->
                            ""

                        Just profile ->
                            profile.username
            in
            model => Ports.createFileBrowser (App.FileBrowser inputId username session.token "" source)

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
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ] [ tbody [] (List.Extra.zip agaveApp.inputs (DictList.values model.inputs) |> List.map viewAppInput) ]

        parameters =
            case agaveApp.parameters of
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ] [ tbody [] (List.Extra.zip agaveApp.parameters (DictList.values model.parameters) |> List.map viewAppParameter) ]
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

        browserButton label msg =
            button [ class "margin-right btn btn-default btn-sm", style [("max-height","2.8em")], onClick msg ]
                [ span [ class "gray gylphicon glyphicon-cloud" ] []
                , text (" " ++ label)
                ]

        syndicateButton =
            case List.member "syndicate" agaveAppInput.semantics.ontology of
                True ->
                    browserButton "Syndicate" (OpenFileBrowser "syndicate" id)

                False ->
                    text ""
    in
    tr []
    [ th [ class "col-md-3" ] [ text label ]
    , td []
        [ div [ style [("display","flex")] ]
            [ textarea [ class "form-control margin-right", style [("width","30em"),("min-height","2.5em")], rows 1, name id, value val, onInput (SetInput "agave" id) ] []
            , browserButton "CyVerse" (OpenFileBrowser "agave" id)
            , syndicateButton
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
                                (enum
                                    |> List.map (List.head >> withDefault ("error", "error"))
                                    |> List.map (\(value_, label) -> option [ value value_, selected (value_ == val) ] [ text label ])
                                )

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
                    div [ class "alert alert-danger" ]
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text error ]
                        ]

        footer =
            case model.dialogError of
                Nothing -> Just (div [] [ text " " ])

                _ ->
                    Just
                        (button
                            [ class "btn btn-default"
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
            button [ class "btn btn-default pull-right margin-right", onClick CancelCartDialog ]
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
