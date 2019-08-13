module Page.App exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session, isLoggedIn)
import Data.App as App exposing (App, AppRun)
import Data.Agave as Agave
import Data.Sample as Sample exposing (Sample, SampleFile, SampleGroup)
import Data.Cart
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
import Request.SampleGroup
import Route
import Json.Decode as Decode
import Task exposing (Task)
import View.Cart as Cart
import View.Spinner exposing (spinner)
import View.FileBrowser as FileBrowser
import Dict exposing (Dict)
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
    , settings : Dict String String
    , cart : Cart.Model
    , cartLoaded : Bool
    , selectedCartId : Maybe Int
    , samples : List Sample
    , files : List SampleFile
    , sampleGroups : List SampleGroup
    , showRunDialog : Bool
    , cartDialogInputId : Maybe String
    , dialogError : Maybe String
    , filterFileType : String
    , inputId : Maybe String
    , fileBrowser : FileBrowser.Model
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

                Agave.NumberValue num -> toString num

        defaultInputs app =
            app.inputs |> List.map (\input -> (input.id, (default input.value.default))) |> DictList.fromList

        defaultParams app =
            app.parameters |> List.map (\param -> (param.id, default param.value.default)) |> DictList.fromList

        defaultSettings app =
            [ ("batchQueue", app.defaultQueue), ("maxRunTime", app.defaultMaxRunTime) ] |> Dict.fromList

        cart =
            Cart.init session.cart Cart.Selectable

        fileBrowserConfig =
            { showMenuBar = True
            , showNewFolderButton = False
            , showUploadFileButton = False
            , allowDirSelection = True
            , allowMultiSelection = True
            , allowFileViewing = False
            , homePath = Nothing
            }
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
                            , inputs = defaultInputs agaveApp
                            , parameters = defaultParams agaveApp
                            , settings = defaultSettings agaveApp
                            , cart = cart
                            , cartLoaded = False
                            , selectedCartId = Nothing -- Current
                            , samples = []
                            , files = []
                            , sampleGroups = []
                            , showRunDialog = False
                            , cartDialogInputId = Nothing
                            , dialogError = Nothing
                            , filterFileType = "All Types"
                            , inputId = Nothing
                            , fileBrowser = FileBrowser.init session (Just fileBrowserConfig)
                        }
                    )
            )
        )
        |> Task.mapError (Error.handleLoadErrorWithLogin (isLoggedIn session))


defaultBatchQueue =
    "normal"


defaultMaxRunTime =
    "12:00:00"



-- UPDATE --


type Msg
    = SetInput InputSource String String
    | SetParameter String String
    | SetSetting String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))
    | ShareJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))
    | AppRunCompleted (Result Http.Error AppRun)
    | CloseRunDialog
    | OpenFileBrowserDialog String
    | CloseFileBrowserDialog
    | OpenCart String
    | LoadCartCompleted (Result Http.Error ((List Sample), (List SampleFile), (List SampleGroup)))
    | SelectCart (Maybe Int)
    | CloseCartDialog
    | CancelCartDialog
    | FilterByFileType String
    | CartMsg Cart.Msg
    | FileBrowserMsg FileBrowser.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        isPlanB =
            model.app.provider_name == "plan-b"
    in
    case msg of
        SetInput source id value ->
            let
                curValue =
                    DictList.get id model.inputs |> Maybe.withDefault ""

                newValue =
--                    if source == "syndicate" then
--                        if model.app.provider_name == "plan-b" then
--                            String.split ";" value |> List.map (\s -> "hsyn:///gbmetagenomes/" ++ s) |> String.join ";" --FIXME hardcoded
--                        else
--                            String.split ";" value |> List.map (\s -> "https://www.imicrobe.us/syndicate/download/gbmetagenomes/fs/" ++ s) |> String.join ";" --FIXME hardcoded and duplicated in config.json
                    if source == CYVERSE && curValue /= "" then
                        curValue ++ ";" ++ value
                    else
                        value

                newInputs =
                    DictList.insert id newValue model.inputs

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
            { model | inputs = newInputs, inputId = Nothing } => Cmd.none

        SetParameter id value ->
            let
                newParams = DictList.insert id value model.parameters
            in
            { model | parameters = newParams } => Cmd.none

        SetSetting id value ->
            let
                newSettings = Dict.insert id value model.settings
            in
            { model | settings = newSettings } => Cmd.none

        RunJob -> --TODO messy, clean this up
            let
                irodsToAgave path = -- convert IRODS paths to Agave paths
                    if String.contains "/iplant/home" path then
                        String.Extra.replace "/iplant/home" "" path -- replace all instances (multiple paths separated by semicolon)
                    else
                        path

                jobInputs =
                    DictList.toList model.inputs
                        |> List.map (\(k, v) -> (k, irodsToAgave v))
                        |> List.map (\(k, v) -> Agave.JobInput k (String.split ";" v))

                encodeParam id val =
                    case List.filter (\p -> p.id == id) model.agaveApp.parameters of
                        [ param ] ->
                            case param.value.type_ of
                                "number" ->
                                    Agave.NumberValue (String.toFloat val |> Result.withDefault 0)

                                "bool" ->
                                    if val == "true" then
                                        Agave.BoolValue True
                                    else
                                        Agave.BoolValue False

                                "flag" ->
                                    if val == "true" then
                                        Agave.BoolValue True
                                    else
                                        Agave.BoolValue False

                                "enumeration" ->
                                    Agave.ArrayValue (String.split ";" val)

                                _ ->
                                    Agave.StringValue val

                        _ ->
                            Agave.StringValue val

                jobParameters =
                    DictList.toList model.parameters
                        |> List.map (\(k, v) -> Agave.JobParameter k (encodeParam k v))

                jobName =
                    "iMicrobe " ++ model.app.app_name --FIXME should be a user-inputted value?

                jobRequest =
                    Agave.JobRequest jobName model.app.app_name True jobInputs jobParameters []

                launchAgave =
                    Request.Agave.launchJob session.token jobRequest (Dict.toList model.settings)
                        |> Http.send RunJobCompleted

                launchPlanB =
                    Request.PlanB.launchJob session.token jobRequest
                        |> Http.send RunJobCompleted

                sendAppRun =
                    Request.App.run session.token model.app_id (Agave.encodeJobRequest jobRequest |> toString)
                        |> Http.send AppRunCompleted

                launchApp =
                    if isPlanB then
                        launchPlanB
                    else
                        launchAgave
            in
            { model | showRunDialog = True } => Cmd.batch [ launchApp, sendAppRun ]

        RunJobCompleted (Ok response) ->
            let
                shareJob =
                    Request.Agave.shareJob session.token response.result.id "imicrobe" "READ" |> Http.send ShareJobCompleted
            in
            model =>
                ((Route.modifyUrl (Route.Job response.result.id)
                    :: (if not isPlanB then [ shareJob ] else [])
                ) |> Cmd.batch)

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

        ShareJobCompleted _ ->
            model => Cmd.none

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

                filterOnType file =
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
                    case model.selectedCartId of
                        Nothing -> -- Current
                            List.filterMap filterOnType model.files |> String.join ";"

                        Just id ->
                            model.sampleGroups
                                |> List.filter (\g -> g.sample_group_id == id)
                                |> List.map .samples
                                |> List.concat
                                |> List.map .sample_files
                                |> List.concat
                                |> List.filterMap filterOnType
                                |> Set.fromList -- remove duplicates
                                |> Set.toList
                                |> String.join ";"

                msg =
                    SetInput CYVERSE (withDefault "" model.cartDialogInputId) filesStr
            in
            update session msg { model | cartDialogInputId = Nothing }

        CancelCartDialog ->
            { model | cartDialogInputId = Nothing } => Cmd.none

        OpenFileBrowserDialog inputId ->
            let
                (subModel, subCmd) =
                    FileBrowser.update session FileBrowser.RefreshPath model.fileBrowser
            in
            { model | inputId = Just inputId, fileBrowser = subModel } => Cmd.map FileBrowserMsg subCmd

        CloseFileBrowserDialog ->
            { model | inputId = Nothing } => Cmd.none

        OpenCart inputId ->
            let
                id_list =
                    session.cart.contents |> Set.toList

                cmd =
                    if model.cartLoaded then
                        Cmd.none
                    else if id_list == [] then -- current cart is empty
                        Task.attempt LoadCartCompleted <|
                            Task.map3 (\samples files sampleGroups -> (samples, files, sampleGroups))
                                (Task.succeed [])
                                (Task.succeed [])
                                (Request.SampleGroup.list session.token |> Http.toTask) -- load samples & files for saved carts
                    else
                        Task.attempt LoadCartCompleted <|
                            Task.map3 (\samples files sampleGroups -> (samples, files, sampleGroups))
                                (Request.Sample.getSome session.token id_list |> Http.toTask) -- load samples for current cart
                                (Request.Sample.files session.token id_list |> Http.toTask) -- load files for current cart
                                (Request.SampleGroup.list session.token |> Http.toTask) -- load samples & files for saved carts
            in
            { model | cartDialogInputId = Just inputId } => cmd

        LoadCartCompleted (Ok (samples, files, sampleGroups)) ->
            { model | samples = samples, files = files, sampleGroups = sampleGroups, cartLoaded = True } => Cmd.none

        LoadCartCompleted (Err error) -> --TODO show error to user
            let
                _ = Debug.log "Error" (toString error)
            in
            model => Cmd.none

        SelectCart maybeId ->
            let
                newCart =
                    case maybeId of
                        Nothing -> -- Current
                            Cart.init session.cart Cart.Selectable

                        Just id ->
                            let
                                cart =
                                    model.sampleGroups
                                        |> List.filter (\g -> g.sample_group_id == id)
                                        |> List.map .samples
                                        |> List.concat
                                        |> List.map .sample_id
                                        |> Set.fromList
                                        |> Data.Cart.Cart
                            in
                            Cart.init cart Cart.Selectable
            in
            { model | selectedCartId = maybeId, cart = newCart } => Cmd.none

        FilterByFileType fileType ->
            { model | filterFileType = fileType } => Cmd.none

        CartMsg subMsg ->
            let
                _ = Debug.log "App.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage )  =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd

        FileBrowserMsg subMsg ->
            let
                ( newFileBrowser, subCmd ) =
                    FileBrowser.update session subMsg model.fileBrowser
            in
            { model | fileBrowser = newFileBrowser } => Cmd.map FileBrowserMsg subCmd


type InputSource
    = CYVERSE
    | SYNDICATE
    | UI



-- VIEW --


view : Model -> Html Msg
view model =
    let
        body =
            if not model.app.is_active then
                div [ class "alert alert-info" ]
                    [ text "This app is no longer available." ]
            else if model.app.is_maintenance then
                div [ class "alert alert-info" ]
                    [ text "This app is currently unavailable due to maintenance." ]
            else
                div []
                    [ viewApp model
                    , div [ class "center" ]
                        [ hr [] []
                        , button [ class "btn btn-primary btn-lg", onClick RunJob ] [ text "Run" ]
                        ]
                    ]
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
            , body
            , Dialog.view
                (if model.showRunDialog then
                    Just (runDialogConfig model)
                 else if model.cartDialogInputId /= Nothing then
                    Just (cartDialogConfig model)
                 else if model.inputId /= Nothing then
                    Just (fileBrowserDialogConfig model.fileBrowser (model.inputId |> Maybe.withDefault "") False)
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
                    table [ class "table" ]
                        [ tbody [] (List.Extra.zip agaveApp.inputs (DictList.values model.inputs) |> List.map viewAppInput)
                        ]

        parameters =
            case agaveApp.parameters of
                [] ->
                    div [] [ text "None" ]

                _  ->
                    table [ class "table" ]
                        [ tbody [] (List.Extra.zip agaveApp.parameters (DictList.values model.parameters) |> List.map viewAppParameter)
                        ]
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
    , h3 [] [ text "Settings" ]
    , viewSettings model.settings
    ]


viewAppInput : (Agave.AppInput, String) -> Html Msg
viewAppInput input =
    let
        agaveAppInput = Tuple.first input

        val = Tuple.second input

        id = agaveAppInput.id

        label =
            if agaveAppInput.value.required then
                agaveAppInput.details.label ++ " *"
            else
                agaveAppInput.details.label

        browserButton label msg =
            button [ class "margin-right btn btn-default btn-sm", style [("max-height","2.8em")], onClick msg ]
                [ span [ class "gray gylphicon glyphicon-cloud" ] []
                , text (" " ++ label)
                ]

--        syndicateButton =
            -- mdb changed 6/27/18 -- show RefSeq button in all apps, not just Libra
--            if List.member "syndicate" agaveAppInput.semantics.ontology then
--                browserButton "GenBank" (OpenFileBrowser "syndicate" id)
--            else
--                text ""
    in
    tr []
    [ th [ class "col-md-3" ] [ text label ]
    , td []
        [ div [ style [("display","flex")] ]
            [ textarea [ class "form-control margin-right", style [("width","30em"),("min-height","2.5em")], rows 1, name id, value val, onInput (SetInput UI id) ] []
            , browserButton "Data Store" (OpenFileBrowserDialog id)
--            , syndicateButton
            , button [ class "btn btn-default btn-sm", style [("max-height","2.8em")], onClick (OpenCart id) ]
                [ span [ class "gray glyphicon glyphicon-shopping-cart" ] []
                , text " Cart"
                ]
            ]
        ]
    , td [] [ text agaveAppInput.details.description ]
    ]


viewAppParameter : (Agave.AppParameter, String) -> Html Msg
viewAppParameter parameter =
    let
        param = Tuple.first parameter

        val = Tuple.second parameter

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

        -- Hide parameters with ID's that start with double-underscore (requested by Ken/Josh)
        hidden =
            if (not param.value.visible) || String.startsWith "__" param.id then
                [ style [("display", "none")] ]
            else
                []
    in
    tr hidden
    [ th [ class "col-md-3" ] [ text param.details.label ]
    , td [ class "nowrap" ] [ interface ]
    , td [] [ text param.details.description ]
    ]


viewSettings : Dict String String -> Html Msg
viewSettings settings =
    let
        batchQueue =
            settings |> Dict.get "batchQueue" |> Maybe.withDefault defaultBatchQueue

        maxRunTime =
            settings |> Dict.get "maxRunTime" |> Maybe.withDefault defaultMaxRunTime
    in
    table [ class "table" ]
        [ tbody []
            [ tr []
                [ th [ class "col-md-3" ] [ text "Queue" ]
                , td [ class "nowrap" ]
                    [ select [ onInput (SetSetting "batchQueue") ]
                        [ option [ value "normal", selected (batchQueue == "normal") ] [ text "normal" ]
                        , option [ value "skx-normal", selected (batchQueue == "skx-normal") ] [ text "high memory" ]
                        ]
                    ]
                , td [] [ text "The queue for the job (note that the high memory queue is often much slower)" ]
                ]
            , tr []
                [ th [ class "col-md-3" ] [ text "Time limit" ]
                , td [ class "nowrap" ]
                    [ Html.input [ class "form-control", type_ "text", size 10, value maxRunTime, onInput (SetSetting "maxRunTime") ] [] ]
                , td [] [ text "The maximum run time allowed in HH:MM:SS" ]
                ]
            ]
        ]


runDialogConfig : Model -> Dialog.Config Msg
runDialogConfig model =
    let
        content =
            case model.dialogError of
                Nothing ->
                    spinner

                Just error ->
                    div [ class "alert alert-danger" ]
                        [ p [] [ text "An error occurred:" ]
                        , p [] [ text error ]
                        ]

        footer =
            if model.dialogError == Nothing then
                Just (div [] [ text " " ])
            else
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
            if model.cartLoaded then
                if Cart.size model.cart > 0 then
                    viewCart model
                else
                    text "The cart is empty"
            else
                div [ class "center" ]
                    [ div [ class "padded-xl spinner" ] [] ]

        header =
            h3 []
                [ text "Cart "
                , if model.sampleGroups /= [] then
                    viewCartDropdown model.selectedCartId model.sampleGroups
                  else
                    text ""
                ]

        closeButton =
            button [ class "btn btn-default pull-right margin-right", onClick CancelCartDialog ] [ text "Close" ]

        empty =
            Cart.size model.cart == 0

        footer =
            if not model.cartLoaded || empty then
                closeButton
            else
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
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just header
    , body = Just content
    , footer = Just footer
    }


viewCartDropdown : Maybe Int -> List SampleGroup -> Html Msg
viewCartDropdown selectedCartId sampleGroups =
    let
        mkOption (id, label) =
            li [] [ a [ onClick (SelectCart id) ] [ text label ] ]

        currentOpt =
            (Nothing, "Current")

        labels =
            currentOpt :: (sampleGroups |> List.sortBy .group_name |> List.map (\g -> (Just g.sample_group_id, g.group_name)))

        options =
            labels |> List.map mkOption

        btnLabel =
            List.Extra.find (\l -> Tuple.first l == selectedCartId) labels |> Maybe.withDefault currentOpt |> Tuple.second
    in
    if sampleGroups == [] then
        text ""
    else
        div [ style [ ("display", "inline-block") ] ]
            [ div [ class "dropdown margin-right", style [ ("display", "inline-block") ] ]
                [ button [ class "btn btn-default dropdown-toggle margin-top-bottom", attribute "type" "button", id "dropdownMenu1", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "true" ]
                    [ text btnLabel
                    , text " "
                    , span [ class "caret" ] []
                    ]
                , ul [ class "dropdown-menu", attribute "aria-labelledby" "dropdownMenu1" ]
                    options
                ]
            ]


viewCart : Model -> Html Msg
viewCart model =
    let
        samples =
            case model.selectedCartId of
                Nothing -> -- Current
                    model.samples

                Just id ->
                    model.sampleGroups
                        |> List.filter (\g -> g.sample_group_id == id)
                        |> List.map .samples
                        |> List.concat
    in
    div [ class "scrollable-half" ] [ Cart.viewCart model.cart samples |> Html.map CartMsg ]


viewFileTypeSelector : Model -> Html Msg
viewFileTypeSelector model =
    let
        types =
            case model.selectedCartId of
                Nothing -> -- Current
                    model.files
                        |> List.map (.sample_file_type >> .file_type)
                        |> Set.fromList
                        |> Set.toList

                Just id ->
                    model.sampleGroups
                        |> List.filter (\g -> g.sample_group_id == id)
                        |> List.map .samples
                        |> List.concat
                        |> List.map .sample_files
                        |> List.concat
                        |> List.map (.sample_file_type >> .file_type)
                        |> Set.fromList
                        |> Set.toList

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


fileBrowserDialogConfig : FileBrowser.Model -> String -> Bool -> Dialog.Config Msg
fileBrowserDialogConfig fileBrowser inputId isBusy =
    let
        content =
            if isBusy then
                spinner
            else
                div [ class "scrollable", style [ ("min-height","50vh"),("max-height","50vh") ] ]
                    [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]

        footer =
            let
                selectedFilepaths =
                    FileBrowser.getSelected fileBrowser |> List.map .path |> String.join ";"
            in
            div []
                [ button [ class "btn btn-default pull-left", onClick CloseFileBrowserDialog ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick (SetInput CYVERSE inputId selectedFilepaths) ] [ text "Select" ]
                ]
    in
    { closeMessage = Just CloseFileBrowserDialog
    , containerClass = Just "wide-modal-container"
    , header = Just (h3 [] [ text "Add Files" ])
    , body = Just content
    , footer = Just footer
    }