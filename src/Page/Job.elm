module Page.Job exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session, isLoggedIn)
import Data.Agave as Agave exposing (FileResult)
import Data.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError, errorString)
import Request.Agave
import Request.PlanB
import Request.App
import Route
import Ports
import Task exposing (Task)
import Dict exposing (Dict)
import Util exposing ((=>), dropFileName)
import Time exposing (Time)
import String.Extra
import List.Extra
import View.Spinner exposing (spinner)
import View.FileBrowser as FileBrowser
import Config exposing (apiBaseUrl)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , username : String
    , token : String
    , job_id : String
    , job : Agave.Job
    , app : App.App
    , loadingJob : Bool
    , loadingHistory : Bool
    , loadedHistory : Bool
    , history : List Agave.JobHistory
    , loadingResults : Bool
    , loadedResults : Bool
    , results : Maybe (List (String, String))
    , startTime : Maybe Time
    , lastPollTime : Maybe Time
    , showCancelDialog : Bool
    , cancelDialogMessage : Maybe String
    , fileBrowser : Maybe FileBrowser.Model
    }


init : Session -> String -> Task PageLoadError Model
init session id =
    let
        loadJobFromAgave =
            Request.Agave.getJob session.token id |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            Request.PlanB.getJob session.token id |> Http.toTask |> Task.map .result

        loadJob =
            if isPlanB id then
                loadJobFromPlanB
            else
                loadJobFromAgave

        loadApp app_name =
            Request.App.getByName app_name |> Http.toTask

        username =
            session.user |> Maybe.map .user_name |> Maybe.withDefault ""
    in
    loadJob
        |> Task.andThen
            (\job ->
                ((loadApp job.app_id)
                    |> Task.andThen
                        ( \app ->
                            Task.succeed
                                { pageTitle = "Job"
                                , username = username
                                , token = session.token
                                , job_id = job.id
                                , job = job
                                , app = app
                                , loadingJob = False
                                , loadingHistory = False
                                , loadedHistory = False
                                , history = []
                                , loadingResults = False
                                , loadedResults = False
                                , results = Nothing
                                , startTime = Nothing
                                , lastPollTime = Nothing
                                , showCancelDialog = False
                                , cancelDialogMessage = Nothing
                                , fileBrowser = Nothing
                                }
                        )
                )
            )
        |> Task.mapError (Error.handleLoadErrorWithLogin (isLoggedIn session))


isPlanB : String -> Bool
isPlanB id =
    String.startsWith "planb" id



-- UPDATE --


type Msg
    = GetHistory
    | SetHistory (List Agave.JobHistory)
    | ShowOutputs
    | GetResults
    | SetResults (Result Http.Error (List (String, String)))
    | SetJob Agave.Job
    | PollJob Time
    | CancelJob
    | CancelJobCompleted (Result Http.Error Agave.Job)
    | CloseCancelDialog
    | FileBrowserMsg FileBrowser.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        username =
            session.user |> Maybe.map .user_name |> Maybe.withDefault ""

        loadJobFromAgave =
            Request.Agave.getJob session.token model.job.id |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            Request.PlanB.getJob session.token model.job.id |> Http.toTask |> Task.map .result

        loadJob =
            if isPlanB model.job_id then
                loadJobFromPlanB
            else
                loadJobFromAgave
    in
    case msg of
        GetHistory ->
            let
                loadHistoryFromAgave =
                    Request.Agave.getJobHistory session.token model.job_id |> Http.toTask |> Task.map .result

                loadHistoryFromPlanB =
                    Request.PlanB.getJobHistory session.token model.job_id |> Http.toTask |> Task.map .result

                loadHistory =
                    if isPlanB model.job_id then
                        loadHistoryFromPlanB
                    else
                       loadHistoryFromAgave

                handleHistory history =
                    case history of
                        Ok history ->
                            SetHistory history

                        Err _ ->
                            let
                                _ = Debug.log "Error" "could not retrieve job history"
                            in
                            SetHistory []
            in
            { model | loadingHistory = True } => Task.attempt handleHistory loadHistory

        SetHistory history ->
--            let
--                filtered =
--                    List.filter (\output -> output.name /= ".") outputs
--            in
            { model | history = history, loadingHistory = False, loadedHistory = True } => Cmd.none

        ShowOutputs ->
            let
                defaultConfig =
                    FileBrowser.defaultConfig

                outputsPath =
                    model.job.owner ++ "/archive/jobs/job-" ++ model.job_id

                fileBrowser =
                    FileBrowser.init session (Just { defaultConfig | showMenuBar = False, homePath = Just outputsPath })

                (subModel, subCmd) =
                    FileBrowser.update session FileBrowser.RefreshPath fileBrowser
            in
            { model | fileBrowser = Just subModel } => Cmd.map FileBrowserMsg subCmd

        GetResults -> -- this code is a little complicated
            let
                loadOutputs path =
                    Request.Agave.getJobOutputs model.job.owner session.token model.job_id (Just path)
                        |> Http.toTask
                        |> Task.map .result
                        |> Task.map (List.filter (\r -> r.name /= "." && String.endsWith ".tab" r.name) >> List.map .path) -- filter out current path "." #FIXME hardcoded for .tab files (for ohana-blast) 

                -- Expects relative path
                loadOutput path =
                    Request.Agave.getJobOutput model.job.owner session.token model.job_id path
                        |> Http.toTask |> Task.map (\data -> List.singleton (path, data))

                -- Expects full path
                loadFile path =
                    Request.Agave.getFile session.token path
                        |> Http.toTask |> Task.map (\data -> List.singleton (path, data))

                -- Gets a single file or every file in a directory if path ends in "/"
                loadResultData path =
                    if String.endsWith "/" path then
                        -- Get contents of every file in the path
                        loadOutputs path
                            |> Task.andThen
                                (\outputs -> outputs |> List.map loadFile |> Task.sequence |> Task.map List.concat)
                    else
                        loadOutput path

                loadResults =
                    model.app.app_results |> List.map (loadResultData << .path) |> Task.sequence |> Task.map List.concat
            in
            { model | loadingResults = True } => Task.attempt SetResults loadResults

        SetResults (Ok results) ->
            case results of
                [] -> { model | loadedResults = True } => Cmd.none -- File not found

                _ ->
                    let
                        datasets =
                            List.Extra.lift2 (\a b -> (a.app_data_type.name, Tuple.first b, Tuple.second b)) model.app.app_results results
                            -- TODO change createSimPlot port to accept record instead of list of tuples
                    in
                    { model | loadedResults = True, results = Just results } => Ports.createSimPlot ("sim-plot", datasets)

        SetResults (Err error) ->
            let
                _ = Debug.log "Page.Job" ("Error retrieving results: " ++ (toString error))
            in
            { model | loadedResults = True }  => Cmd.none

        SetJob job ->
            { model | job = job, loadingJob = False } => Cmd.none

        PollJob time ->
            if model.loadingJob == False && isRunning model.job then
                let
                    _ = Debug.log "Job.Poll" ("polling job " ++ (toString model.job.id))

                    startTime =
                        case model.startTime of
                            Nothing -> time

                            Just t -> t

                    lastPollTime =
                        case model.lastPollTime of
                            Nothing -> time

                            Just t -> t

                    timeSinceStart =
                        time - startTime

                    timeSinceLastPoll =
                        time - lastPollTime

                    handleJob job =
                        case job of
                            Ok job ->
                                SetJob job

                            Err error ->
                                let
                                    _ = Debug.log "Error" ("could not poll job" ++ (toString error))
                                in
                                SetJob model.job

                    doPoll =
                        -- Poll every 10 seconds if job has been running less than 15 minutes
                        if timeSinceStart < (15 * Time.minute) && timeSinceLastPoll >= (10 * Time.second) then
                            True
                        -- Poll every 30 seconds if job has been running less than 30 minutes
                        else if timeSinceStart < (30 * Time.minute) && timeSinceLastPoll >= (30 * Time.second) then
                            True
                        -- Poll every 60 seconds if job has been running longer than 30 minutes
                        else if timeSinceStart >= (30 * Time.minute) && timeSinceLastPoll >= (60 * Time.second) then
                            True
                        else
                            False
                in
                if doPoll then
                    { model | loadingJob = True, startTime = Just startTime, lastPollTime = Just time } => Task.attempt handleJob loadJob
                else
                    { model | startTime = Just startTime, lastPollTime = Just time } => Cmd.none
            else
                model => Cmd.none

        CancelJob ->
            let
                stopJob =
                    Request.Agave.stopJob session.token model.job_id
                        |> Http.toTask
                        |> Task.andThen (\_ -> loadJob)
            in
            { model | showCancelDialog = True, cancelDialogMessage = Nothing } => Task.attempt CancelJobCompleted stopJob

        CancelJobCompleted (Ok job) ->
            let
                msg =
                    "A cancellation request was sent.  This may or may not result in the termination of the job depending on its state."
            in
            { model | cancelDialogMessage = Just msg, job = job } => Cmd.none

        CancelJobCompleted (Err error) ->
            { model | cancelDialogMessage = Just (errorString error) }  => Cmd.none

        CloseCancelDialog ->
            { model | showCancelDialog = False } => Cmd.none

        FileBrowserMsg subMsg ->
            case model.fileBrowser of
                Nothing ->
                    model => Cmd.none

                Just fileBrowser ->
                    let
                        ( newFileBrowser, subCmd ) =
                            FileBrowser.update session subMsg fileBrowser
                    in
                    { model | fileBrowser = Just newFileBrowser } => Cmd.map FileBrowserMsg subCmd



-- VIEW --


view : Model -> Html Msg
view model =
    let
        label =
            String.Extra.replace "_" " " model.job.status -- replace _ with space
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.job.name ]
                    , small [ class "pull-right", style [("padding-top","0.6em")] ]
                        [ text ("Status: " ++ label) ]
                    ]
                ]
            , viewJob model
            , viewInputs model.job.inputs
            , viewParameters model.job.parameters
            , if not (isPlanB model.job.id) then
                viewSettings model.job
              else
                text ""
            , viewHistory model
            , viewOutputs model
            , viewResults model
            ]
        , Dialog.view
            (if model.showCancelDialog then
                Just (cancelDialogConfig model)
            else
                Nothing
            )
        ]


viewJob : Model -> Html Msg
viewJob model =
    div []
        [ table [ class "table" ]
            [ colgroup []
                [ col [ class "col-md-1" ] []
                , col [ class "col-md-4" ] []
                ]
            , tr []
                [ th [] [ text "ID" ]
                , td [] [ text model.job.id ]
                ]
            , tr []
                [ th [] [ text "Name" ]
                , td [] [ text model.job.name ]
                ]
            , tr []
                [ th [] [ text "App" ]
                , td [] [ a [ Route.href (Route.App (toString model.app.app_id)) ] [ text model.job.app_id ] ]
                ]
            , tr []
                [ th [] [ text "Owner" ]
                , td [] [ text model.job.owner ]
                ]
            , tr []
                [ th [] [ text "Start Time" ]
                , td [] [ text model.job.created ]
                ]
            , tr []
                [ th [] [ text "End Time" ]
                , td [] [ text model.job.ended ]
                ]
            , tr []
                [ th [ class "top" ] [ text "Status" ]
                , td []
                    [ viewStatus model.job.status
                    , if isRunning model.job then
                        button [ class "btn btn-default btn-xs", style [("float","left")], onClick CancelJob ] [ text "Cancel" ]
                      else
                        text ""
                    ]
                , td [] []
                ]
            ]
        , if isFailed model.job then
            div [ class "alert alert-danger" ] [ text model.job.lastStatusMessage ]
          else
            text ""
        ]


isRunning : Agave.Job -> Bool
isRunning job =
    job.status /= "FINISHED" && job.status /= "FAILED" && job.status /= "STOPPED"


isFailed : Agave.Job -> Bool
isFailed job =
    job.status == "FAILED" || job.status == "STOPPED"


viewStatus : String -> Html msg
viewStatus status =
    let
        progressBar pct =
            let
                label =
                    String.Extra.replace "_" " " status -- replace _ with space
            in
            div [ class "progress", style [("float","left"), ("width","20em")] ]
                [ div [ class "progress-bar progress-bar-striped active", style [("width", ((toString pct) ++ "%"))],
                        attribute "role" "progressbar", attribute "aria-valuenow" (toString pct), attribute "aria-valuemin" "0", attribute "aria-valuemax" "100" ]
                    [ text label ]
                ]
    in
    case String.toUpper status of
        "CREATED" -> progressBar 10
        "PENDING" -> progressBar 20
        "PROCESSING_INPUTS" -> progressBar 30
        "STAGING_INPUTS" -> progressBar 40
        "STAGED" -> progressBar 45
        "SUBMITTING" -> progressBar 50
        "STAGING_JOB" -> progressBar 55
        "QUEUED" -> progressBar 60
        "BLOCKED" -> progressBar 60
        "RUNNING" -> progressBar 70
        "CLEANING_UP" -> progressBar 80
        "ARCHIVING" -> progressBar 90
        "ARCHIVING_FINISHED" -> progressBar 95
        _ -> text status


viewInputs : Dict String Agave.JobInputValue -> Html msg
viewInputs inputs =
    let
        count =
            Dict.size inputs

        body =
            case count of
                0 ->
                    [ tr [] [ td [] [ text "None" ] ] ]

                _ ->
                    Dict.toList inputs |> List.map viewInput
    in
    div []
        [ h2 [] [ text "Inputs" ]
        , table [ class "table" ]
            [ colgroup []
                [ col [ class "col-md-3" ] [] ]
            , tbody [] body
            ]
        ]


viewInput : (String, Agave.JobInputValue) -> Html msg
viewInput (id, val) =
    let
        valList =
            case val of
                Agave.JobInputString s ->
                    [ s ]

                Agave.JobInputArray a ->
                    a
    in
    tr []
        [ th [] [ text id ]
        , td [] [ text (String.join "; " valList) ]
        ]


viewParameters : Dict String Agave.ValueType -> Html msg
viewParameters params =
    let
        count =
            Dict.size params

        body =
            case count of
                0 ->
                    [ tr [] [ td [] [ text "None" ] ] ]

                _ ->
                    Dict.toList params |> List.map viewParameter
    in
    div []
        [ h2 [] [ text "Parameters" ]
        , table [ class "table" ]
            [ colgroup []
                [ col [ class "col-md-3" ] [] ]
            , tbody [] body
            ]
        ]


viewParameter : (String, Agave.ValueType) -> Html msg
viewParameter (id, value) =
    let
        valueStr =
            case value of --TODO move to separate function
                Agave.StringValue s ->
                    s

                Agave.BoolValue b ->
                    if b then
                        "True"
                    else
                        "False"

                Agave.NumberValue n ->
                    toString n

                Agave.ArrayValue l ->
                    String.join ";" l
    in
    tr []
        [ th [] [ text id ]
        , td [] [ text valueStr ]
        ]


viewSettings : Agave.Job -> Html Msg
viewSettings job =
    div []
        [ h2 [] [ text "Settings" ]
        , table [ class "table" ]
            [ colgroup []
                [ col [ class "col-md-3" ] [] ]
            , tbody []
                [ tr []
                    [ th [ class "" ] [ text "Queue" ]
                    , td [] [ text job.remoteQueue ]
                    ]
                , tr []
                    [ th [ class "" ] [ text "Time limit" ]
                    , td [] [ text (toString job.maxHours) ]
                    ]
                ]
            ]
        ]


viewHistory : Model -> Html Msg
viewHistory model =
    let
        body =
            case model.history of
                [] ->
                    if model.loadedHistory then
                        [ tr [] [ td [] [ text "None" ] ] ]
                    else if model.loadingHistory then
                        [ tr [] [ td [] [ spinner ] ] ]
                    else
                        [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetHistory ] [ text "Show History" ] ] ] ]

                _ -> (List.map viewEvent model.history)
    in
    div []
        [ h2 [] [ text "History" ]
        , table [ class "table" ]
            [ tbody [] body
            ]
        ]


viewEvent : Agave.JobHistory -> Html msg
viewEvent event =
    tr []
        [ td [ class "nowrap" ] [ text event.created ]
        , td [ class "nowrap" ] [ text event.status ]
        , td [] [ text event.description ]
        ]


viewOutputs : Model -> Html Msg
viewOutputs model =
    let
        body =
            case model.job.status of
                "FINISHED" ->
                    case model.fileBrowser of
                        Nothing ->
                            button [ class "btn btn-default", onClick ShowOutputs ] [ text "Show Outputs" ]

                        Just fileBrowser ->
                            div []
                                [ div [ class "left", style [ ("display", "inline-block"), ("width", "70%"), ("min-height", "20em") ] ]
                                    [ FileBrowser.view fileBrowser |> Html.map FileBrowserMsg ]
                                , div [ class "right", style [ ("display", "inline-block"), ("max-width", "20em") ] ]
                                    [ br [] []
                                    , case FileBrowser.getSelected fileBrowser of
                                        [] ->
                                            p []
                                                [ text "Here are the output files from the job."
                                                , br [] []
                                                , br [] []
                                                , text "Click to select a file or directory."
                                                , br [] []
                                                , text "Double-click to open a file or directory."
                                                ]

                                        file :: _ ->
                                            if file.name == ".. (previous)" then -- FIXME
                                                text ""
                                            else
                                                viewFileInfo model.token file
                                    ]
                                ]

                "FAILED" ->
                    text "None"

                _ ->
                    text "Job is not FINISHED, please wait ..."

        de_url =
            "https://de.cyverse.org/de/?type=data&folder=/iplant/home/" ++ model.username ++ "/archive/jobs/job-" ++ model.job_id --FIXME move base url to config
    in
    div []
        [ h2 [] [ text "Outputs" ]
        , div []
            [ text "Browse and view output files in the "
            , a [ target "_blank", href de_url ] [ text "CyVerse Data Store" ]
            , text "."
            ]
        , table [ class "table" ]
            [ tbody [] [ tr [] [ td [] [ body ] ] ]
            ]
        ]


--TODO move into FileBrowser.elm, dup'ed in Dashboard.elm
viewFileInfo : String -> FileResult -> Html Msg
viewFileInfo token file =
    let
        myLocale =
            { usLocale | decimals = 0 }

        deleteText =
            "Are you sure you want to remove the " ++
                (if file.type_ == "dir" then
                    "directory"
                 else
                    file.type_
                ) ++
                " '" ++ file.name ++ "'?"

        deleteMsg =
            FileBrowserMsg (FileBrowser.OpenConfirmationDialog deleteText (FileBrowser.DeletePath file.path))

        deUrl =
            "https://de.cyverse.org/de/?type=data&folder=/iplant/home" ++ --TODO move base url to config file
                (if file.type_ == "dir" then
                     file.path
                else
                    dropFileName file.path)
    in
    div [ class "table-responsive" ]
        [ table [ class "table info-table" ]
            [ tbody []
                [ colgroup []
                    [ col [ class "col-md-1" ] [] ]
                , tr []
                    [ th [] [ text "Name " ]
                    , td [] [ text file.name ]
                    ]
                , tr []
                    [ th [] [ text "Type " ]
                    , td [] [ text file.type_ ]
                    ]
                , tr []
                    [ th [] [ text "Size " ]
                    , td [] [ text ((file.length |> toFloat |> format myLocale) ++ " bytes") ]
                    ]
                , tr []
                    [ th [] [ text "Last modified " ]
                    , td [] [ text file.lastModified ]
                    ]
--                , tr []
--                    [ td [] [ button [ class "btn btn-link btn-xs" ]
--                        [ span [ class "glyphicon glyphicon-plus" ] [], text " Add to Sample" ] ]
--                    ]
                , (if file.type_ == "file" then
                    tr []
                        [ td []
                            [ a [ type_ "button", class "btn btn-link btn-xs", href (apiBaseUrl ++ "/download" ++ file.path ++ "?token=" ++ token) ]
                                [ span [ class "glyphicon glyphicon-cloud-download" ] [], text " Download" ]
                            ]
                        ]
                  else
                    tr [] [])
                , tr []
                    [ td []
                        [ button [ class "btn btn-link btn-xs", onClick (FileBrowserMsg (FileBrowser.OpenShareDialog file.path)) ]
                            [ span [ class "glyphicon glyphicon-user" ] [], text " Share" ]
                        ]
                    ]
                , tr []
                    [ td []
                        [ a [ type_ "button", class "btn btn-link btn-xs", href deUrl, target "_blank" ]
                            [ span [ class "glyphicon glyphicon-share-alt" ] [], text " View in CyVerse DE" ]
                        ]
                    ]
                , tr []
                    [ td []
                        [ button [ class "btn btn-link btn-xs", onClick deleteMsg ]
                            [ span [ class "glyphicon glyphicon-trash" ] [], text " Delete" ]
                        ]
                    ]
                ]
            ]
        ]


viewResults : Model -> Html Msg
viewResults model =
    let
        body =
            case model.job.status of
                "FINISHED" ->
                    case model.loadedResults of
                        True ->
                            case model.results of
                                Nothing ->
                                    text "None"

                                _ ->
                                    div [] []

                        False ->
                            case model.loadingResults of
                                True ->
                                    spinner

                                False ->
                                    button [ class "btn btn-default", onClick GetResults ] [ text "Show Results" ]

                "FAILED" ->
                    tr [] [ td [] [ text "None" ] ]

                _ ->
                    div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ]
    in
    div []
        [ h2 [] [ text "Results" ]
        , table [ class "table" ]
            [ tbody []
                [ tr []
                    [ td []
                        [ div [] [ body ]
                        , div [ id "sim-plot" ] [] -- has to be located here for accessibility from heatmap.js
                        ]
                    ]
                ]
            ]
        ]


cancelDialogConfig : Model -> Dialog.Config Msg
cancelDialogConfig model =
    let
        content =
            case model.cancelDialogMessage of
                Nothing ->
                    spinner

                Just message ->
                    div [ class "alert alert-info" ]
                        [ p [] [ text message ]
                        ]

        footer =
            if model.cancelDialogMessage == Nothing then
                div [] [ text " " ]
            else
                button [ class "btn btn-default", onClick CloseCancelDialog ] [ text "OK" ]
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Cancel Job" ])
    , body = Just content
    , footer = Just footer
    }