module Page.Job exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave
import Data.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Request.PlanB
import Request.App
import Route
import Ports
import Task exposing (Task)
import Dict exposing (Dict)
import Util exposing ((=>))
import Time exposing (Time)
import String.Extra
import List.Extra
import View.Spinner exposing (spinner)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , username : String
    , job_id : String
    , job : Agave.Job
    , loadingJob : Bool
    , loadingOutputs : Bool
    , loadingHistory : Bool
    , loadedHistory : Bool
    , outputs : List Agave.JobOutput
    , history : List Agave.JobHistory
    , app : App.App
    , loadingResults : Bool
    , loadedResults : Bool
    , results : Maybe (List (String, String))
    , startTime : Maybe Time
    , lastPollTime : Maybe Time
    , showCancelDialog : Bool
    , cancelDialogMessage : Maybe String
    }


init : Session -> String -> Task PageLoadError Model
init session id =
    let
        loadJobFromAgave =
            Request.Agave.getJob session.token id |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            Request.PlanB.getJob session.token id |> Http.toTask |> Task.map .result

        loadJob =
            if String.startsWith "planb" id then
                loadJobFromPlanB
            else
                loadJobFromAgave

        loadApp app_name =
            Request.App.getByName app_name |> Http.toTask

        username =
            case session.profile of
                Nothing -> ""

                Just profile -> profile.username
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
                                , job_id = job.id
                                , job = job
                                , loadingJob = False
                                , loadingOutputs = False
                                , loadingHistory = False
                                , loadedHistory = False
                                , outputs = []
                                , history = []
                                , app = app
                                , loadingResults = False
                                , loadedResults = False
                                , results = Nothing
                                , startTime = Nothing
                                , lastPollTime = Nothing
                                , showCancelDialog = False
                                , cancelDialogMessage = Nothing
                                }
                        )
                )
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = GetHistory
    | SetHistory (List Agave.JobHistory)
    | GetOutputs
    | SetOutputs (List Agave.JobOutput)
    | GetResults
    | SetResults (Result Http.Error (List (String, String)))
    | SetJob Agave.Job
    | PollJob Time
    | CancelJob
    | CancelJobCompleted (Result Http.Error Agave.Job)
    | CloseCancelDialog


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        username =
            case session.profile of
                Nothing -> ""

                Just profile -> profile.username

        loadJobFromAgave =
            Request.Agave.getJob session.token model.job.id |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            Request.PlanB.getJob session.token model.job.id |> Http.toTask |> Task.map .result

        loadJob =
            if String.startsWith "planb" model.job_id then
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
                    if String.startsWith "planb" model.job_id then
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

        GetOutputs ->
            let
                loadOutputs =
                    Request.Agave.getJobOutputs model.job.owner session.token model.job_id Nothing |> Http.toTask |> Task.map .result

                handleOutputs outputs =
                    case outputs of
                        Ok outputs ->
                            SetOutputs outputs

                        Err _ ->
                            let
                                _ = Debug.log "Error" "could not retrieve job outputs"
                            in
                            SetOutputs []
            in
            { model | loadingOutputs = True } => Task.attempt handleOutputs loadOutputs

        SetOutputs outputs ->
            let
                filtered =
                    List.filter (\output -> output.name /= ".") outputs
            in
            { model | outputs = filtered } => Cmd.none

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
                    case String.endsWith "/" path of
                        False ->
                            loadOutput path

                        True ->
                            -- Get contents of every file in the path
                            loadOutputs path
                                |> Task.andThen
                                    (\outputs -> outputs |> List.map loadFile |> Task.sequence |> Task.map List.concat)

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
                case doPoll of
                    True ->
                        { model | loadingJob = True, startTime = Just startTime, lastPollTime = Just time } => Task.attempt handleJob loadJob
                    False ->
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
            { model | cancelDialogMessage = Just (toString error) }  => Cmd.none

        CloseCancelDialog ->
            { model | showCancelDialog = False } => Cmd.none


-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.job.name ]
                    , small [ class "pull-right", style [("padding-top","0.6em")] ]
                        [ text ("Status: " ++ model.job.status) ]
                    ]
                ]
            , viewJob model
            , viewInputs model.job.inputs
            , viewParameters model.job.parameters
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
    table [ class "table" ]
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
            , td [] [ a [ Route.href (Route.App model.app.app_id) ] [ text model.job.app_id ] ]
            ]
        , tr []
            [ th [] [ text "Owner" ]
            , td [] [ text model.job.owner ]
            ]
        , tr []
            [ th [] [ text "Start Time" ]
            , td [] [ text model.job.startTime ]
            ]
        , tr []
            [ th [] [ text "End Time" ]
            , td [] [ text model.job.endTime ]
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


isRunning : Agave.Job -> Bool
isRunning job =
    job.status /= "FINISHED" && job.status /= "FAILED" && job.status /= "STOPPED"


viewStatus : String -> Html msg
viewStatus status =
    let
        progressBar pct =
            let
                label = String.Extra.replace "_" " " status -- replace _ with space
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
        "RUNNING" -> progressBar 70
        "CLEANING_UP" -> progressBar 80
        "ARCHIVING" -> progressBar 90
        "ARCHIVING_FINISHED" -> progressBar 95
        _ -> text status


viewInputs : Dict String (List String) -> Html msg
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


viewInput : (String, List String) -> Html msg
viewInput (id, values) =
    tr []
        [ th [] [ text id ]
        , td [] [ text (String.join "; " values) ]
        ]


viewParameters : Dict String String -> Html msg
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


viewParameter : (String, String) -> Html msg
viewParameter (id, value) =
    tr []
        [ th [] [ text id ]
        , td [] [ text value ]
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
                    case model.outputs of
                        [] ->
                            if model.loadingOutputs then
                                [ tr [] [ td [] [ spinner ] ] ]
                            else
                                [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetOutputs ] [ text "Show Outputs" ] ] ] ]

                        _ -> (List.map viewOutput model.outputs)

                "FAILED" ->
                    [ tr [] [ td [] [ text "None" ] ] ]

                _ ->
                    [ tr [] [ td [] [ div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ] ] ] ]

        de_url =
            "https://de.cyverse.org/de/?type=data&folder=/iplant/home/" ++ model.username ++ "/archive/jobs/job-" ++ model.job_id --FIXME move base url to config
    in
    div []
        [ h2 [] [ text "Outputs" ]
        , div [] [ text "Browse and view output files in the ", a [ target "_blank", href de_url ] [ text "CyVerse Data Store" ], text "." ]
        , table [ class "table" ]
            [ tbody [] body
            ]
        ]


viewOutput : Agave.JobOutput -> Html msg
viewOutput output =
    tr []
        [ td [] [ text output.name ]
        , td [] [ text output.type_ ]
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