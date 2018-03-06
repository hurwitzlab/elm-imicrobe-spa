module Page.Job exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave
import Data.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
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
    , outputs : List Agave.JobOutput
    , app : App.App
    , loadingResults : Bool
    , loadedResults : Bool
    , results : Maybe (List (String, String))
    , startTime : Maybe Time
    , lastPollTime : Maybe Time
    }


init : Session -> String -> Task PageLoadError Model
init session id =
    let
        loadJobFromAgave =
            Request.Agave.getJob session.token id |> Http.toTask |> Task.map .result

        loadJobFromPlanB =
            Request.PlanB.getJob session.token id |> Http.toTask |> Task.map .result

        loadJob =
            case String.startsWith "planb" id of
                True -> loadJobFromPlanB

                False -> loadJobFromAgave

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
                                , outputs = []
                                , app = app
                                , loadingResults = False
                                , loadedResults = False
                                , results = Nothing
                                , startTime = Nothing
                                , lastPollTime = Nothing
                                }
                        )
                )
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = GetOutputs
    | SetOutputs (List Agave.JobOutput)
    | GetResults
    | SetResults (Result Http.Error (List (String, String)))
    | SetJob Agave.Job
    | PollJob Time


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        username =
            case session.profile of
                Nothing -> ""

                Just profile -> profile.username
    in
    case msg of
        GetOutputs ->
            let
                loadOutputs =
                    Request.Agave.getJobOutputs username session.token model.job_id Nothing |> Http.toTask |> Task.map .result

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
                    Request.Agave.getJobOutputs username session.token model.job_id (Just path)
                        |> Http.toTask
                        |> Task.map .result
                        |> Task.map (List.filter (\r -> r.name /= ".") >> List.map .path) -- filter out current path "."

                -- Expects relative path
                loadOutput path =
                    Request.Agave.getJobOutput username session.token model.job_id path
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
            if model.loadingJob == False && model.job.status /= "FINISHED" && model.job.status /= "FAILED" && model.job.status /= "STOPPED" then
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

                    loadJobFromAgave =
                        Request.Agave.getJob session.token model.job.id |> Http.toTask |> Task.map .result

                    loadJobFromPlanB =
                        Request.PlanB.getJob session.token model.job.id |> Http.toTask |> Task.map .result

                    loadJob =
                        case String.startsWith "planb" model.job_id of
                            True -> loadJobFromPlanB

                            False -> loadJobFromAgave

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
            , viewOutputs model
            , viewResults model
            ]
        ]


viewJob : Model -> Html msg
viewJob model =
    table [ class "table" ]
        [ colgroup []
            [ col [ class "col-md-1" ] []
            , col [ class "col-md-3" ] []
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
            [ th [] [ text "Start Time" ]
            , td [] [ text model.job.startTime ]
            ]
        , tr []
            [ th [] [ text "End Time" ]
            , td [] [ text model.job.endTime ]
            ]
        , tr []
            [ th [ class "top" ] [ text "Status" ]
            , td [] [ viewStatus model.job.status ]
            , td [] []
            ]
        ]


viewStatus : String -> Html msg
viewStatus status =
    let
        progressBar pct =
            let
                label = String.Extra.replace "_" " " status -- replace _ with space
            in
            div [ class "progress" ]
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
        "STAGING_JOB" -> progressBar 45
        "STAGED" -> progressBar 50
        "QUEUED" -> progressBar 55
        "SUBMITTING" -> progressBar 60
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
    let
        display =
            "foo"
    in
    tr []
        [ th [] [ text id ]
        , td [] [ text value ]
        ]


viewOutputs : Model -> Html Msg
viewOutputs model =
    let
        body =
            case model.job.status of
                "FINISHED" ->
                    case model.outputs of
                        [] ->
                            case model.loadingOutputs of
                                False ->[ tr [] [ td [] [ button [ class "btn btn-default", onClick GetOutputs ] [ text "Show Outputs" ] ] ] ]

                                True -> [ tr [] [ td [] [ spinner ] ] ]

                        _ -> (List.map viewOutput model.outputs)

                "FAILED" ->
                    [ tr [] [ td [] [ text "None" ] ] ]

                _ ->
                    [ tr [] [ td [] [ div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ] ] ] ]

        de_url =
            "https://de.cyverse.org/de/?type=data&folder=/iplant/home/" ++ model.username ++ "/archive/jobs/job-" ++ model.job_id
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
