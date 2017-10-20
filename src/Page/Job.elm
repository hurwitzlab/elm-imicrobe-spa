module Page.Job exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Ports
import Task exposing (Task)
import Dict exposing (Dict)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , job_id : String
    , job : Agave.Job
    , loadingOutputs : Bool
    , outputs : List Agave.JobOutput
    , loadingResults : Bool
    , results : String
    }


init : Session -> String -> Task PageLoadError Model
init session id =
    let
        loadJob =
            Request.Agave.getJob session.token id |> Http.toTask |> Task.map .result
    in
    loadJob
        |> Task.andThen
            (\job ->
                Task.succeed
                    { pageTitle = "Job"
                    , job_id = job.id
                    , job = job
                    , loadingOutputs = False
                    , outputs = []
                    , loadingResults = False
                    , results = ""
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = GetOutputs
    | SetOutputs (List Agave.JobOutput)
    | GetResults
    | SetResults String


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        GetOutputs ->
            let
                loadOutputs =
                    Request.Agave.getJobOutputs session.token model.job_id |> Http.toTask |> Task.map .result

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
            { model | outputs = outputs } => Cmd.none

        GetResults ->
            let
                loadResults =
                    Request.Agave.getJobOutput session.token model.job_id "mash-out/sna/distance.tab" |> Http.toTask

                handleResults results =
                    case results of
                        Ok results ->
                            SetResults results

                        Err error ->
                            let
                                _ = Debug.log "Error" ("could not retrieve job results: " ++ (toString error))
                            in
                            SetResults ""
            in
            { model | loadingResults = True } => Task.attempt handleResults loadResults

        SetResults results ->
            { model | results = results } => Ports.createSimPlot ("sim-plot", results)


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
                    ]
                ]
            , viewJob model.job
            , viewInputs model.job.inputs
            , viewParameters model.job.parameters
            , viewOutputs model
            , viewResults model
            ]
        ]


viewJob : Agave.Job -> Html msg
viewJob job =
    table [ class "table" ]
        [ tr []
            [ th [] [ text "ID" ]
            , td [] [ text job.id ]
            ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text job.name ]
            ]
        , tr []
            [ th [] [ text "App" ]
            , td [] [ text job.app_id ]
            ]
        , tr []
            [ th [] [ text "Start Time" ]
            , td [] [ text job.startTime ]
            ]
        , tr []
            [ th [] [ text "End Time" ]
            , td [] [ text job.endTime ]
            ]
        , tr []
            [ th [] [ text "Status" ]
            , td [] [ text job.status ]
            ]
        ]


viewInputs : Dict String (List String) -> Html msg
viewInputs inputs =
    let
        count =
            Dict.size inputs

        body =
            case count of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        [ tbody [] (Dict.toList inputs |> List.map viewInput) ]
    in
    div []
        [ h2 [] [ text "Inputs" ]
        , body
        ]


viewInput : (String, List String) -> Html msg
viewInput (id, values) =
    tr []
        [ th [] [ text id ]
        , td [] [ text (String.join ", " values) ]
        ]


viewParameters : Dict String String -> Html msg
viewParameters params =
    let
        count =
            Dict.size params

        body =
            case count of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        [ tbody [] (Dict.toList params |> List.map viewParameter) ]
    in
    div []
        [ h2 [] [ text "Parameters" ]
        , body
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
                            let
                                element =
                                    case model.loadingOutputs of
                                        False -> button [ class "btn btn-default", onClick GetOutputs ] [ text "Show Outputs" ]

                                        True -> div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]
                            in
                            [ element
                            ]


                        _ -> [ table [ class "table" ] [ tbody [] (List.map viewOutput model.outputs) ] ]

                _ -> [ div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ] ]
    in
    div []
        [ h2 [] [ text "Outputs" ]
        , div [] body
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
                    case model.results of
                        "" ->
                            case model.loadingResults of
                                False -> button [ class "btn btn-default", onClick GetResults ] [ text "Show Results" ]

                                True -> div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]


                        _ -> div [] []

                _ -> div [ class "italic" ] [ text "Job is not FINISHED, please wait ..." ]
    in
    div []
        [ h2 [] [ text "Results" ]
        , div [] [ body ]
        , div [ id "sim-plot" ] [] -- has to be located here for accessibility from heatmap.js
        ]