module Page.Jobs exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave exposing (Job)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Request.PlanB
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Widgets exposing (counter)
import View.Spinner exposing (spinner)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , jobs : List Job
    , pageLoaded : Bool
    , jobsLoaded : Bool
    , tableState : Table.State
    , query : String
    }


init : Session -> Task PageLoadError Model
init session =
--    let
--        loadJobsFromAgave =
--            Request.Agave.getJobs session.token |> Http.toTask |> Task.map .result
--
--        loadJobsFromPlanB =
--            Request.PlanB.getJobs session.token |> Http.toTask |> Task.map .result
--
--        loadAllJobs =
--            Task.sequence [ loadJobsFromAgave, loadJobsFromPlanB ] |> Task.map List.concat
--    in
--    loadAllJobs
--        |> Task.andThen
--            (\jobs ->
                Task.succeed
                    { pageTitle = "Jobs"
                    , jobs = []
                    , pageLoaded = False
                    , jobsLoaded = False
                    , tableState = Table.initialSort "Start"
                    , query = ""
                    }
--            )
--            |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State
    | DelayedInit
    | LoadJobsCompleted (Result Http.Error (List Job))


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none

        DelayedInit ->
            let
                loadJobsFromAgave =
                    Request.Agave.getJobs session.token |> Http.toTask |> Task.map .result

                loadJobsFromPlanB =
                    Request.PlanB.getJobs session.token |> Http.toTask |> Task.map .result

                loadAllJobs =
                    Task.sequence [ loadJobsFromAgave, loadJobsFromPlanB ] |> Task.map List.concat

                cmd =
                    if model.pageLoaded then
                        Cmd.none
                    else
                        Task.attempt LoadJobsCompleted loadAllJobs
            in
            { model | pageLoaded = True } => cmd

        LoadJobsCompleted (Ok jobs) ->
            { model | jobs = jobs, jobsLoaded = True } => Cmd.none

        LoadJobsCompleted (Err error) ->
            model => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        jobFilter job =
            ( (String.contains lowerQuery (String.toLower job.name))
                || (String.contains lowerQuery (String.toLower job.app_id))
                || (String.contains lowerQuery (String.toLower job.status)) )

        acceptableJobs =
            List.filter jobFilter model.jobs
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , counter (List.length acceptableJobs)
                , small [ class "right" ]
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , if model.jobsLoaded then
                Table.view config model.tableState acceptableJobs
              else
                spinner
            ]
        ]


config : Table.Config Job Msg
config =
    Table.customConfig
        { toId = .app_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , appColumn
            , startColumn
            , endColumn
            , Table.stringColumn "Status" .status
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


nameColumn : Table.Column Job Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .name
        }


nameLink : Job -> Table.HtmlDetails Msg
nameLink job =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Job job.id) ] [ text job.name ]
        ]


appColumn : Table.Column Job Msg
appColumn =
    Table.veryCustomColumn
        { name = "App"
        , viewData = appLink
        , sorter = Table.increasingOrDecreasingBy .app_id
        }


appLink : Job -> Table.HtmlDetails Msg
appLink job =
    Table.HtmlDetails []
        [ text job.app_id
        ]


startColumn : Table.Column Job Msg
startColumn =
    Table.customColumn
        { name = "Start"
        , viewData = .startTime
        , sorter = Table.decreasingOrIncreasingBy .startTime
        }


endColumn : Table.Column Job Msg
endColumn =
    Table.customColumn
        { name = "End"
        , viewData = .endTime
        , sorter = Table.decreasingOrIncreasingBy .endTime
        }
