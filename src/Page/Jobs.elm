module Page.Jobs exposing (Model, Msg, init, update, view)

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
import View.Widgets



---- MODEL ----


type alias Model =
    { pageTitle : String
    , jobs : List Job
    , tableState : Table.State
    , query : String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Jobs"

        loadJobsFromAgave =
            Request.Agave.getJobs session.token |> Http.toTask |> Task.map .result

        loadJobsFromPlanB =
            Request.PlanB.getJobs session.token |> Http.toTask |> Task.map .result

        loadAllJobs =
            Task.sequence [ loadJobsFromAgave, loadJobsFromPlanB ] |> Task.map List.concat

        tblState =
            Task.succeed (Table.initialSort "Start")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadAllJobs tblState qry
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


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
                , View.Widgets.counter (List.length acceptableJobs)
                , small [ class "right" ]
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptableJobs
            ]
        ]