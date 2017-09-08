module Page.Jobs exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave exposing (Job)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Page as Page



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

        loadJobs =
            Request.Agave.getJobs session.token |> Http.toTask |> Task.map .result

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadJobs tblState qry
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
            , Table.stringColumn "Start" .startTime
            , Table.stringColumn "End" .endTime
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



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        acceptableJobs =
            List.filter (String.contains lowerQuery << String.toLower << .name) model.jobs

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableJobs

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ] [ text numStr ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , numShowing
                , small [ class "right" ]
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptableJobs
            ]
        ]