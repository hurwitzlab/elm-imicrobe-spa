module Page.Search exposing (Model, Msg, init, update, view)

import Data.Search
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Search
import Route
import Table
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , query : String
    , searchResults : List Data.Search.SearchResult
    , tableState : Table.State
    }


init : String -> Task PageLoadError Model
init query =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Search Results"

        loadSearchResults =
            Request.Search.get query |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        handleLoadError err =
            -- If a resource task fail load error page
            let
                errMsg =
                    case err of
                        Http.BadStatus response ->
                            case String.length response.body of
                                0 ->
                                    "Bad status"

                                _ ->
                                    response.body

                        _ ->
                            toString err
            in
            Error.pageLoadError Page.Home errMsg
    in
    Task.map4 Model title (Task.succeed query) loadSearchResults tblState
        |> Task.mapError handleLoadError



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



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , Table.view config model.tableState model.searchResults
            ]
        ]


config : Table.Config Data.Search.SearchResult Msg
config =
    Table.config
        { toId = toString << .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Type" .table_name
            , nameColumn
            ]
        }


nameColumn : Table.Column Data.Search.SearchResult Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .name
        }


nameLink : Data.Search.SearchResult -> Table.HtmlDetails Msg
nameLink result =
    let
        route =
            case result.table_name of
                "investigator" ->
                    Route.Investigator result.id

                "project" ->
                    Route.Project result.id

                "sample" ->
                    Route.Sample result.id

                _ ->
                    Route.Home
    in
    Table.HtmlDetails []
        [ a [ Route.href route ] [ text result.name ] ]
