module Page.Search exposing (Model, Msg, init, update, view)

import Data.Search
import FormatNumber exposing (format)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import RemoteData exposing (..)
import Request.Search
import Route
import Table
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , query : String
    , searchResults : WebData (List Data.Search.SearchResult)
    , searchResultsMessage : String
    , tableState : Table.State
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Search Results"

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
    Task.map5 Model title (Task.succeed "") (Task.succeed NotAsked) (Task.succeed "") tblState
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State
    | DoSearch
    | UpdateSearchResults (WebData (List Data.Search.SearchResult))


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

        DoSearch ->
            ( model, doSearch model )

        UpdateSearchResults response ->
            let
                message =
                    case response of
                        RemoteData.Success data ->
                            let
                                numFound =
                                    List.length data

                                myLocale =
                                    { decimals = 0
                                    , thousandSeparator = ","
                                    , decimalSeparator = "."
                                    }
                            in
                            "Found "
                                ++ format myLocale (toFloat numFound)
                                ++ " for "
                                ++ model.query

                        _ ->
                            ""
            in
            ( { model
                | searchResults = response
                , searchResultsMessage = message
              }
            , Cmd.none
            )


doSearch : Model -> Cmd Msg
doSearch model =
    Request.Search.get model.query
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults



-- VIEW --


view : Model -> Html Msg
view model =
    let
        curQuery =
            if String.length model.query > 0 then
                model.query
            else
                "Nada"
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text ("query = " ++ curQuery) ]
            , div []
                [ input [ onInput SetQuery ] []
                , button [ onClick DoSearch ] [ text "Search" ]
                ]
            , div [] (viewResults model)
            ]
        ]


viewResults : Model -> List (Html Msg)
viewResults model =
    case model.searchResults of
        RemoteData.Failure err ->
            [ text ("Error: " ++ toString err) ]

        RemoteData.Success data ->
            let
                table =
                    if List.length data > 0 then
                        Table.view config model.tableState data
                    else
                        text ""
            in
            [ text model.searchResultsMessage, table ]

        RemoteData.Loading ->
            [ text "Loading ..." ]

        RemoteData.NotAsked ->
            [ text "" ]


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
