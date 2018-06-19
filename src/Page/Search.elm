module Page.Search exposing (Model, Msg, init, update, view)

import Data.Search
import List.Extra
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Page.Error as Error exposing (PageLoadError)
import Request.Search
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Widgets



---- MODEL ----


type alias Model =
    { pageTitle : String
    , query : String
    , tableState : Table.State
    , searchResults : List Data.Search.SearchResult --WebData (List Data.Search.SearchResult)
    , searchResultTypes : List String
    , searchRestrictions : List String
    }


init : String -> Task PageLoadError Model
init query =
    let
        doSearch =
            Request.Search.get query |> Http.toTask
--            Request.Search.get model.query
--                |> RemoteData.sendRequest
--                |> Cmd.map UpdateSearchResults
    in
    doSearch
        |> Task.andThen
            (\results ->
                Task.succeed
                    { pageTitle = "Search Results"
                    , query = query
                    , tableState = Table.initialSort "Name"
                    , searchResults = results
                    , searchResultTypes = []
                    , searchRestrictions = []
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetTableState Table.State
--    | DoSearch
    | SelectOption String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

--        DoSearch ->
--            case model.query of
--                "" ->
--                    ( model, Cmd.none )
--
--                _ ->
--                    ( model, doSearch model )

        SelectOption value bool ->
            let
                curOptions =
                    model.searchRestrictions

                newOpts =
                    case bool of
                        True ->
                            List.sort (value :: curOptions)

                        False ->
                            List.filter ((/=) value) curOptions
            in
            ( { model | searchRestrictions = newOpts }, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ resultsTable model ]
        ]


mkCheckbox : String -> Html Msg
mkCheckbox val =
    label [ style [("padding-left", "1em")]]
        [ input [ type_ "checkbox", onCheck (SelectOption val) ] []
        , text (" " ++ val)
        ]


resultsTable : Model -> Html Msg
resultsTable model =
    let
        filtered =
            if List.length model.searchRestrictions == 0 then
                model.searchResults
            else
                List.filter
                    (\v -> List.member v.table_name model.searchRestrictions)
                    model.searchResults

        types =
            List.map .table_name model.searchResults
                |> List.sort
                |> List.Extra.unique

        restrict =
            if List.length types == 0 then
                text ""
            else
                fieldset []
                    (text "Types: "
                        :: List.map mkCheckbox types
                    )
    in
    if List.length filtered > 0 then
        div []
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , View.Widgets.counter (List.length filtered)
                    ]
                ]
            , div [ class "panel panel-default" ]
                    [ div [ class "panel-body" ]
                        [ restrict
                        ]
                    ]
            , Table.view config model.tableState filtered
            ]
    else
        div []
            [ div [ class "page-header" ]
                [ h1 []
                    [ text model.pageTitle
                    ]
                ]
            , text "None"
            ]


config : Table.Config Data.Search.SearchResult Msg
config =
    Table.customConfig
        { toId = toString << .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Type" .table_name
            , nameColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed"
    ]


nameColumn : Table.Column Data.Search.SearchResult Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Link"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .table_name
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

                "project_group" ->
                    Route.ProjectGroup result.id

                "publication" ->
                    Route.Publication result.id

                "sample" ->
                    Route.Sample result.id

                _ ->
                    Route.Home

        name =
            if result.object_name == "" then
                Route.routeToString route
            else
                result.object_name
    in
    Table.HtmlDetails []
        [ a [ Route.href route ] [ text name ] ]
