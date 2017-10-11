module Page.Search exposing (Model, Msg, init, update, view)

import Data.Search
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import List.Extra
import Page.Error as Error exposing (PageLoadError)
import RemoteData exposing (..)
import Request.Search
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , query : String
    , tableState : Table.State
    , searchResults : WebData (List Data.Search.SearchResult)
    , searchResultTypes : List String
    , searchRestrictions : List String
    }


initialModel : Model
initialModel =
    { pageTitle = "General Search"
    , query = ""
    , tableState = Table.initialSort "Name"
    , searchResults = NotAsked
    , searchResultTypes = []
    , searchRestrictions = []
    }


init : Task PageLoadError Model
init =
    Task.succeed initialModel



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State
    | DoSearch
    | UpdateSearchResults (WebData (List Data.Search.SearchResult))
    | SelectOption String Bool


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

        UpdateSearchResults response ->
            let
                types =
                    case response of
                        RemoteData.Success data ->
                            List.map .table_name data
                                |> List.sort
                                |> List.Extra.unique

                        _ ->
                            []
            in
            { model | searchResults = response, searchResultTypes = types } => Cmd.none


doSearch : Model -> Cmd Msg
doSearch model =
    Request.Search.get model.query
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 [] [ text model.pageTitle ]
                ]
            , div [ class "center form-inline" ]
                    [ Html.form
                        [ onSubmit DoSearch ]
                        [ input
                            [ placeholder "Enter search term", class "form-control margin-right", size 50, onInput SetQuery ]
                            []
                        , button
                            [ onClick DoSearch, class "btn btn-primary" ]
                            [ text "Search" ]
                        ]
                    ]
            , div [] [ resultsTable model ]
            ]
        ]


mkCheckbox : String -> Html Msg
mkCheckbox val =
    label [ style [("padding-left", "1em")]]
        [ input [ type_ "checkbox", onCheck (SelectOption val) ] []
        , text (" " ++ val)
        ]


resultsTable : Model -> Html Msg
resultsTable model =
    case model.searchResults of
        RemoteData.Failure err ->
            text ("Error: " ++ toString err)

        RemoteData.Success data ->
            let
                filtered =
                    case List.length model.searchRestrictions of
                        0 ->
                            data

                        _ ->
                            List.filter
                                (\v ->
                                    List.member v.table_name
                                        model.searchRestrictions
                                )
                                data

                numShowing =
                    let
                        myLocale =
                            { usLocale | decimals = 0 }

                        count =
                            List.length filtered

                        numStr =
                            count |> toFloat |> format myLocale
                    in
                    case count of
                        0 ->
                            span [] []

                        _ ->
                            span [ class "badge" ] [ text numStr ]


                restrict =
                    case List.length model.searchResultTypes of
                        0 ->
                            text ""

                        _ ->
                            fieldset []
                                (text "Types: "
                                    :: List.map mkCheckbox model.searchResultTypes
                                )
            in
            if List.length filtered > 0 then
                div []
                    [ h2 []
                        [ text "Results "
                        , numShowing
                        ]
                    , div [ class "panel panel-default" ]
                        [ div [ class "panel-body" ]
                            [ restrict
                            ]
                        ]
                    , Table.view config model.tableState filtered
                    ]
            else
                text ""

        RemoteData.Loading ->
            text "Loading ..."

        RemoteData.NotAsked ->
            text ""


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

                "publication" ->
                    Route.Publication result.id

                "sample" ->
                    Route.Sample result.id

                _ ->
                    Route.Home
    in
    Table.HtmlDetails []
        [ a [ Route.href route ] [ text (Route.routeToString route) ] ]
