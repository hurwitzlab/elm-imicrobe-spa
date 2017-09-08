module Page.Search exposing (Model, Msg, init, update, view)

import Data.Search
import Data.Session as Session exposing (Session)
import FormatNumber exposing (format)
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



---- MODEL ----


type alias Model =
    { pageTitle : String
    , query : String
    , tableState : Table.State
    , searchResults : WebData (List Data.Search.SearchResult)
    , searchResultsMessage : Html Msg
    , searchResultTypes : List String
    , searchRestrictions : List String
    }


initialModel : Model
initialModel =
    { pageTitle = "Search Results"
    , query = ""
    , tableState = Table.initialSort "Name"
    , searchResults = NotAsked
    , searchResultsMessage = text ""
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
                ( message, types ) =
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

                                msg =
                                    span []
                                        [ text
                                            ("Found "
                                                ++ format myLocale (toFloat numFound)
                                                ++ " for "
                                            )
                                        , em [] [ text model.query ]
                                        ]

                                types =
                                    List.map .table_name data
                                        |> List.sort
                                        |> List.Extra.unique
                            in
                            ( msg, types )

                        _ ->
                            ( text "", [] )
            in
            ( { model
                | searchResults = response
                , searchResultsMessage = message
                , searchResultTypes = types
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
        restrict =
            case List.length model.searchResultTypes of
                0 ->
                    text ""

                _ ->
                    fieldset []
                        (text "Show: "
                            :: List.map mkCheckbox model.searchResultTypes
                        )
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ style [ ( "text-align", "center" ) ] ]
                [ h2 [] [ text model.pageTitle ]
                , div [ class "form-inline" ]
                    [ Html.form
                        [ onSubmit DoSearch ]
                        [ input
                            [ placeholder "Search for", class "form-control", size 30, onInput SetQuery ]
                            []
                        , button
                            [ onClick DoSearch, class "btn btn-primary" ]
                            [ text "Search" ]
                        , br [] []
                        , model.searchResultsMessage
                        , restrict
                        ]
                    ]
                ]
            , div [] [ resultsTable model ]
            ]
        ]


mkCheckbox : String -> Html Msg
mkCheckbox val =
    label []
        [ input [ type_ "checkbox", onCheck (SelectOption val) ] []
        , text val
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
            in
            if List.length filtered > 0 then
                Table.view config model.tableState filtered
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
    [ attribute "class" "table"
    ]


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

        name =
            case result.name of
                "" ->
                    "NA"

                _ ->
                    result.name
    in
    Table.HtmlDetails []
        [ a [ Route.href route ] [ text name ] ]
