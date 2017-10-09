module Page.Apps exposing (Model, Msg, init, update, view)

import Data.App
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.App
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , apps : List Data.App.App
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Apps"

        loadApps =
            Request.App.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadApps tblState qry
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


config : Table.Config Data.App.App Msg
config =
    Table.customConfig
        { toId = toString << .app_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , tagColumn
            , dataTypeColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


nameColumn : Table.Column Data.App.App Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .app_name
        }


nameLink : Data.App.App -> Table.HtmlDetails Msg
nameLink app =
    Table.HtmlDetails []
        [ a [ Route.href (Route.App app.app_id) ]
            [ text app.app_name ]
        ]


tagColumn : Table.Column Data.App.App Msg
tagColumn =
    Table.veryCustomColumn
        { name = "Tags"
        , viewData = tagBadges
        , sorter = Table.unsortable
        }


tagBadges : Data.App.App -> Table.HtmlDetails Msg
tagBadges app =
    Table.HtmlDetails []
        (List.map tagBadge app.app_tags)


tagBadge : Data.App.AppTag -> Html Msg
tagBadge tag =
    span [ class "badge margin-right" ] [ text tag.value ]


dataTypeColumn : Table.Column Data.App.App Msg
dataTypeColumn =
    Table.customColumn
        { name = "Data Types"
        , viewData = dataTypesToString << .app_data_types
        , sorter = Table.increasingOrDecreasingBy (dataTypesToString << .app_data_types)
        }


dataTypesToString : List Data.App.AppDataType -> String
dataTypesToString types =
    String.join ", " (List.map .name types)



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        appFilter app =
            ( (String.contains lowerQuery (app.app_name |> String.toLower))
                || (String.contains lowerQuery (app.app_tags |> List.map .value |> String.join ", " |> String.toLower))
                || (String.contains lowerQuery (app.app_data_types |> List.map .name |> String.join ", " |> String.toLower)) )

        acceptableApps =
            List.filter appFilter model.apps

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableApps

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , numShowing
                , small [ class "right" ]
                    [ input [ placeholder "Search", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptableApps
            ]
        ]
