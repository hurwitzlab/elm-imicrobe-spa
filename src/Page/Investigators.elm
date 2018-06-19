module Page.Investigators exposing (Model, Msg, init, update, view)

import Data.Investigator
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Investigator
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Widgets



---- MODEL ----


type alias Model =
    { pageTitle : String
    , investigators : List Data.Investigator.Investigator
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Investigators"

        loadInvestigators =
            Request.Investigator.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadInvestigators tblState qry
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


config : Table.Config Data.Investigator.Investigator Msg
config =
    Table.customConfig
        { toId = toString << .investigator_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , Table.stringColumn "Inst" .institution
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


nameColumn : Table.Column Data.Investigator.Investigator Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .investigator_name
        }


nameLink : Data.Investigator.Investigator -> Table.HtmlDetails Msg
nameLink inv =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Investigator inv.investigator_id) ]
            [ text inv.investigator_name ]
        ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .investigator_name) model.investigators
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , View.Widgets.counter (List.length acceptablePeople)
                , small [ class "right" ]
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptablePeople
            ]
        ]