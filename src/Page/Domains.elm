module Page.Domains exposing (Model, Msg, init, update, view)

import Data.Domain
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Domain
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , domains : List Data.Domain.Domain
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Domains"

        loadDomains =
            Request.Domain.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""

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
    Task.map4 Model title loadDomains tblState qry
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


config : Table.Config Data.Domain.Domain Msg
config =
    Table.customConfig
        { toId = toString << .domain_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , projectsColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


nameColumn : Table.Column Data.Domain.Domain Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.unsortable
        }


nameLink : Data.Domain.Domain -> Table.HtmlDetails Msg
nameLink domain =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Domain domain.domain_id) ]
            [ text domain.domain_name ]
        ]


projectsColumn : Table.Column Data.Domain.Domain Msg
projectsColumn =
    Table.veryCustomColumn
        { name = "Projects"
        , viewData = projectsText
        , sorter = Table.unsortable
        }


projectsText : Data.Domain.Domain -> Table.HtmlDetails Msg
projectsText domain =
    Table.HtmlDetails [] [ text (toString (List.length domain.projects)) ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .domain_name) model.domains
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , small []
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptablePeople
            ]
        ]
