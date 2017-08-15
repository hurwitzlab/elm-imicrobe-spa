module Page.Projects exposing (Model, Msg, init, update, view)

import Data.Project
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Encode as Encode
import List exposing (map)
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Project
import Route
import String exposing (join)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , projects : List Data.Project.Project
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Projects"

        loadProjects =
            Request.Project.list |> Http.toTask

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
    Task.map4 Model title loadProjects tblState qry
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


config : Table.Config Data.Project.Project Msg
config =
    Table.customConfig
        { toId = .project_name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , domainColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


domainColumn : Table.Column Data.Project.Project Msg
domainColumn =
    Table.customColumn
        { name = "Domains"
        , viewData = domainsToString << .domains
        , sorter = Table.increasingOrDecreasingBy (domainsToString << .domains)
        }


domainsToString : List Data.Project.Domain -> String
domainsToString domains =
    join ", " (List.map .domain_name domains)


nameColumn : Table.Column Data.Project.Project Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.unsortable
        }


nameLink : Data.Project.Project -> Table.HtmlDetails Msg
nameLink project =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project project.project_id) ]
            [ text project.project_name ]
        ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        acceptableProjects =
            List.filter (String.contains lowerQuery << String.toLower << .project_name) model.projects
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , small []
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptableProjects
            ]
        ]