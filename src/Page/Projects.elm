module Page.Projects exposing (Model, Msg, init, update, view)

import Data.Project
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Project
import Route
import Table
import Task exposing (Task)
import View.Page as Page
import String exposing (join)
import List exposing (map)


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
    Table.config
        { toId = .project_name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .project_name
            , domainColumn
            ]
        }

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
            [ h2 [] [ text model.pageTitle ]
            , input [ placeholder "Search by Name", onInput SetQuery ] []
            , Table.view config model.tableState acceptableProjects
            ]
        ]


viewProjects projects =
    case List.length projects of
        0 ->
            text "No projects"

        _ ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Domains" ]
                        ]
                    ]
                , tbody []
                    (List.map rowProject projects)
                ]


viewDomain domain =
    a [ href ("/domain" ++ domain.domain_name) ] [ text domain.domain_name ]


viewDomains domains =
    List.map viewDomain domains


rowProject project =
    let
        invs =
            project.investigators

        domains =
            List.intersperse (text ", ") (viewDomains project.domains)
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Project project.project_id) ]
                [ text project.project_name ]
            ]
        , td [] domains
        ]
