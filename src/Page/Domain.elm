module Page.Domain exposing (Model, Msg, init, update, view)

import Data.Domain
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Domain
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Widgets



---- MODEL ----


type alias Model =
    { pageTitle : String
    , domain_id : Int
    , domain : Data.Domain.Domain
    , tableState : Table.State
    , query : String
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Domain"

        loadDomain =
            Request.Domain.get id |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""
    in
    Task.map5 Model title (Task.succeed id) loadDomain tblState qry
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


config : Table.Config Data.Domain.Project Msg
config =
    Table.customConfig
        { toId = toString << .project_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


nameColumn : Table.Column Data.Domain.Project Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.unsortable
        }


nameLink : Data.Domain.Project -> Table.HtmlDetails Msg
nameLink project =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project project.project_id) ]
            [ text project.project_name ]
        ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        acceptable =
            List.filter
                (String.contains lowerQuery << String.toLower << .project_name)
                model.domain.projects
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.domain.domain_name ]
                    ]
                ]
            , viewProjects model.domain.projects
            ]
        ]


viewProjects : List Data.Domain.Project -> Html Msg
viewProjects projects =
    let
        searchInput =
            small [ style [ ("float", "right") ] ] [
                input [ placeholder "Search by Name", onInput SetQuery ] []
            ]

        body =
            if projects == [] then
                text "None"
            else
                table [ class "table table-condensed" ]
                    [ tbody [] (List.map viewProject projects) ]
    in
    div []
        [ h2 []
            [ text "Projects "
            , View.Widgets.counter (List.length projects)
            , text " "
            , searchInput
            ]
        , body
        ]


viewProject : Data.Domain.Project -> Html msg
viewProject project =
    tr []
        [ td []
            [ a [ Route.href (Route.Project project.project_id) ]
                [ text project.project_name ]
            ]
        ]
