module Page.Publications exposing (Model, Msg, init, update, view)

import Data.Publication
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Publication
import Route
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Widgets



---- MODEL ----


type alias Model =
    { pageTitle : String
    , publications : List Data.Publication.Publication
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Publications"

        loadPublications =
            Request.Publication.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Title")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadPublications tblState qry
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


config : Table.Config Data.Publication.Publication Msg
config =
    Table.customConfig
        { toId = toString << .publication_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , authorColumn
            , projectColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


authorColumn : Table.Column Data.Publication.Publication Msg
authorColumn =
    Table.veryCustomColumn
        { name = "Author(s)"
        , viewData = authorLink
        , sorter = Table.increasingOrDecreasingBy .author
        }


authorLink : Data.Publication.Publication -> Table.HtmlDetails msg
authorLink pub =
    Table.HtmlDetails [] [ text pub.author ]


nameColumn : Table.Column Data.Publication.Publication Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Title"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .title
        }


nameLink : Data.Publication.Publication -> Table.HtmlDetails Msg
nameLink publication =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Publication publication.publication_id) ]
            [ text publication.title ]
        ]


projectName : Data.Publication.Publication -> String
projectName pub =
    case pub.project of
        Nothing ->
            "NA"

        Just project ->
            project.project_name


projectColumn : Table.Column Data.Publication.Publication Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project(s)"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy projectName
        }


projectLink : Data.Publication.Publication -> Table.HtmlDetails Msg
projectLink pub =
    let
        link =
            case pub.project of
                Nothing ->
                    text "NA"

                Just project ->
                    a [ Route.href (Route.Project project.project_id) ]
                        [ text project.project_name ]
    in
    Table.HtmlDetails [] [ link ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        catter pub =
            String.concat
                (List.intersperse " "
                    [ pub.title
                    , pub.author
                    , projectName pub
                    ]
                )
                |> String.toLower

        acceptablePubs =
            List.filter
                (\pub -> String.contains lowerQuery (catter pub))
                model.publications
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , View.Widgets.counter (List.length acceptablePubs)
                , small [ class "right" ]
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptablePubs
            ]
        ]
