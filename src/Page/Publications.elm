module Page.Publications exposing (Model, Msg, init, update, view)

import Data.Publication
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Publication
import Route
import Table
import Task exposing (Task)
import View.Page as Page


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
    Task.map4 Model title loadPublications tblState qry
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


config : Table.Config Data.Publication.Publication Msg
config =
    Table.config
        { toId = toString << .publication_id
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , authorColumn
            , projectColumn
            ]
        }


authorColumn : Table.Column Data.Publication.Publication Msg
authorColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = authorLink
        , sorter = Table.increasingOrDecreasingBy .author
        }


authorLink : Data.Publication.Publication -> Table.HtmlDetails msg
authorLink pub =
    Table.HtmlDetails [] [ text pub.author ]


nameColumn : Table.Column Data.Publication.Publication Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
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
        { name = "Projects"
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
        query =
            model.query

        lowerQuery =
            String.toLower query

        catter pub =
            String.concat
                (List.intersperse " "
                    [ pub.title
                    , pub.author
                    , projectName pub
                    ]
                )
                |> String.toLower

        acceptablePeople =
            List.filter
                (\pub -> String.contains lowerQuery (catter pub))
                model.publications
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
