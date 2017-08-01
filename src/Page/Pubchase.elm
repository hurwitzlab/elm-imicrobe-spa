module Page.Pubchase exposing (Model, Msg, init, update, view)

import Data.Pubchase
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Pubchase
import Table
import Task exposing (Task)
import Util exposing (truncate)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , articles : List Data.Pubchase.Article
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Recommended Reading"

        loadArticles =
            Request.Pubchase.list |> Http.toTask

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
    Task.map4 Model title loadArticles tblState qry
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


config : Table.Config Data.Pubchase.Article Msg
config =
    Table.config
        { toId = toString << .pubchase_id
        , toMsg = SetTableState
        , columns =
            [ titleColumn
            , authorColumn
            , journalColumn
            ]
        }


titleColumn : Table.Column Data.Pubchase.Article Msg
titleColumn =
    Table.veryCustomColumn
        { name = "Title"
        , viewData = titleLink
        , sorter = Table.increasingOrDecreasingBy .title
        }


titleLink : Data.Pubchase.Article -> Table.HtmlDetails Msg
titleLink article =
    Table.HtmlDetails []
        [ a
            [ href <|
                "http://www.pubchase.com/article/"
                    ++ toString article.article_id
            ]
            [ text article.title ]
        ]


authorColumn : Table.Column Data.Pubchase.Article Msg
authorColumn =
    Table.veryCustomColumn
        { name = "Authors"
        , viewData = authorLink
        , sorter = Table.increasingOrDecreasingBy .authors
        }


authorLink : Data.Pubchase.Article -> Table.HtmlDetails msg
authorLink article =
    Table.HtmlDetails [] [ text <| Util.truncate article.authors ]


journalColumn : Table.Column Data.Pubchase.Article Msg
journalColumn =
    Table.veryCustomColumn
        { name = "Journal"
        , viewData = journalText
        , sorter = Table.increasingOrDecreasingBy .journal_title
        }


journalText : Data.Pubchase.Article -> Table.HtmlDetails msg
journalText article =
    Table.HtmlDetails [] [ text <| Util.truncate article.journal_title ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        catter article =
            String.concat
                (List.intersperse " "
                    [ article.title
                    , article.authors
                    , article.journal_title
                    ]
                )
                |> String.toLower

        acceptablePeople =
            List.filter
                (\article -> String.contains lowerQuery (catter article))
                model.articles
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , input [ placeholder "Search by Name", onInput SetQuery ] []
            , Table.view config model.tableState acceptablePeople
            ]
        ]
