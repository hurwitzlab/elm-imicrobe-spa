module Page.Pubchase exposing (Model, Msg, init, update, view)

import Data.Pubchase as Pubchase
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Pubchase
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing (truncate)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , articles : List Pubchase.Article
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        title =
            Task.succeed "Recommended Reading"

        loadArticles =
            Request.Pubchase.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Title")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadArticles tblState qry
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


config : Table.Config Pubchase.Article Msg
config =
    Table.customConfig
        { toId = toString << .pubchase_id
        , toMsg = SetTableState
        , columns =
            [ titleColumn
            , authorColumn
            , journalColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


titleColumn : Table.Column Pubchase.Article Msg
titleColumn =
    Table.veryCustomColumn
        { name = "Title"
        , viewData = titleLink
        , sorter = Table.increasingOrDecreasingBy .title
        }


titleLink : Pubchase.Article -> Table.HtmlDetails Msg
titleLink article =
    Table.HtmlDetails []
        [ a [ href ("http://www.pubchase.com/article/" ++ toString article.article_id), target "_blank" ] [ text article.title ]
        ]


authorColumn : Table.Column Pubchase.Article Msg
authorColumn =
    Table.veryCustomColumn
        { name = "Authors"
        , viewData = authorLink
        , sorter = Table.increasingOrDecreasingBy .authors
        }


authorLink : Pubchase.Article -> Table.HtmlDetails msg
authorLink article =
    Table.HtmlDetails [] [ text <| Util.truncate article.authors ]


journalColumn : Table.Column Pubchase.Article Msg
journalColumn =
    Table.veryCustomColumn
        { name = "Journal"
        , viewData = journalText
        , sorter = Table.increasingOrDecreasingBy .journal_title
        }


journalText : Pubchase.Article -> Table.HtmlDetails msg
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

        acceptableArticles =
            List.filter
                (\article -> String.contains lowerQuery (catter article))
                model.articles

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableArticles

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
            , Table.view config model.tableState acceptableArticles
            ]
        ]
