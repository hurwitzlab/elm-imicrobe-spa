module Page.Publication exposing (Model, Msg, init, update, view)

import Data.Publication
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Publication
import Route
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , publication_id : Int
    , publication : Data.Publication.Publication
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Publication"

        loadPublication =
            Request.Publication.get id |> Http.toTask

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
    Task.map3 Model title (Task.succeed id) loadPublication
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , viewPublication model.publication
            ]
        ]


viewPublication : Data.Publication.Publication -> Html msg
viewPublication pub =
    let
        projectLink =
            case pub.project of
                Nothing ->
                    text "NA"

                Just project ->
                    a
                        [ Route.href (Route.Publication project.project_id) ]
                        [ text project.project_name ]

        pubMedLink =
            case pub.pubmed_id of
                0 ->
                    text "NA"

                _ ->
                    a
                        [ href <|
                            "https://www.ncbi.nlm.nih.gov/pubmed/?term="
                                ++ toString pub.pubmed_id
                        ]
                        [ text <| toString pub.pubmed_id ]
    in
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Title" ]
            , td [] [ text pub.title ]
            ]
        , tr []
            [ th [] [ text "Author" ]
            , td [] [ text pub.author ]
            ]
        , tr []
            [ th [] [ text "Published" ]
            , td [] [ text pub.pub_date ]
            ]
        , tr []
            [ th [] [ text "PubMed" ]
            , td [] [ pubMedLink ]
            ]
        , tr []
            [ th [] [ text "DOI" ]
            , td [] [ text pub.doi ]
            ]
        , tr []
            [ th [] [ text "Project" ]
            , td [] [ projectLink ]
            ]
        ]
