module Page.Sample exposing (Model, Msg, init, update, view)

import Data.Sample
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Sample
import Route
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , sample_id : Int
    , sample : Data.Sample.Sample
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Sample"

        loadSample =
            Request.Sample.get id |> Http.toTask

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
    Task.map3 Model title (Task.succeed id) loadSample
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
            , viewSample model.sample
            ]
        ]


viewSample : Data.Sample.Sample -> Html msg
viewSample sample =
    let
        numFiles =
            List.length sample.sample_files

        numOntologies =
            List.length sample.ontologies
    in
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Project" ]
            , td []
                [ a [ Route.href (Route.Project sample.project_id) ] [ text sample.project.project_name ]
                ]
            ]
        , tr []
            [ th [] [ text "Sample" ]
            , td [] [ text sample.sample_name ]
            ]
        , tr []
            [ th [] [ text "Code" ]
            , td [] [ text sample.sample_acc ]
            ]
        , tr []
            [ th [] [ text "Sample Type" ]
            , td [] [ text sample.sample_type ]
            ]
        , tr []
            [ th [] [ text <| "Files (" ++ toString numFiles ++ ")" ]
            , td [] [ viewFiles sample.sample_files ]
            ]
        , tr []
            [ th [] [ text <| "Ontologies (" ++ toString numOntologies ++ ")" ]
            , td [] [ viewOntologies sample.ontologies ]
            ]
        ]


viewFiles : List Data.Sample.SampleFile -> Html msg
viewFiles files =
    case List.length files of
        0 ->
            text "NA"

        _ ->
            ul [] (List.map viewFile files)


viewFile : Data.Sample.SampleFile -> Html msg
viewFile file =
    li [] [ text file.file ]


viewOntologies : List Data.Sample.Ontology -> Html msg
viewOntologies ontologies =
    case List.length ontologies of
        0 ->
            text "NA"

        _ ->
            ul [] (List.map viewOntology ontologies)


viewOntology : Data.Sample.Ontology -> Html msg
viewOntology ont =
    let
        display =
            ont.ontology_acc
                ++ (case ont.label of
                        "" ->
                            ""

                        _ ->
                            " (" ++ ont.label ++ ")"
                   )
    in
    li [] [ text display ]
