module Page.Sample exposing (Model, Msg, init, update, view)

import Data.Sample
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Sample
import Route
import Task exposing (Task)
import Config exposing (dataCommonsUrl)



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
    in
    Task.map3 Model title (Task.succeed id) loadSample
        |> Task.mapError Error.handleLoadError



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
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.sample.sample_name ]
                    ]
                ]
            , viewSample model.sample
            , viewFiles model.sample.sample_files
            , viewOntologies model.sample.ontologies
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
        ]


viewFiles : List Data.Sample.SampleFile2 -> Html msg
viewFiles files =
    let
        numFiles =
            List.length files

        label =
            case numFiles of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numFiles)
                        ]

        body =
            case numFiles of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (List.map viewFile files)
    in
    div []
        [ h2 []
            [ text "Files "
            , label
            ]
        , body
        ]


viewFile : Data.Sample.SampleFile2 -> Html msg
viewFile file =
    tr []
        [ td []
            [ a [ href (dataCommonsUrl ++ file.file) ] [ text file.file ]
            ]
        ]


viewOntologies : List Data.Sample.Ontology -> Html msg
viewOntologies ontologies =
    let
        numOntologies =
            List.length ontologies

        label =
            case numOntologies of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numOntologies)
                        ]

        body =
            case numOntologies of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (List.map viewOntology ontologies)
    in
    div []
        [ h2 []
            [ text "Ontologies "
            , label
            ]
        , body
        ]


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
    tr []
        [ td []
            [ text display
            ]
        ]
