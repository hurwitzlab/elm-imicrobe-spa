module Page.Assembly exposing (Model, Msg, init, update, view)

import Data.Assembly
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Assembly
import Route
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , assembly_id : Int
    , assembly : Data.Assembly.Assembly
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Assembly"

        loadAssembly =
            Request.Assembly.get id |> Http.toTask
    in
    Task.map3 Model title (Task.succeed id) loadAssembly
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
                        [ text model.assembly.assembly_name ]
                    ]
                ]
            , viewAssembly model.assembly
            ]
        ]


viewAssembly : Data.Assembly.Assembly -> Html msg
viewAssembly assembly =
    let
        projectLink =
            case assembly.project of
                Nothing ->
                    text "NA"

                Just project ->
                    a
                        [ Route.href (Route.Project project.project_id) ]
                        [ text project.project_name ]

        sampleLink =
            case assembly.sample of
                Nothing ->
                    text "NA"

                Just sample ->
                    a
                        [ Route.href (Route.Sample sample.sample_id) ]
                        [ text sample.sample_name ]

        cdsLink =
            case assembly.cds_file of
                "" -> text "No"

                _ -> text "Yes"

        ntLink =
            case assembly.nt_file of
                "" -> text "No"

                _ -> text "Yes"

        pepLink =
            case assembly.pep_file of
                "" -> text "No"

                _ -> text "Yes"

    in
    table [ class "table table-condensed" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text assembly.assembly_name ]
            ]
        , tr []
            [ th [] [ text "Code" ]
            , td [] [ text assembly.assembly_code ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text assembly.description ]
            ]
        , tr []
            [ th [] [ text "Organism" ]
            , td [] [ text assembly.organism ]
            ]
        , tr []
            [ th [] [ text "Sample" ]
            , td [] [ sampleLink ]
            ]
        , tr []
            [ th [] [ text "Project" ]
            , td [] [ projectLink ]
            ]
        , tr []
            [ th [] [ text "CDS" ]
            , td [] [ cdsLink ]
            ]
        , tr []
            [ th [] [ text "NT" ]
            , td [] [ ntLink ]
            ]
        , tr []
            [ th [] [ text "Peptides" ]
            , td [] [ pepLink ]
            ]
        ]