module Page.CombinedAssembly exposing (Model, Msg, init, update, view)

import Data.CombinedAssembly
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.CombinedAssembly
import Route
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , combined_assembly_id : Int
    , combined_assembly : Data.CombinedAssembly.CombinedAssembly
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Combined Assembly"

        loadCombinedAssembly =
            Request.CombinedAssembly.get id |> Http.toTask
    in
    Task.map3 Model title (Task.succeed id) loadCombinedAssembly
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
                        [ text model.combined_assembly.assembly_name ]
                    ]
                ]
            , viewAssembly model.combined_assembly
            , viewSamples model.combined_assembly.samples
            ]
        ]


viewAssembly : Data.CombinedAssembly.CombinedAssembly -> Html msg
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

        annoLink =
            case assembly.anno_file of
                "" -> text "No"

                _ -> text "Yes"

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
    table []
        [ tbody []
            [ tr []
                [ th [] [ text "Name" ]
                , td [] [ text assembly.assembly_name ]
                ]
            , tr []
                [ th [] [ text "Project" ]
                , td [] [ projectLink ]
                ]
            , tr []
                [ th [] [ text "Phlyum" ]
                , td [] [ text assembly.phylum ]
                ]
            , tr []
                [ th [] [ text "Class" ]
                , td [] [ text assembly.class ]
                ]
            , tr []
                [ th [] [ text "Genus" ]
                , td [] [ text assembly.genus ]
                ]
            , tr []
                [ th [] [ text "Species" ]
                , td [] [ text assembly.species ]
                ]
            , tr []
                [ th [] [ text "Strain" ]
                , td [] [ text assembly.strain ]
                ]
            , tr []
                [ th [] [ text "PCR Amp?" ]
                , td [] [ text assembly.pcr_amp ]
                ]
            , tr []
                [ th [] [ text "Annotations" ]
                , td [] [ annoLink ]
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
        ]


viewSamples : List Data.CombinedAssembly.Sample -> Html msg
viewSamples samples =
    let
        numSamples =
            List.length samples

        label =
            case numSamples of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numSamples)
                        ]
        cols =
            tr []
                (List.map (\s -> th [] [ text s ]) [ "Name", "Type" ])

        rows =
            List.map viewSample samples

        body =
            case numSamples of
                0 ->
                    text "NA"

                _ ->
                    table [ class "table table-condensed" ] [ tbody [] (cols :: rows) ]
    in
    div []
        [ h2 []
            [ text "Samples "
            , label
            ]
        , body
        ]


viewSample : Data.CombinedAssembly.Sample -> Html msg
viewSample sample =
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        ]
