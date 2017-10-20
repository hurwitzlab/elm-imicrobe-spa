module Page.Investigator exposing (Model, Msg, init, update, view)

import Data.Investigator
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Investigator
import Route
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , investigator_id : Int
    , investigator : Data.Investigator.Investigator
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Investigator"

        loadInvestigator =
            Request.Investigator.get id |> Http.toTask
    in
    Task.map3 Model title (Task.succeed id) loadInvestigator
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
                        [ text model.investigator.investigator_name ]
                    ]
                ]
            , viewInvestigator model.investigator
            , viewProjects model.investigator.projects
            , viewSamples model.investigator.samples
            ]
        ]


viewInvestigator : Data.Investigator.Investigator -> Html msg
viewInvestigator inv =
    let
        title =
            "Investigator: " ++ inv.investigator_name

        institution =
            case inv.institution of
                "" -> "None"

                _ -> inv.institution

        numProjects =
            List.length inv.projects

        numSamples =
            List.length inv.samples
    in
    table [ class "table" ]
        [ colgroup []
            [ col [ class "col-md-2" ] [] ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text inv.investigator_name ]
            ]
        , tr []
            [ th [] [ text "Institution" ]
            , td [] [ text institution ]
            ]
        , tr []
            [ th [] [ text "Link" ]
            , td [] [ a [ href inv.url ] [ text inv.url ] ]
            ]
        ]


viewProjects : List Data.Investigator.Project -> Html msg
viewProjects projects =
    let
        numProjects =
            List.length projects

        label =
            case numProjects of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numProjects)
                        ]

        body =
            case numProjects of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (List.map viewProject projects) ]
    in
    div []
        [ h2 []
            [ text "Projects "
            , label
            ]
        , body
        ]


viewProject : Data.Investigator.Project -> Html msg
viewProject project =
    tr []
        [ td []
            [ a [ Route.href (Route.Project project.project_id) ]
                [ text project.project_name ]
            ]
        ]


viewSamples : List Data.Investigator.Sample -> Html msg
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
                    text "None"

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


viewSample : Data.Investigator.Sample -> Html msg
viewSample sample =
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        ]