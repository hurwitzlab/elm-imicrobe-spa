module Page.Investigator exposing (Model, Msg, init, update, view)

import Data.Investigator
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Investigator
import Route
import Task exposing (Task)
import View.Page as Page


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
    Task.map3 Model title (Task.succeed id) loadInvestigator
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
        [ viewInvestigator model.investigator
        , viewProjects model.investigator.projects
        , viewSamples model.investigator.samples
        ]


viewInvestigator : Data.Investigator.Investigator -> Html msg
viewInvestigator inv =
    let
        numProjects =
            List.length inv.projects

        numSamples =
            List.length inv.samples
    in
    div []
        [ div []
            [ h2 [] [ text "Investigator" ]
            , table [ class "table" ]
                [ tr []
                    [ th [] [ text "Name" ]
                    , td [] [ text inv.investigator_name ]
                    ]
                , tr []
                    [ th [] [ text "Institution" ]
                    , td [] [ text inv.institution ]
                    ]
                ]
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
                    table [ class "table" ]
                        (List.map viewProject projects)
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
                    table [] (cols :: rows)
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