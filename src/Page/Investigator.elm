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
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , text (toString model.investigator)
            , viewInvestigator model.investigator
            ]
        ]


viewInvestigator : Data.Investigator.Investigator -> Html msg
viewInvestigator inv =
    let
        numProjects =
            List.length inv.projects

        numSamples =
            List.length inv.samples
    in
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text inv.investigator_name ]
            ]
        , tr []
            [ th [] [ text "Institution" ]
            , td [] [ text inv.institution ]
            ]
        , tr []
            [ th [] [ text <| "Projects (" ++ toString numProjects ++ ")" ]
            , td [] [ viewProjects inv.projects ]
            ]
        , tr []
            [ th [] [ text <| "Samples (" ++ toString numSamples ++ ")" ]
            , td [] [ viewSamples inv.samples ]
            ]
        ]


viewProjects : List Data.Investigator.Project -> Html msg
viewProjects projects =
    case List.length projects of
        0 ->
            text ""

        _ ->
            ul [] (List.map viewProject projects)


viewProject : Data.Investigator.Project -> Html msg
viewProject project =
    li []
        [ a [ Route.href (Route.Project project.project_id) ]
            [ text project.project_name ]
        ]


viewSamples : List Data.Investigator.Sample -> Html msg
viewSamples samples =
    case List.length samples of
        0 ->
            text ""

        _ ->
            ul [] (List.map viewSample samples)


viewSample : Data.Investigator.Sample -> Html msg
viewSample sample =
    li []
        [ a [ Route.href (Route.Sample sample.sample_id) ]
            [ text sample.sample_name ]
        ]
