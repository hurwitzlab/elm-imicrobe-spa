module Page.Job exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Task exposing (Task)
import Dict exposing (Dict)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , job_id : String
    , job : Agave.Job
    }


init : Session -> String -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Job"

        loadJob =
            Request.Agave.getJob session.token id |> Http.toTask |> Task.map .result
    in
    Task.map3 Model title (Task.succeed id) loadJob
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
                        [ text model.job.name ]
                    ]
                ]
            , viewJob model.job
            , viewInputs model.job.inputs
            , viewParameters model.job.parameters
            ]
        ]


viewJob : Agave.Job -> Html msg
viewJob job =
    table [ class "table" ]
        [ tr []
            [ th [] [ text "ID" ]
            , td [] [ text job.id ]
            ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text job.name ]
            ]
        , tr []
            [ th [] [ text "App" ]
            , td [] [ text job.app_id ]
            ]
        , tr []
            [ th [] [ text "Start Time" ]
            , td [] [ text job.startTime ]
            ]
        , tr []
            [ th [] [ text "End Time" ]
            , td [] [ text job.endTime ]
            ]
        , tr []
            [ th [] [ text "Status" ]
            , td [] [ text job.status ]
            ]
        ]


viewInputs : Dict String (List String) -> Html msg
viewInputs inputs =
    let
        count =
            Dict.size inputs

        label =
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString count)
                        ]

        body =
            case count of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (Dict.toList inputs |> List.map viewInput)
    in
    div []
        [ h2 []
            [ text "Inputs "
            , label
            ]
        , body
        ]


viewInput : (String, List String) -> Html msg
viewInput (id, values) =
    tr []
        [ th [] [ text id ]
        , td [] [ text (String.join ", " values) ]
        ]


viewParameters : Dict String String -> Html msg
viewParameters params =
    let
        count =
            Dict.size params

        label =
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString count)
                        ]

        body =
            case count of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (Dict.toList params |> List.map viewParameter)
    in
    div []
        [ h2 []
            [ text "Parameters "
            , label
            ]
        , body
        ]


viewParameter : (String, String) -> Html msg
viewParameter (id, value) =
    let
        display =
            "foo"
    in
    tr []
        [ th [] [ text id ]
        , td [] [ text value ]
        ]