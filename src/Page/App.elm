module Page.App exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.App as App exposing (App)
import Data.Agave as Agave
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.App
import Request.Agave
import Route
import Task exposing (Task)
import View.Page as Page
import Dict as Dict exposing (Dict)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , app_id : Int
    , app : App
    , app_spec : Agave.App
    , inputs : Dict String String
    , parameters : Dict String String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadApp =
            Request.App.get id |> Http.toTask

        loadAppFromAgave name =
            Request.Agave.getApp session.token name |> Http.toTask |> Task.map .result

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
    loadApp |> Task.andThen
        (\app ->
            (loadAppFromAgave app.app_name |> Task.andThen
                (\spec -> Task.succeed (Model "App" id app spec Dict.empty Dict.empty))
            )
        )
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetInput String String
    | SetParameter String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SetInput id value ->
            let
                newInputs = Dict.insert id value model.inputs
            in
            { model | inputs = newInputs } => Cmd.none

        SetParameter id value ->
            let
                newParams = Dict.insert id value model.parameters
            in
            { model | parameters = newParams } => Cmd.none

        RunJob ->
            let
                jobInputs =
                    Dict.toList model.inputs |> List.map (\(k, v) -> Agave.JobInput k v)

                jobParameters =
                    Dict.toList model.parameters |> List.map (\(k, v) -> Agave.JobParameter k v)

                jobRequest =
                    Agave.JobRequest "job name here" model.app.app_name False jobInputs jobParameters []

                cmd = Request.Agave.launchJob session.token jobRequest
                    |> Http.send RunJobCompleted
            in
            ( model, cmd )

        RunJobCompleted (Ok response) ->
            --TODO add job to app_run table
            model => Route.modifyUrl (Route.Job response.result.id)

        RunJobCompleted (Err error) ->
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
                        [ text model.app.app_name ]
                    ]
                ]
            ]
            , viewApp model.app model.app_spec
            , div [ class "center" ]
                [ hr [] []
                , button [ class "btn btn-primary btn-lg", onClick RunJob ] [ text "Run" ]
                ]
        ]


viewApp : App -> Agave.App -> Html Msg
viewApp app spec =
    let
        inputs =
            case spec.inputs of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] (List.map viewAppInput spec.inputs)

        parameters =
            case spec.parameters of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] (List.map viewAppParameter spec.parameters)
    in
    div []
    [ table [ class "table" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text app.app_name ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text spec.shortDescription ]
            ]
        , tr []
            [ th [] [ text "Help" ]
            , td [] [ a [ href spec.helpURI ] [ text spec.helpURI ] ]
            ]
        ]
    , h3 [] [ text "Inputs" ]
    , inputs
    , h3 [] [ text "Parameters" ]
    , parameters
    ]


viewAppInput : Agave.AppInput -> Html Msg
viewAppInput input =
    tr []
    [ th [] [ text input.details.label ]
    , td []
        [ Html.input [ class "margin-right", type_ "text", size 40, name input.id, onInput (SetInput input.id) ] []
        , button [ class "margin-right btn btn-default btn-sm" ] [ text "CyVerse" ]
        , button [ class "btn btn-default btn-sm" ] [ text "Cart" ]
        ]
    ]


viewAppParameter : Agave.AppParameter -> Html Msg
viewAppParameter param =
    tr []
    [ th [] [ text param.details.label ]
    , td []
        [ select [ onInput (SetParameter param.id) ]
            (param.value.enum_values |> List.map (List.head >> Maybe.withDefault ("error", "error")) |> List.map (\(val, label) -> option [ value val] [ text label ]))
        ]
    ]