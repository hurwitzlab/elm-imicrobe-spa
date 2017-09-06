module Page.App exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.App as App exposing (App)
import Data.Agave as Agave
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.App
import Request.Agave
import Route
import Ports
import Task exposing (Task)
import View.Page as Page
import Dict as Dict exposing (Dict)
import List.Extra
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , app_id : Int
    , app : App
    , app_spec : Agave.App
    , inputs : Dict String String
    , parameters : Dict String String
    , showRunDialog : Bool
    , dialogError : String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadApp =
            Request.App.get id |> Http.toTask

        loadAppFromAgave name =
            Request.Agave.getApp session.token name |> Http.toTask |> Task.map .result

        inputs spec =
            Dict.fromList (List.map (\input -> (input.id, "")) spec.inputs)

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
                (\spec -> Task.succeed (Model "App" id app spec (inputs spec) Dict.empty False ""))
            )
        )
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetInput String String
    | SetParameter String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))
    | Acknowledge
    | OpenFileBrowser String


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
            { model | showRunDialog = True } => cmd

        RunJobCompleted (Ok response) ->
            --TODO add job to app_run table
            model => Route.modifyUrl (Route.Job response.result.id)

        RunJobCompleted (Err error) ->
            { model | dialogError = toString error } => Cmd.none

        Acknowledge ->
            { model | showRunDialog = False } => Cmd.none

        OpenFileBrowser id ->
            model => Ports.createFileBrowser (App.FileBrowser id session.username session.token "")



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
            , viewApp model
            , div [ class "center" ]
                [ hr [] []
                , button [ class "btn btn-primary btn-lg", onClick RunJob ] [ text "Run" ]
                ]
            , Dialog.view
                (if model.showRunDialog then
                    Just (runDialogConfig model)
                 else
                    Nothing
                )
        ]


viewApp : Model -> Html Msg
viewApp model =
    let
        app = model.app

        spec = model.app_spec

        inputs =
            case spec.inputs of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] (List.map viewAppInput (List.Extra.zip spec.inputs (Dict.values model.inputs)) ) --FIXME rewrite this

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


viewAppInput : (Agave.AppInput, String) -> Html Msg
viewAppInput input =
    let
        _ = Debug.log "input" (toString input)

        spec = Tuple.first input

        val = Tuple.second input

        id = spec.id
    in
    tr []
    [ th [] [ text spec.details.label ]
    , td []
        [ Html.input [ class "margin-right", type_ "text", size 40, name id, value val, onInput (SetInput id) ] []
        , button [ class "margin-right btn btn-default btn-sm", onClick (OpenFileBrowser id) ] [ text "CyVerse" ]
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


runDialogConfig : Model -> Dialog.Config Msg
runDialogConfig model =
    let
        content =
            case model.dialogError of
                "" ->
                    div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

                _ ->
                    text model.dialogError

        footer =
            case model.dialogError of
                "" -> Just (div [] [ text " " ])

                _ ->
                    Just
                        (button
                            [ class "btn btn-success"
                            , onClick Acknowledge
                            ]
                            [ text "OK" ]
                        )
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Submitting Job" ])
    , body = Just content
    , footer = footer
    }