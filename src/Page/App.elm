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
    , agaveApp : Agave.App
    , inputs : Dict String String
    , parameters : Dict String String
    , showRunDialog : Bool
    , dialogError : Maybe String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadApp =
            Request.App.get id |> Http.toTask

        loadAppFromAgave name =
            Request.Agave.getApp session.token name |> Http.toTask |> Task.map .result

        inputs app =
            app.inputs |> List.map (\input -> (input.id, "")) |> Dict.fromList

        params app =
            app.parameters |> List.map (\param -> (param.id, param.value.default)) |> Dict.fromList

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
                (\agaveApp -> Task.succeed (Model "App" id app agaveApp (inputs agaveApp) (params agaveApp) False Nothing))
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
    | OpenCart String


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
            { model | dialogError = Just (toString error) } => Cmd.none

        Acknowledge ->
            { model | showRunDialog = False } => Cmd.none

        OpenFileBrowser id ->
            let
                username =
                    case session.profile of
                        Nothing ->
                            ""

                        Just profile ->
                            profile.username
            in
            model => Ports.createFileBrowser (App.FileBrowser id username session.token "")

        OpenCart id ->
            model => Cmd.none



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

        agaveApp = model.agaveApp

        inputs =
            case agaveApp.inputs of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] (List.map viewAppInput (List.Extra.zip (List.sortBy .id agaveApp.inputs) (Dict.values model.inputs))) --FIXME simplify this

        parameters =
            case agaveApp.parameters of
                [] -> div [] [ text "None" ]
                _  -> table [ class "table" ] (List.map viewAppParameter (List.Extra.zip (List.sortBy .id agaveApp.parameters) (Dict.values model.parameters))) --FIXME simplify this
    in
    div []
    [ table [ class "table" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text app.app_name ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text agaveApp.shortDescription ]
            ]
        , tr []
            [ th [] [ text "Help" ]
            , td [] [ a [ href agaveApp.helpURI ] [ text agaveApp.helpURI ] ]
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
        agaveApp = Tuple.first input

        val = Tuple.second input

        id = agaveApp.id
    in
    tr []
    [ th [] [ text agaveApp.details.label ]
    , td []
        [ Html.input [ class "margin-right", type_ "text", size 40, name id, value val, onInput (SetInput id) ] []
        , button [ class "margin-right btn btn-default btn-sm", onClick (OpenFileBrowser id) ] [ text "CyVerse" ]
        , button [ class "btn btn-default btn-sm", onClick (OpenCart id) ] [ text "Cart" ]
        ]
    ]


viewAppParameter : (Agave.AppParameter, String) -> Html Msg
viewAppParameter input =
    let
        param = Tuple.first input

        val = Tuple.second input

        id = param.id

        interface =
            case param.value.enum_values of
                Nothing ->
                    Html.input [ type_ "text", size 40, name id, value val, onInput (SetParameter id) ] []

                Just enum ->
                    select [ onInput (SetParameter id) ]
                        (enum |> List.map (List.head >> Maybe.withDefault ("error", "error")) |> List.map (\(val, label) -> option [ value val] [ text label ]))
    in
    tr []
    [ th [] [ text param.details.label ]
    , td []
        [ interface
        ]
    ]


runDialogConfig : Model -> Dialog.Config Msg
runDialogConfig model =
    let
        content =
            case model.dialogError of
                Nothing ->
                    div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

                Just error ->
                    text error

        footer =
            case model.dialogError of
                Nothing -> Just (div [] [ text " " ])

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