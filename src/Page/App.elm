module Page.App exposing (Model, Msg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.App as App exposing (App, AppRun)
import Data.Agave as Agave
import Data.Sample as Sample exposing (Sample, SampleFile)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.App
import Request.Agave
import Request.Sample
import Route
import Ports
import Task exposing (Task)
import View.Cart as Cart
import Dict as Dict exposing (Dict)
import List.Extra
import Util exposing ((=>))
import Set



---- MODEL ----


type alias Model =
    { pageTitle : String
    , app_id : Int
    , app : App
    , agaveApp : Agave.App
    , inputs : Dict String String
    , parameters : Dict String String
    , cart : Cart.Model
    , samples : List Sample
    , files : List SampleFile
    , showRunDialog : Bool
    , cartDialogInputId : Maybe String
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
            app.inputs |> List.map (\input -> (input.id, input.value.default)) |> Dict.fromList

        params app =
            app.parameters |> List.map (\param -> (param.id, param.value.default)) |> Dict.fromList

        cart =
            Cart.init session.cart Cart.Selectable
    in
    loadApp |> Task.andThen
        (\app ->
            (loadAppFromAgave app.app_name |> Task.andThen
                (\agaveApp -> Task.succeed (Model "App" id app agaveApp (inputs agaveApp) (params agaveApp) cart [] [] False Nothing Nothing))
            )
        )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetInput String String
    | SetParameter String String
    | RunJob
    | RunJobCompleted (Result Http.Error (Request.Agave.Response Agave.JobStatus))
    | AppRunCompleted (Result Http.Error AppRun)
    | CloseRunDialog
    | OpenFileBrowser String
    | OpenCart String
    | LoadCartCompleted (Result Http.Error ((List Sample), (List SampleFile)))
    | CloseCartDialog
    | CartMsg Cart.Msg


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

                cmd1 = Request.Agave.launchJob session.token jobRequest
                    |> Http.send RunJobCompleted

                cmd2 = Request.App.run model.app_id 13 (Agave.encodeJobRequest jobRequest |> toString) --FIXME user_id hardcoded to mbomhoff
                    |> Http.send AppRunCompleted
            in
            { model | showRunDialog = True } => Cmd.batch [ cmd1, cmd2 ]

        RunJobCompleted (Ok response) ->
            --TODO add job to app_run table
            model => Route.modifyUrl (Route.Job response.result.id)

        RunJobCompleted (Err error) ->
            { model | dialogError = Just (toString error) } => Cmd.none

        AppRunCompleted (Ok response) ->
            model => Cmd.none

        AppRunCompleted (Err error) ->
            model => Cmd.none

        CloseRunDialog ->
            { model | showRunDialog = False } => Cmd.none

        CloseCartDialog ->
            let
                selected =
                    model.cart.selected

                sampleIds =
                    Set.toList selected.contents

                match file =
                    if (List.member file.sample_id sampleIds) then
                        Just file.file
                    else
                        Nothing

                filesStr =
                    List.filterMap match model.files |> String.join ";"

                msg =
                    SetInput (Maybe.withDefault "" model.cartDialogInputId) filesStr
            in
            update session msg { model | cartDialogInputId = Nothing }

        OpenFileBrowser inputId ->
            let
                username =
                    case session.profile of
                        Nothing ->
                            ""

                        Just profile ->
                            profile.username
            in
            model => Ports.createFileBrowser (App.FileBrowser inputId username session.token "")

        OpenCart inputId ->
            let
                id_list =
                    session.cart.contents |> Set.toList

                cmd =
                    Task.attempt LoadCartCompleted <|
                        Task.map2 (\samples files -> (samples, files))
                            (Request.Sample.getSome id_list |> Http.toTask)
                            (Request.Sample.files id_list |> Http.toTask)
            in
            { model | cartDialogInputId = Just inputId } => cmd

        LoadCartCompleted (Ok (samples, files)) ->
            { model | samples = samples, files = files } => Cmd.none

        LoadCartCompleted (Err error) ->
            model => Cmd.none

        CartMsg subMsg ->
            let
                _ = Debug.log "App.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage )  =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd



-- VIEW --


view : Model -> Html Msg
view model =
    let
        showCartDialog =
            case model.cartDialogInputId of
                Nothing -> False
                _ -> True
    in
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
                 else if showCartDialog then
                    Just (cartDialogConfig model)
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
        , tr []
            [ th [] [ text "Version" ]
            , td [] [ text agaveApp.version ]
            ]
        , tr []
            [ th [] [ text "Tags" ]
            , td [] [ text (String.join ", " agaveApp.tags) ]
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
                            , onClick CloseRunDialog
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


cartDialogConfig : Model -> Dialog.Config Msg
cartDialogConfig model =
    let
        content =
            case List.length model.samples of
                0 ->
                    text "Cart is empty"

                _ ->
                    viewCart model

        footer =
            button
                [ class "btn btn-success"
                , onClick CloseCartDialog
                ]
                [ text "OK" ]

    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Cart" ])
    , body = Just content
    , footer = Just footer
    }


viewCart : Model -> Html Msg
viewCart model =
    case model.samples of
        [] -> text "The cart is empty"

        _ ->
            div [] [ Cart.viewCart model.cart model.samples |> Html.map CartMsg ]