module Page.App exposing (Model, Msg, init, update, view)

import Data.App
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.App
import Route
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , app_id : Int
    , app : Data.App.App
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "App"

        loadApp =
            Request.App.get id |> Http.toTask

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
    Task.map3 Model title (Task.succeed id) loadApp
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
            [ div [ class "page-header" ]
                [ h2 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.app.app_name ]
                    ]
                ]
            ]
            , viewApp model.app
        ]


viewApp : Data.App.App -> Html msg
viewApp app =
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text app.app_name ]
            ]
        ]
