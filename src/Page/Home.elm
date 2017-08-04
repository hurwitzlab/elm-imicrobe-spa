module Page.Home exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Home Page"

        body =
            Task.succeed "Welcome to the homepage!"

        handleLoadError _ =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home "The homepage is currently unavailable."
    in
    Task.map2 Model title body
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetCart String


type ExternalMsg
    = NoOp
    | AddToCart String


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        SetCart val ->
            model => Cmd.none => AddToCart val



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        mkTable =
            table [] (List.map mkTr [ 1, 2, 3 ])

        mkTr item =
            tr []
                [ td [] [ text <| toString item ]
                , button [ onClick (SetCart (toString item)) ] [ text "Add" ]
                ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , div [] [ mkTable ]
            , div [] [ text (toString session) ]
            ]
        ]
