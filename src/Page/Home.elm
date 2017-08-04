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
    = AddCart Int
    | RemoveCart Int
    | VoidCart


type ExternalMsg
    = NoOp
    | AddToCart Int
    | EmptyCart
    | RemoveFromCart Int


update : Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update msg model =
    case msg of
        AddCart id ->
            model => Cmd.none => AddToCart id

        VoidCart ->
            model => Cmd.none => EmptyCart

        RemoveCart id ->
            model => Cmd.none => RemoveFromCart id



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        mkTable =
            table [] (List.map mkTr [ 1, 2, 3 ])

        mkTr item =
            tr []
                [ td [] [ text <| toString item ]
                , button [ onClick (AddCart item) ] [ text "Add" ]
                , button [ onClick (RemoveCart item) ] [ text "Delete" ]
                ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            , div [] [ mkTable ]
            , div [] [ button [ onClick VoidCart ] [ text "Empty" ] ]
            , div [] [ text (toString session) ]
            ]
        ]
