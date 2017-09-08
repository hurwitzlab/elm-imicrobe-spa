--module Page.Home exposing (ExternalMsg(..), Model, Msg, init, update, view)
module Page.Home exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)



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
    in
    Task.map2 Model title body
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
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            ]
        ]