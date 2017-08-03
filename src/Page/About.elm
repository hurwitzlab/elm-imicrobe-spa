module Page.About exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
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
            Task.succeed "About Page"

        body =
            Task.succeed "About page is the page where you can read all about the about page!"

        handleLoadError _ =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home "The about page is currently unavailable."
    in
    Task.map2 Model title body
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = Todo


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text model.pageBody ]
            ]
        ]
