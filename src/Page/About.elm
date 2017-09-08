module Page.About exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import View.Page as Page



---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageBody : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "About Page"

        body =
            Task.succeed "About page is the page where you can read all about the about page!"
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
