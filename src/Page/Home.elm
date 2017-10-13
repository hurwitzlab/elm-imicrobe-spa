module Page.Home exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Encode as Encode
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

        loadBody =
            Http.getString "main.html" |> Http.toTask
    in
    Task.map2 Model title loadBody
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
    div [  (Html.Attributes.property "innerHTML" (Encode.string model.pageBody)) ] []