module Page.Map exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode as Encode exposing (Value)
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import String exposing (toFloat)
import Task exposing (Task)
import Time exposing (Time)
import View.GMap as GMap exposing (LatLng, MapState, gmap, loadMap, setCenter)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , mapState : MapState
    }


init : String -> String -> Task PageLoadError Model
init lat lng =
    let
        -- Load page - Perform tasks to load the resources of a page
        center =
            LatLng (Result.withDefault 0 (String.toFloat lat)) (Result.withDefault 0 (String.toFloat lng))

        map =
            Task.succeed (MapState (Encode.string "google map here") center)

        title =
            Task.succeed "Map Page"

        handleLoadError _ =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home "The map page is currently unavailable."
    in
    Task.map2 Model title map
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = Tick Time
    | JSMap Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JSMap gmap ->
            { model | mapState = MapState gmap model.mapState.center } ! []

        Tick _ ->
            model ! [ loadMap model.mapState.center ]



-- VIEW --


mapStyles : Attribute msg
mapStyles =
    style
        [ ( "display", "block" )
        , ( "height", "400px" )
        , ( "width", "600px" )
        ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , button [ onClick (Tick Time.millisecond) ] [ text "Load map" ]
            , div [] [ gmap [ mapStyles ] [] ]
            ]
        ]
