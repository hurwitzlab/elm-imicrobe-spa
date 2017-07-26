port module View.GMap exposing (MapState, LatLng, loadMap, setCenter, gmap)

import Html exposing (Html, Attribute, node)
import Json.Encode as Encode exposing (Value)


type alias LatLng =
    { lat : Float
    , lng : Float
    }


type alias MapState =
    { gmap : Value
    , center : LatLng
    }


port loadMap : LatLng -> Cmd msg


port setCenter : MapState -> Cmd msg


port receiveMap : (Value -> msg) -> Sub msg


gmap : List (Attribute msg) -> List (Html msg) -> Html msg
gmap =
    node "gmap"