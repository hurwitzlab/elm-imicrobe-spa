module View.Tags exposing (State, Config, empty, init, add, remove, selected, view)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Dict exposing (Dict)


type alias State =
    { selected : Dict Int String
    }


type alias Config msg =
    { removeMsg : Int -> msg
    }


empty : State
empty =
    State Dict.empty


init : List (Int, String) -> State
init selected =
    State (Dict.fromList selected)


add : Int -> String -> State -> State
add id label state =
    { state | selected = Dict.insert id label state.selected }


remove : Int -> State -> State
remove id state =
    { state | selected = Dict.remove id state.selected }


selected : State -> List (Int, String)
selected state =
    Dict.toList state.selected


view : Config msg -> State -> Html msg
view config state =
    if (state /= empty) then
        div [ style [("display","inline-block")] ]
            (List.map (\(i, s) -> span [ class "badge", style [("margin","0.25em"),("padding","0.5em")] ] [ text s, text " ", span [ class "glyphicon glyphicon-remove", style [("cursor","pointer")], onClick (config.removeMsg i) ] [] ] ) (Dict.toList state.selected))
    else
        text ""