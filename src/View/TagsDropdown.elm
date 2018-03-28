module View.TagsDropdown exposing (State, Config, empty, init, add, remove, selected, view)

import Html exposing (Html, text, div, span, button, a, ul, li)
import Html.Attributes exposing (class, style, attribute, type_, value)
import Html.Events exposing (onClick)
import Dict exposing (Dict)


type alias State =
    { selected : Dict Int String
    }


type alias Config msg1 msg2 =
        { options : List (Int, String)
        , addMsg : Int -> String -> msg1
        , removeMsg : Int -> msg2
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


view : Config msg msg -> State -> Html msg
view config state =
    div []
        [ div [ class "dropdown", style [("display","inline-block")] ]
            [ button [ class "btn btn-default dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Add ", span [ class "caret" ] [] ]
            , ul [ class "dropdown-menu" ]
                (List.map (\(i, s) -> li [ onClick (config.addMsg i s) ] [ a [] [ text s ]]) config.options)
            ]
        , text " "
        , div [ style [("display","inline-block")] ]
            (List.map (\(i, s) -> span [ class "badge", style [("margin","0.25em"),("padding","0.5em")] ] [ text s, text " ", span [ class "glyphicon glyphicon-remove", style [("cursor","pointer")], onClick (config.removeMsg i) ] [] ] ) (Dict.toList state.selected))
        ]
