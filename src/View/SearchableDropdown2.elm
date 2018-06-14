module View.SearchableDropdown2 exposing (State, Config, init, view)

import Html exposing (Html, text, div, span, input, button, table, tbody, tr, td)
import Html.Attributes exposing (class, style, type_, attribute, value, placeholder, autofocus)
import Html.Events exposing (onInput, onClick)



type alias State =
    { show : Bool
    , value : String
    , results : List (String, String)
    , selectedId : Maybe Int
    }


type alias Config msg1 msg2 msg3 =
        { placeholder : String
        , autofocus : Bool
        , inputMsg : String -> msg1
        , selectMsg : String -> String -> msg2
        , toggleMsg : msg3
        , className : String
        }


init : State
init =
    State False "" [] Nothing


view : Config msg msg msg -> State -> Html msg
view config state =
    let
        invOption (id, name) =
            tr [ onClick (config.selectMsg id name) ] [ td [] [ text name ] ]

        resultTable =
            div [ style [("overflow-y","auto"),("max-height","10em"),("border","1pt solid lightgray")] ]
                [ table [ class "table-condensed table-hover", style [("width","100%")] ]
                    [ tbody [] (List.map invOption state.results) ]
                ]
    in
    div [ class config.className ]
        [ div [ class "input-group" ]
            [ input [ class "form-control", type_ "text", autofocus config.autofocus, value state.value, placeholder config.placeholder, onInput config.inputMsg ] []
            , div [ class "input-group-btn" ]
                [ button [ class "btn btn-default dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown", onClick config.toggleMsg ]
                    [ span [ class "caret" ] [] ]
                ]
        ]
        , if state.show || (state.value /= "" && state.results /= []) then
            resultTable
          else
            text ""
        ]
