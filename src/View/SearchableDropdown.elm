module View.SearchableDropdown exposing (State, Config, init, view)

import Html exposing (Html, text, div, input, table, tbody, tr, td)
import Html.Attributes exposing (class, style, type_, value, placeholder, autofocus)
import Html.Events exposing (onInput, onClick)


type alias State =
    { value : String
    , results : List (Int, String)
    , selectedId : Maybe Int
    }


type alias Config msg1 msg2 =
        { placeholder : String
        , inputMsg : String -> msg1
        , selectMsg : Int -> String -> msg2
        }


init : State
init =
    State "" [] Nothing


view : Config msg msg -> State -> Html msg
view config state =
    let
        invOption (id, name) =
            tr [ onClick (config.selectMsg id name) ] [ td [] [ text name ] ]

        resultTable =
            div [ style [("overflow-y","auto"),("max-height","10em")] ]
                [ table [ class "table-condensed table-hover", style [("width","100%")] ]
                    [ tbody [] (List.map invOption state.results) ]
                ]
    in
    div []
        [ input [ class "form-control", type_ "text", autofocus True, value state.value, placeholder config.placeholder, onInput config.inputMsg ] []
        , if state.results /= [] then
            resultTable
          else
            text ""
        ]
