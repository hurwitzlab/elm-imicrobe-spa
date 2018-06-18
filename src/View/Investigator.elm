module View.Investigator exposing (viewList)

import Html exposing (Html, a, text)
import Route



viewList : List { a | investigator_id : Int, investigator_name : String } -> List (Html msg)
viewList investigators =
    if investigators == [] then
        [ text "None" ]
    else
        List.sortBy .investigator_name investigators |> List.map viewLink |> List.intersperse (text ", ")


viewLink : { a | investigator_id : Int, investigator_name : String } -> Html msg
viewLink investigator =
    a [ Route.href (Route.Investigator investigator.investigator_id) ]
        [ text investigator.investigator_name ]
