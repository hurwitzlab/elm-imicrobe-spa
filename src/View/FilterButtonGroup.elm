module View.FilterButtonGroup exposing (Config, view)

import Html exposing (Html, text, div, button)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)



type alias Config msg =
    { options : List String
    , toMsg : String -> msg
    }


permissionFilterConfig : (String -> msg) -> Config msg
permissionFilterConfig toMsg =
    Config [ "All", "Mine" ] toMsg


view : Config msg -> String -> Html msg
view config selected =
    let
        filterButton label =
            button [ class "btn btn-default", classList [("active", label == selected)], onClick (config.toMsg label) ]
                [ text label ]
    in
    div [ class "btn-group btn-group-sm" ]
        (List.map filterButton config.options)
