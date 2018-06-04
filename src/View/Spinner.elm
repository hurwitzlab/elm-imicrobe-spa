module View.Spinner exposing (spinner)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


spinner : Html msg
spinner =
    div [ class "center" ]
        [ div [ class "padded-xl spinner" ] []
        ]