module View.Widgets exposing (..)

import Html exposing (Html, text, div, span)
import Html.Attributes exposing (class, style)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)


counter : Int -> Html msg
counter num =
    let
        myLocale =
            { usLocale | decimals = 0 }

        numStr =
            num |> toFloat |> format myLocale
    in
    if num == 0 then
        text ""
    else
        span [ class "badge" ]
            [ text numStr ]