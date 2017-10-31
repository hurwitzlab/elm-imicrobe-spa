module Events exposing (onKeyDown)

import Html exposing (Attribute)
import Html.Events exposing (on, keyCode, onInput)
import Json.Decode as Json



onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)
