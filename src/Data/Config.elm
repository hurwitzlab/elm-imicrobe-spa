module Data.Config exposing (Config, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Config =
    { oauthClientId : String
    }


decoder : Decoder Config
decoder =
    decode Config
        |> required "oauthClientId" Decode.string

