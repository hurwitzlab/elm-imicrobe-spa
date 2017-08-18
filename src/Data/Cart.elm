module Data.Cart exposing (Cart, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Decode.Extra as Extra exposing (set)
import Json.Encode as Encode exposing (Value)
import Set
import Util exposing ((=>))


type alias Cart =
    { contents : Set.Set Int
    }


decoder : Decoder Cart
decoder =
    decode Cart
        |> required "contents" (Decode.list Decode.int |> Decode.map Set.fromList)


encode : Cart -> Value
encode cart =
    Encode.object
        [ "contents" => (cart.contents |> Set.toList |> List.map Encode.int |> Encode.list)
        ]