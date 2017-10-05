module Data.Cart exposing (Cart, empty, size, add, remove, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Set
import Util exposing ((=>))



type alias Cart =
    { contents : Set.Set Int
    }


empty : Cart
empty =
    Cart Set.empty


size : Cart -> Int
size cart =
    Set.size cart.contents


add : Int -> Cart -> Cart
add id cart =
    let
        newContents =
            Set.insert id cart.contents
    in
    { cart | contents = newContents }


remove : Int -> Cart -> Cart
remove id cart =
    let
        newContents =
            Set.remove id cart.contents
    in
    { cart | contents = newContents }


decoder : Decoder Cart
decoder =
    decode Cart
        |> required "contents" (Decode.list Decode.int |> Decode.map Set.fromList)


encode : Cart -> Value
encode cart =
    Encode.object
        [ "contents" => (cart.contents |> Set.toList |> List.map Encode.int |> Encode.list)
        ]