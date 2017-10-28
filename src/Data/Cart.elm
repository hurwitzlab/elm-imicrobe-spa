module Data.Cart exposing (..)

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


contains : Cart -> Int -> Bool
contains cart id =
    Set.member id cart.contents


add : Cart -> Int -> Cart
add cart id =
    let
        newContents =
            Set.insert id cart.contents
    in
    { cart | contents = newContents }


addList : Cart -> List Int -> Cart
addList cart ids =
    let
        newContents =
            Set.union (Set.fromList ids) cart.contents
    in
    { cart | contents = newContents }


remove : Cart -> Int -> Cart
remove cart id =
    let
        newContents =
            Set.remove id cart.contents
    in
    { cart | contents = newContents }


removeList : Cart -> List Int -> Cart
removeList cart ids =
    let
        newContents =
            Set.diff cart.contents (Set.fromList ids)
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