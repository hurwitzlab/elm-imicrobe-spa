module Data.Session exposing (Session, decoder, encode)

import Data.Cart as Cart exposing (Cart)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Session =
    { cart : Cart
    , token : String
    }


decoder : Decoder Session
decoder =
    decode Session
        |> required "cart" Cart.decoder
        |> optional "token" Decode.string ""


encode : Session -> Value
encode session =
    Encode.object
        [ "cart" => Cart.encode session.cart
        , "token" => Encode.string session.token
        ]
