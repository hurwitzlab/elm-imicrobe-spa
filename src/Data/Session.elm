module Data.Session exposing (Session, decoder, encode, store)

import Data.Cart as Cart exposing (Cart)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))
import Ports


type alias Session =
    { cart : Cart
    , username : String
    , token : String
    }


decoder : Decoder Session
decoder =
    decode Session
        |> required "cart" Cart.decoder
        |> optional "username" Decode.string ""
        |> optional "token" Decode.string ""


encode : Session -> Value
encode session =
    Encode.object
        [ "cart" => Cart.encode session.cart
        , "username" => Encode.string session.username
        , "token" => Encode.string session.token
        ]


store : Session -> Cmd msg
store session =
    encode session
        |> Encode.encode 0
        |> Ports.storeSession