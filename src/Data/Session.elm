module Data.Session exposing (Session, decoder, encode, store)

import Data.Cart as Cart exposing (Cart)
import Data.Profile as Profile exposing (Profile)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import Ports


type alias Session =
    { cart : Cart
    , token : String
    , profile : Maybe Profile
    }


decoder : Decoder Session
decoder =
    decode Session
        |> required "cart" Cart.decoder
        |> optional "token" Decode.string ""
        |> optional "profile" (Decode.nullable Profile.decoder) Nothing


encode : Session -> Value
encode session =
    Encode.object
        [ "cart" => Cart.encode session.cart
        , "token" => Encode.string session.token
        , "profile" => EncodeExtra.maybe Profile.encode session.profile
        ]


store : Session -> Cmd msg
store session =
    encode session
        |> Encode.encode 0
        |> Ports.storeSession