module Data.Session exposing (Session, empty, decoder, encode, store)

import Data.Cart as Cart exposing (Cart)
import Data.Profile as Profile exposing (Profile)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import Ports
import Set



type alias Session =
    { cart : Cart
    , token : String
    , user_id : Maybe Int
    , profile : Maybe Profile
    }


empty : Session
empty =
    Session (Cart Set.empty) "" Nothing Nothing


decoder : Decoder Session
decoder =
    decode Session
        |> required "cart" Cart.decoder
        |> optional "token" Decode.string ""
        |> optional "user_id" (Decode.nullable Decode.int) Nothing
        |> optional "profile" (Decode.nullable Profile.decoder) Nothing


encode : Session -> Value
encode session =
    Encode.object
        [ "cart" => Cart.encode session.cart
        , "token" => Encode.string session.token
        , "user_id" => EncodeExtra.maybe Encode.int session.user_id
        , "profile" => EncodeExtra.maybe Profile.encode session.profile
        ]


store : Session -> Cmd msg
store session =
    encode session
        |> Encode.encode 0
        |> Ports.storeSession