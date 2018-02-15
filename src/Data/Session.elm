module Data.Session exposing (Session, empty, decoder, encode, store)

import Data.Cart as Cart exposing (Cart)
import Data.Agave as Agave exposing (Profile)
import Data.User as User exposing (User)
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
    , user : Maybe User
    , profile : Maybe Profile
    , url : String
    }


empty : Session
empty =
    Session (Cart Set.empty) "" Nothing Nothing ""


decoder : Decoder Session
decoder =
    decode Session
        |> required "cart" Cart.decoder
        |> optional "token" Decode.string ""
        |> optional "user" (Decode.nullable User.decoder) Nothing
        |> optional "profile" (Decode.nullable Agave.decoderProfile) Nothing
        |> optional "url" Decode.string ""


encode : Session -> Value
encode session =
    Encode.object
        [ "cart" => Cart.encode session.cart
        , "token" => Encode.string session.token
        , "user" => EncodeExtra.maybe User.encode session.user
        , "profile" => EncodeExtra.maybe Agave.encodeProfile session.profile
        , "url" => Encode.string session.url
        ]


store : Session -> Cmd msg
store session =
    encode session
        |> Encode.encode 0
        |> Ports.storeSession