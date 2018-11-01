module Data.Session exposing (..)

import Data.Cart as Cart exposing (Cart)
import Data.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import Ports


-- IMPORTANT!!!
-- When changing the structure of this record be sure to change the cookieName in config.json to prevent
-- errors when decoding the old cookies (can manifest as infinite login loop)
type alias Session =
    { cart : Cart
    , token : String
    , expiresIn : Maybe Int
    , expiresAt : Maybe Int
    , user : Maybe User
    , url : String
    }


empty : Session
empty =
    { cart = Cart.empty
    , token = ""
    , expiresIn = Nothing
    , expiresAt = Nothing
    , user = Nothing
    , url = ""
    }


expired : Session -> Session
expired session =
    { session | expiresAt = Nothing, expiresIn = Nothing, token = "", user = Nothing }


isLoggedIn : Session -> Bool
isLoggedIn session =
    session.token /= ""


decoder : Decoder Session
decoder =
    decode Session
        |> required "cart" Cart.decoder
        |> optional "token" Decode.string ""
        |> optional "expiresIn" (Decode.nullable Decode.int) Nothing
        |> optional "expiresAt" (Decode.nullable Decode.int) Nothing
        |> optional "user" (Decode.nullable User.decoder) Nothing
        |> optional "url" Decode.string ""


encode : Session -> Value
encode session =
    Encode.object
        [ "cart" => Cart.encode session.cart
        , "token" => Encode.string session.token
        , "expiresIn" => EncodeExtra.maybe Encode.int session.expiresIn
        , "expiresAt" => EncodeExtra.maybe Encode.int session.expiresAt
        , "user" => EncodeExtra.maybe User.encode session.user
        , "url" => Encode.string session.url
        ]


store : Session -> Cmd msg
store session =
    encode session
        |> Encode.encode 0
        |> Ports.storeSession