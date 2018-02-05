module Data.User exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))



type alias User =
    { user_id : Int
    , user_name : String
    , date : String
    , orcid : String
    }


type alias Login =
    { login_id : Int
    , user : User
    , login_date : String
    }



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> required "date" Decode.string
        |> optional "orcid" Decode.string ""


decoderLogin : Decoder Login
decoderLogin =
    decode Login
        |> required "login_id" Decode.int
        |> required "user" decoder
        |> optional "login_date" Decode.string ""


encode : User -> Value
encode user =
    Encode.object
        [ "user_id" => Encode.int user.user_id
        , "user_name" => Encode.string user.user_name
        , "date" => Encode.string user.date
        , "orcid" => Encode.string user.orcid
        ]
