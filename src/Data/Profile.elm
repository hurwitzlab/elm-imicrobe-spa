module Data.Profile exposing (Profile, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Profile =
    { email : String
    , first_name : String
    , last_name : String
    , username : String
    }



-- SERIALIZATION --


decoder : Decoder Profile
decoder =
    decode Profile
        |> required "email" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> required "username" Decode.string


encode : Profile -> Value
encode profile =
    Encode.object
        [ "email" => Encode.string profile.email
        , "first_name" => Encode.string profile.first_name
        , "last_name" => Encode.string profile.last_name
        , "username" => Encode.string profile.username
        ]
