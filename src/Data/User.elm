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
    , projects : List Project
    , samples : List Sample
    }


type alias Project =
    { project_id : Int
    , project_name : String
    , project_code : String
    , project_type : String
    }


type alias Sample =
    { sample_id : Int
    , sample_name : String
    , sample_type : String
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
        |> optional "projects" (Decode.list decoderProject) []
        |> optional "samples" (Decode.list decoderSample) []


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string
        |> optional "project_code" Decode.string ""
        |> optional "project_type" Decode.string ""


decoderSample : Decoder Sample
decoderSample =
    decode Sample
        |> required "sample_id" Decode.int
        |> required "sample_name" Decode.string
        |> optional "sample_type" Decode.string ""


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
