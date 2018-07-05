module Data.SampleGroup exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)



type alias SampleGroup =
    { sample_group_id : Int
    , group_name : String
    , description : String
    , url : String
    , user_id : Int
    , samples : List Sample
    }


type alias Sample =
    { sample_id : Int
    , sample_name : String
    , project : Project
    }


type alias Project =
    { project_id : Int
    , project_name : String
    }


type alias User =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    }



-- SERIALIZATION --


decoder : Decoder SampleGroup
decoder =
    decode SampleGroup
        |> required "sample_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string "NA"
        |> required "user_id" Decode.int
        |> optional "samples" (Decode.list decoderSample) []


decoderSample : Decoder Sample
decoderSample =
    decode Sample
        |> required "sample_id" Decode.int
        |> required "sample_name" Decode.string
        |> required "project" decoderProject


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string


decoderUser : Decoder User
decoderUser =
    decode User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
