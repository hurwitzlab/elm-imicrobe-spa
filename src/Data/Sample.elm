module Data.Sample exposing (Sample, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Sample =
    { sample_id : Int
    , project_name : String
    , sample_name : String
    , sample_type : String
    }



-- SERIALIZATION --


decoder : Decoder Sample
decoder =
    decode Sample
        |> required "sample_id" Decode.int
        |> required "project_name" Decode.string
        |> required "sample_name" Decode.string
        |> optional "sample_type" Decode.string "NA"


encode : Sample -> Value
encode sample =
    Encode.object
        [ "sample_id" => Encode.int sample.sample_id
        , "project_name" => Encode.string sample.project_name
        , "sample_name" => Encode.string sample.sample_name
        , "sample_type" => Encode.string sample.sample_type
        ]
