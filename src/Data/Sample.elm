module Data.Sample exposing (Investigator, Sample, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    }


type alias Sample =
    { sample_id : Int
    , sample_acc : String
    , sample_name : String
    , sample_type : String
    , sample_description : String
    , comments : String
    , taxon_id : String
    , url : String
    , latitude : Float
    , longitude : Float
    , project_id : Int
    , project_name : String
    }



-- SERIALIZATION --


decoder : Decoder Sample
decoder =
    decode Sample
        |> required "sample_id" Decode.int
        |> required "sample_acc" Decode.string
        |> required "sample_name" Decode.string
        |> optional "sample_type" Decode.string "NA"
        |> optional "sample_description" Decode.string ""
        |> optional "comments" Decode.string ""
        |> optional "taxon_id" Decode.string ""
        |> optional "url" Decode.string "NA"
        |> optional "latitude" Decode.float 0.0
        |> optional "longitude" Decode.float 0.0
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string



{--
encode : Sample -> Value
encode sample =
    Encode.object
        [ "sample_id" => Encode.int sample.sample_id
        , "project_id" => Encode.int sample.project_id
        , "project_name" => Encode.string sample.project_name
        , "sample_name" => Encode.string sample.sample_name
        , "sample_type" => Encode.string sample.sample_type
        , "domain_name" => Encode.string sample.domain_name
        ]
        --}
