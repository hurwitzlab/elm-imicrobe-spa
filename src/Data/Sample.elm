module Data.Sample exposing (File, Investigator, Ontology, Sample, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    }


type alias File =
    { sample_file_id : Int
    , file : String
    , num_seqs : Int
    , num_bp : Int
    , avg_len : Int
    , sample_file_type_id : Int
    , file_type : String
    }


type alias Ontology =
    { ontology_acc : String
    , label : String
    , ontology_type : String
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
    , investigators : List Investigator
    , files : List File
    , ontologies : List Ontology
    }



-- SERIALIZATION --


decoderInv : Decoder Investigator
decoderInv =
    decode Investigator
        |> required "investigator_id" Decode.int
        |> required "investigator_name" Decode.string
        |> optional "institution" Decode.string "NA"


decoderFile : Decoder File
decoderFile =
    decode File
        |> required "sample_file_id" Decode.int
        |> required "file" Decode.string
        |> optional "num_seqs" Decode.int 0
        |> optional "num_bp" Decode.int 0
        |> optional "avg_len" Decode.int 0
        |> optional "sample_file_type" Decode.int 0
        |> optional "file_type" Decode.string ""


decoderOnt : Decoder Ontology
decoderOnt =
    decode Ontology
        |> required "ontology_acc" Decode.string
        |> optional "label" Decode.string "NA"
        |> optional "ontology_type" Decode.string "NA"


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
        |> optional "investigators" (Decode.list decoderInv) []
        |> optional "files" (Decode.list decoderFile) []
        |> optional "ontologies" (Decode.list decoderOnt) []



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
