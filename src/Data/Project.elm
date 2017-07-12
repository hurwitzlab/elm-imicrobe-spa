module Data.Project exposing (Project, decoder, encode)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as JDE
import UrlParser
import Util exposing ((=>))


type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    }


type alias Domain =
    { domain_id : Int
    , domain_name : String
    }



-- stringInt =
--     JD.string |> JD.map String.toInt |> JD.andThen JDE.fromResult


type alias Project =
    { project_id : Int
    , project_name : String
    , project_code : String
    , project_type : String
    , description : String
    , read_file : String
    , meta_file : String
    , assembly_file : String
    , peptide_file : String
    , num_samples : String
    , domains : List Domain
    , investigators : List Investigator
    }



-- SERIALIZATION --


decoderInv : Decoder Investigator
decoderInv =
    decode Investigator
        |> required "investigator_id" Decode.int
        |> required "investigator_name" Decode.string
        |> optional "institution" Decode.string "NA"


decoderDomain : Decoder Domain
decoderDomain =
    decode Domain
        |> required "domain_id" Decode.int
        |> required "domain_name" Decode.string


decoder : Decoder Project
decoder =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string
        |> optional "project_code" Decode.string "NA"
        |> optional "project_type" Decode.string "NA"
        |> optional "description" Decode.string "NA"
        |> optional "read_file" Decode.string "NA"
        |> optional "meta_file" Decode.string "NA"
        |> optional "assembly_file" Decode.string "NA"
        |> optional "peptide_file" Decode.string "NA"
        |> optional "num_samples" Decode.string ""
        |> optional "domains" (Decode.list decoderDomain) []
        |> optional "investigators" (Decode.list decoderInv) []


encode : Project -> Value
encode inv =
    Encode.object
        [ "project_id" => Encode.int inv.project_id
        , "project_name" => Encode.string inv.project_name
        ]
