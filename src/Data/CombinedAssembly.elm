module Data.CombinedAssembly exposing (CombinedAssembly, Project, Sample, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias CombinedAssembly =
    { combined_assembly_id : Int
    , assembly_name : String
    , project_id : Int
    , phylum : String
    , class : String
    , family : String
    , genus : String
    , species : String
    , strain : String
    , pcr_amp : String
    , anno_file : String
    , pep_file : String
    , nt_file : String
    , cds_file : String
    , project : Maybe Project
    , samples : List Sample
    }


type alias Project =
    { project_id : Int
    , project_name : String
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
    , latitude : String
    , longitude : String
    }



-- SERIALIZATION --


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string


decoderSample : Decoder Sample
decoderSample =
    decode Sample
        |> required "sample_id" Decode.int
        |> optional "sample_acc" Decode.string ""
        |> optional "sample_name" Decode.string ""
        |> optional "sample_type" Decode.string "NA"
        |> optional "sample_description" Decode.string ""
        |> optional "comments" Decode.string ""
        |> optional "taxon_id" Decode.string ""
        |> optional "url" Decode.string "NA"
        |> optional "latitude" Decode.string ""
        |> optional "longitude" Decode.string ""


decoder : Decoder CombinedAssembly
decoder =
    decode CombinedAssembly
        |> required "combined_assembly_id" Decode.int
        |> optional "assembly_name" Decode.string ""
        |> optional "project_id" Decode.int 0
        |> optional "phylum" Decode.string ""
        |> optional "class" Decode.string ""
        |> optional "family" Decode.string ""
        |> optional "genus" Decode.string ""
        |> optional "species" Decode.string ""
        |> optional "strain" Decode.string ""
        |> optional "pcr_amp" Decode.string ""
        |> optional "annotations_file" Decode.string ""
        |> optional "peptides_file" Decode.string ""
        |> optional "nucleotides_file" Decode.string ""
        |> optional "cds_file" Decode.string ""
        |> optional "project" (Decode.nullable decoderProject) Nothing
        |> optional "samples" (Decode.list decoderSample) []


encode : CombinedAssembly -> Value
encode assembly =
    Encode.object
        [ "combined_assembly_id" => Encode.int assembly.combined_assembly_id
        ]