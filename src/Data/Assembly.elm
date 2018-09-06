module Data.Assembly exposing (Assembly, Project, decoder, encode)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Assembly =
    { assembly_id : Int
    , project_id : Int
    , assembly_code : String
    , assembly_name : String
    , organism : String
    , pep_file : String
    , nt_file : String
    , cds_file : String
    , description : String
    , sample_id : Int
    , project : Maybe Project
    , sample : Maybe Sample
    , url : String
    }


type alias Project =
    { project_id : Int
    , project_name : String
    }


type alias Sample =
    { sample_id : Int
    , sample_name : String
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
        |> required "sample_name" Decode.string


decoder : Decoder Assembly
decoder =
    decode Assembly
        |> required "assembly_id" Decode.int
        |> optional "project_id" Decode.int 0
        |> required "assembly_code" Decode.string
        |> required "assembly_name" Decode.string
        |> required "organism" Decode.string
        |> optional "pep_file" Decode.string ""
        |> optional "nt_file" Decode.string ""
        |> optional "cds_file" Decode.string ""
        |> optional "description" Decode.string ""
        |> optional "sample_id" Decode.int 0
        |> optional "project" (Decode.nullable decoderProject) Nothing
        |> optional "sample" (Decode.nullable decoderSample) Nothing
        |> optional "url" Decode.string ""


encode : Assembly -> Value
encode assembly =
    Encode.object
        [ "assembly_id" => Encode.int assembly.assembly_id
        , "assembly_name" => Encode.string assembly.assembly_name
        ]
