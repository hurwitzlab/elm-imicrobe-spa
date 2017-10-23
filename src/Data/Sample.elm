module Data.Sample exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))



type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    }


type alias Ontology =
    { ontology_id : Int
    , ontology_acc : String
    , label : String
    , ontology_type_id : Int
    , sample_to_ontology : SampleToOntology
    }


type alias Attribute =
    { sample_attr_id : Int
    , sample_attr_type_id : Int
    , sample_id : Int
    , attr_value : String
    , unit : Maybe String
    , sample_attr_type : AttributeType
    }


type alias AttributeType =
    { sample_attr_type_id : Int
    , type_ : String
    , url_template : Maybe String
    , description : Maybe String
    , category : Maybe String
    , sample_attr_type_aliases : List AttributeTypeAlias
    }


type alias AttributeTypeAlias =
    { sample_attr_type_alias_id : Int
    , sample_attr_type_id : Int
    , alias_ : String
    }


type alias Project =
    { project_id : Int
    , project_code : String
    , project_name : String
    , pi : String
    , institution : String
    , project_type : String
    , description : String
    , url : String
    , email : String
    , read_file : String
    , meta_file : String
    , assembly_file : String
    , peptide_file : String
    , read_pep_file : String
    , nt_file : String
    }


type alias Assembly =
    { assembly_id : Int
    , assembly_name : String
    }


type alias CombinedAssembly =
    { combined_assembly_id : Int
    , assembly_name : String
    }


type alias Sample =
    { sample_id : Int
    , project_id : Int
    , combined_assembly_id : Int
    , sample_acc : String
    , sample_name : String
    , sample_type : String
    , sample_description : String
    , comments : String
    , taxon_id : String
    , latitude : String
    , longitude : String
    , url : String
    , project : Project
    , investigators : List Investigator
    , sample_files : List SampleFile2
    , assemblies : List Assembly
    , combined_assemblies : List CombinedAssembly
    , ontologies : List Ontology
    , sample_attrs : List Attribute
    , protein_count : Int
    }


type alias SampleFile =
    { sample_file_id : Int
    , sample_id : Int
    , sample_file_type_id : Int
    , file : String
    , num_seqs : Int
    , num_bp : Int
    , avg_len : Int
    , pct_gc : Int
    , sample_file_type : SampleFileType
    , sample : SampleFileSample
    }


-- FIXME added this because samples reference sample files but sample files reference samples
type alias SampleFile2 =
    { sample_file_id : Int
    , sample_id : Int
    , sample_file_type_id : Int
    , file : String
    , num_seqs : Int
    , num_bp : Int
    , avg_len : Int
    , pct_gc : Int
--    , sample_file_type : SampleFileType
    }


type alias SampleFileSample =
    { sample_id : Int
    , sample_name : String
    }


type alias SampleFileType =
    { sample_file_type_id : Int
    , file_type : String
    }


type alias SampleToOntology =
    { sample_to_ontology_id : Int
    , sample_id : Int
    , ontology_id : Int
    }


type alias SampleUProC =
    { sample_uproc_id : Int
    , sample_id : Int
    , uproc_id : String
    , count : Int
    }



-- SERIALIZATION --


decoderInv : Decoder Investigator
decoderInv =
    decode Investigator
        |> required "investigator_id" Decode.int
        |> required "investigator_name" Decode.string
        |> optional "institution" Decode.string "NA"


decoderOnt : Decoder Ontology
decoderOnt =
    decode Ontology
        |> required "ontology_id" Decode.int
        |> required "ontology_acc" Decode.string
        |> optional "label" Decode.string "NA"
        |> optional "ontology_type_id" Decode.int 0
        |> required "sample_to_ontology" decoderSampleToOntology


decoderAttribute : Decoder Attribute
decoderAttribute =
    decode Attribute
        |> required "sample_attr_id" Decode.int
        |> required "sample_attr_type_id" Decode.int
        |> required "sample_id" Decode.int
        |> required "attr_value" Decode.string
        |> optional "unit" (Decode.nullable Decode.string) Nothing
        |> required "sample_attr_type" decoderAttributeType


decoderAttributeType : Decoder AttributeType
decoderAttributeType =
    decode AttributeType
        |> required "sample_attr_type_id" Decode.int
        |> required "type" Decode.string
        |> optional "url_template" (Decode.nullable Decode.string) Nothing
        |> optional "description" (Decode.nullable Decode.string) Nothing
        |> optional "category" (Decode.nullable Decode.string) Nothing
        |> optional "sample_attr_type_aliases" (Decode.list decoderAttributeTypeAlias) []


decoderAttributeTypeAlias : Decoder AttributeTypeAlias
decoderAttributeTypeAlias =
    decode AttributeTypeAlias
        |> required "sample_attr_type_alias_id" Decode.int
        |> required "sample_attr_type_id" Decode.int
        |> required "alias" Decode.string


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> optional "project_code" Decode.string "NA"
        |> required "project_name" Decode.string
        |> optional "pi" Decode.string "NA"
        |> optional "institution" Decode.string "NA"
        |> optional "project_type" Decode.string "NA"
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string "NA"
        |> optional "email" Decode.string "NA"
        |> optional "read_file" Decode.string "NA"
        |> optional "meta_file" Decode.string "NA"
        |> optional "assembly_file" Decode.string "NA"
        |> optional "peptide_file" Decode.string "NA"
        |> optional "read_pep_file" Decode.string "NA"
        |> optional "nt_file" Decode.string "NA"


decoderAssembly : Decoder Assembly
decoderAssembly =
    decode Assembly
        |> required "assembly_id" Decode.int
        |> required "assembly_name" Decode.string


decoderCombinedAssembly : Decoder CombinedAssembly
decoderCombinedAssembly =
    decode CombinedAssembly
        |> required "combined_assembly_id" Decode.int
        |> optional "assembly_name" Decode.string ""


decoder : Decoder Sample
decoder =
    decode Sample
        |> required "sample_id" Decode.int
        |> required "project_id" Decode.int
        |> optional "combined_assembly_id" Decode.int 0
        |> optional "sample_acc" Decode.string "NA"
        |> required "sample_name" Decode.string
        |> optional "sample_type" Decode.string "NA"
        |> optional "sample_description" Decode.string ""
        |> optional "comments" Decode.string ""
        |> optional "taxon_id" Decode.string ""
        |> optional "latitude" Decode.string ""
        |> optional "longitude" Decode.string ""
        |> optional "url" Decode.string "NA"
        |> required "project" decoderProject
        |> optional "investigators" (Decode.list decoderInv) []
        |> optional "sample_files" (Decode.list decoderSampleFile2) []
        |> optional "assemblies" (Decode.list decoderAssembly) []
        |> optional "combined_assemblies" (Decode.list decoderCombinedAssembly) []
        |> optional "ontologies" (Decode.list decoderOnt) []
        |> optional "sample_attrs" (Decode.list decoderAttribute) []
        |> optional "protein_count" Decode.int 0


decoderSampleFile : Decoder SampleFile
decoderSampleFile =
    decode SampleFile
        |> required "sample_file_id" Decode.int
        |> required "sample_id" Decode.int
        |> optional "sample_file_type_id" Decode.int 0
        |> required "file" Decode.string
        |> optional "num_seqs" Decode.int 0
        |> optional "num_bp" Decode.int 0
        |> optional "avg_len" Decode.int 0
        |> optional "pct_gc" Decode.int 0
        |> required "sample_file_type" decoderSampleFileType
        |> required "sample" decoderSampleFileSample


decoderSampleFile2 : Decoder SampleFile2
decoderSampleFile2 =
    decode SampleFile2
        |> required "sample_file_id" Decode.int
        |> required "sample_id" Decode.int
        |> optional "sample_file_type_id" Decode.int 0
        |> required "file" Decode.string
        |> optional "num_seqs" Decode.int 0
        |> optional "num_bp" Decode.int 0
        |> optional "avg_len" Decode.int 0
        |> optional "pct_gc" Decode.int 0
--        |> required "sample_file_type" decoderSampleFileType


decoderSampleFileType : Decoder SampleFileType
decoderSampleFileType =
    decode SampleFileType
        |> required "sample_file_type_id" Decode.int
        |> required "type" Decode.string


decoderSampleFileSample : Decoder SampleFileSample
decoderSampleFileSample =
    decode SampleFileSample
        |> required "sample_id" Decode.int
        |> required "sample_name" Decode.string


decoderSampleToOntology : Decoder SampleToOntology
decoderSampleToOntology =
    decode SampleToOntology
        |> required "sample_to_ontology_id" Decode.int
        |> required "sample_id" Decode.int
        |> required "ontology_id" Decode.int


decoderSampleUProC : Decoder SampleUProC
decoderSampleUProC =
    decode SampleUProC
        |> required "sample_uproc_id" Decode.int
        |> required "sample_id" Decode.int
        |> required "uproc_id" Decode.string
        |> required "count" Decode.int


encode : Sample -> Value
encode sample =
    Encode.object
        [ "sample_id" => Encode.int sample.sample_id
        , "project_id" => Encode.int sample.project_id
        , "combined_assembly_id" => Encode.int sample.combined_assembly_id
        , "sample_acc" => Encode.string sample.sample_acc
        , "sample_name" => Encode.string sample.sample_name
        , "sample_type" => Encode.string sample.sample_type
        , "sample_description" => Encode.string sample.sample_description
        , "comments" => Encode.string sample.comments
        , "taxon_id" => Encode.string sample.taxon_id
        , "latitude" => Encode.string sample.latitude
        , "longitude" => Encode.string sample.longitude
        , "url" => Encode.string sample.url
--        , "project" => Project.encode sample.project
        ]