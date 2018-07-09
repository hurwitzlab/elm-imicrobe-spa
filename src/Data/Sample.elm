module Data.Sample exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Util exposing ((=>))



type alias Sample =
    { sample_id : Int
    , project_id : Int
    , sample_acc : String
    , sample_name : String
    , sample_type : String
    , sample_description : String
    , url : String
    , project : Project
    , investigators : List Investigator
    , sample_files : List SampleFile2
    , sample_file_count : Int
    , assemblies : List Assembly
    , combined_assemblies : List CombinedAssembly
    , ontologies : List Ontology
    , sample_attrs : List Attribute
    , protein_count : Int
    , centrifuge_count : Int
    , available_file_types : List SampleFileType
    , available_types : List String
    }


type alias SampleGroup =
    { sample_group_id : Int
    , group_name : String
    , description : String
    , url : String
    , user_id : Int
    , samples : List Sample
    }


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
    }


type alias Attribute =
    { sample_attr_id : Int
    , sample_attr_type_id : Int
    , sample_id : Int
    , attr_value : String
    , sample_attr_type : AttributeType
    }


type alias AttributeType =
    { sample_attr_type_id : Int
    , sample_attr_type_category_id : Int
    , type_ : String
    , url_template : Maybe String
    , description : Maybe String
    , units : String
    , category : AttributeTypeCategory
    , sample_attr_type_aliases : List AttributeTypeAlias
    }


type alias AttributeTypeCategory =
    { sample_attr_type_category_id : Int
    , category : String
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
    , project_type : String
    , description : String
    , users : List User
    , project_groups : List ProjectGroup
    , private : Int
    }


type alias ProjectGroup =
    { project_group_id : Int
    , group_name : String
    , users : List User2
    }


type alias User =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , permconn : ProjectToUser
    }


type alias ProjectToUser =
    { permission : String
    }


type alias User2 =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , permconn : ProjectGroupToUser
    }


type alias ProjectGroupToUser =
    { permission : String
    }


type alias Assembly =
    { assembly_id : Int
    , assembly_name : String
    }


type alias CombinedAssembly =
    { combined_assembly_id : Int
    , assembly_name : String
    }


type alias SampleFile =
    { sample_file_id : Int
    , sample_id : Int
    , sample_file_type_id : Int
    , file : String
    , sample_file_type : SampleFileType
    , sample : SampleFileSample
    }


-- FIXME added this because samples reference sample files but sample files reference samples
type alias SampleFile2 =
    { sample_file_id : Int
    , sample_id : Int
    , sample_file_type_id : Int
    , file : String
    , sample_file_type : SampleFileType
    }


type alias SampleFileSample =
    { sample_id : Int
    , sample_name : String
    }


type alias SampleFileType =
    { sample_file_type_id : Int
    , file_type : String
    }


type alias Proteins =
    { pfam : List UProC_PFAM
    , kegg : List UProC_KEGG
    }


type alias UProC_PFAM =
    { sample_to_uproc_id : Int
    , sample_id : Int
    , uproc_id : Int
    , read_count : Int
    , annotation : PFAMAnnotation
    }


type alias UProC_KEGG =
    { uproc_kegg_result_id : Int
    , sample_id : Int
    , kegg_annotation_id : String
    , read_count : Int
    , annotation : KEGGAnnotation
    }


type alias PFAMAnnotation =
    { accession : String
    , identifier : String
    , name : String
    , description : String
    }


type alias KEGGAnnotation =
    { name : String
    , definition : String
    , pathway : String
    , module_ : String
    }


type alias PFAMProtein =
    { uproc_id : Int
    , accession : String
    , identifier : String
    , name : String
    , description : String
    , uproc_pfam_results : List PFAMResult
    }


type alias PFAMResult =
    { sample_to_uproc_id : Int
    , read_count : Int
    , sample : Sample
    }


type alias KEGGProtein =
    { kegg_annotation_id : String
    , name : String
    , definition : String
    , pathway : String
    , module_ : String
    , uproc_kegg_results : List KEGGResult
    }


type alias KEGGResult =
    { uproc_kegg_result_id : Int
    , read_count : Int
    , sample : Sample
    }


-- FIXME centrifuge-related types below are a mess
type alias Centrifuge =
    { centrifuge_id : Int
    , tax_id : Int
    , name : String
    }


type alias Centrifuge2 =
    { centrifuge_id : Int
    , tax_id : Int
    , name : String
    , samples : List CentrifugeSample
    }


type alias SampleToCentrifuge =
    { sample_to_centrifuge_id : Int
    , num_reads : Int
    , num_unique_reads : Int
    , abundance : Float
    , centrifuge : Centrifuge
    }

type alias SampleToCentrifuge2 =
    { sample_to_centrifuge_id : Int
    , num_reads : Int
    , num_unique_reads : Int
    , abundance : Float
    }


type alias CentrifugeSample =
    { sample_id : Int
    , sample_name : String
    , project_id : Int
    , project : Project
    , sample_to_centrifuge : SampleToCentrifuge2
    }


type alias SearchResult =
    { attributes: Dict String JsonType
    , users: List User
    }


type alias SearchParamsResult =
    { param : String
    , values : List JsonType
    , units : String
    }


type JsonType
    = StrType String
    | IntType Int
    | FloatType Float
    | ValueType Decode.Value



-- SERIALIZATION --


decoder : Decoder Sample
decoder =
    decode Sample
        |> required "sample_id" Decode.int
        |> required "project_id" Decode.int
        |> optional "sample_acc" Decode.string "NA"
        |> required "sample_name" Decode.string
        |> optional "sample_type" Decode.string "NA"
        |> optional "sample_description" Decode.string ""
        |> optional "url" Decode.string "NA"
        |> required "project" decoderProject
        |> optional "investigators" (Decode.list decoderInv) []
        |> optional "sample_files" (Decode.list decoderSampleFile2) []
        |> optional "sample_file_count" Decode.int 0
        |> optional "assemblies" (Decode.list decoderAssembly) []
        |> optional "combined_assemblies" (Decode.list decoderCombinedAssembly) []
        |> optional "ontologies" (Decode.list decoderOnt) []
        |> optional "sample_attrs" (Decode.list decoderAttribute) []
        |> optional "protein_count" Decode.int 0
        |> optional "centrifuge_count" Decode.int 0
        |> optional "available_file_types" (Decode.list decoderSampleFileType) []
        |> optional "available_types" (Decode.list Decode.string) []


decoderSampleGroup : Decoder SampleGroup
decoderSampleGroup =
    decode SampleGroup
        |> required "sample_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string "NA"
        |> required "user_id" Decode.int
        |> optional "samples" (Decode.list decoder) []


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


decoderAttribute : Decoder Attribute
decoderAttribute =
    decode Attribute
        |> required "sample_attr_id" Decode.int
        |> required "sample_attr_type_id" Decode.int
        |> required "sample_id" Decode.int
        |> required "attr_value" Decode.string
        |> required "sample_attr_type" decoderAttributeType


decoderAttributeType : Decoder AttributeType
decoderAttributeType =
    decode AttributeType
        |> required "sample_attr_type_id" Decode.int
        |> optional "sample_attr_type_category_id" Decode.int 0
        |> required "type" Decode.string
        |> optional "url_template" (Decode.nullable Decode.string) Nothing
        |> optional "description" (Decode.nullable Decode.string) Nothing
        |> optional "units" Decode.string ""
        |> optional "category" decoderAttributeTypeCategory (AttributeTypeCategory 0 "")
        |> optional "sample_attr_type_aliases" (Decode.list decoderAttributeTypeAlias) []


decoderAttributeTypeCategory : Decoder AttributeTypeCategory
decoderAttributeTypeCategory =
    decode AttributeTypeCategory
        |> required "sample_attr_type_category_id" Decode.int
        |> required "category" Decode.string


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
        |> optional "project_type" Decode.string "NA"
        |> optional "description" Decode.string "NA"
        |> optional "users" (Decode.list decoderUser) []
        |> optional "project_groups" (Decode.list decoderProjectGroup) []
        |> optional "private" Decode.int 0


decoderProjectGroup : Decoder ProjectGroup
decoderProjectGroup =
    decode ProjectGroup
        |> required "project_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "users" (Decode.list decoderUser2) []


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


decoderSampleFile : Decoder SampleFile
decoderSampleFile =
    decode SampleFile
        |> required "sample_file_id" Decode.int
        |> required "sample_id" Decode.int
        |> optional "sample_file_type_id" Decode.int 0
        |> required "file" Decode.string
        |> required "sample_file_type" decoderSampleFileType
        |> required "sample" decoderSampleFileSample


decoderSampleFile2 : Decoder SampleFile2
decoderSampleFile2 =
    decode SampleFile2
        |> required "sample_file_id" Decode.int
        |> required "sample_id" Decode.int
        |> optional "sample_file_type_id" Decode.int 0
        |> required "file" Decode.string
        |> required "sample_file_type" decoderSampleFileType


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


decoderProteins : Decoder Proteins
decoderProteins =
    decode Proteins
        |> required "pfam" (Decode.list decoderUProC_PFAM)
        |> required "kegg" (Decode.list decoderUProC_KEGG)


decoderUProC_PFAM : Decoder UProC_PFAM
decoderUProC_PFAM =
    decode UProC_PFAM
        |> required "sample_to_uproc_id" Decode.int
        |> required "sample_id" Decode.int
        |> required "uproc_id" Decode.int
        |> required "read_count" Decode.int
        |> required "pfam_annotation" decoderPFAMAnnotation


decoderUProC_KEGG : Decoder UProC_KEGG
decoderUProC_KEGG =
    decode UProC_KEGG
        |> required "uproc_kegg_result_id" Decode.int
        |> required "sample_id" Decode.int
        |> required "kegg_annotation_id" Decode.string
        |> required "read_count" Decode.int
        |> required "kegg_annotation" decoderKEGGAnnotation


decoderPFAMAnnotation : Decoder PFAMAnnotation
decoderPFAMAnnotation =
    decode PFAMAnnotation
        |> required "accession" Decode.string
        |> required "identifier" Decode.string
        |> required "name" Decode.string
        |> required "description" Decode.string


decoderKEGGAnnotation : Decoder KEGGAnnotation
decoderKEGGAnnotation =
    decode KEGGAnnotation
        |> required "name" Decode.string
        |> required "definition" Decode.string
        |> required "pathway" Decode.string
        |> required "module" Decode.string


decoderPFAMProtein : Decoder PFAMProtein
decoderPFAMProtein =
    decode PFAMProtein
        |> required "uproc_id" Decode.int
        |> required "accession" Decode.string
        |> required "identifier" Decode.string
        |> required "name" Decode.string
        |> required "description" Decode.string
        |> optional "uproc_pfam_results" (Decode.list decoderPFAMResult) []


decoderPFAMResult : Decoder PFAMResult
decoderPFAMResult =
    decode PFAMResult
        |> required "sample_to_uproc_id" Decode.int
        |> required "read_count" Decode.int
        |> required "sample" decoder


decoderKEGGProtein : Decoder KEGGProtein
decoderKEGGProtein =
    decode KEGGProtein
        |> required "kegg_annotation_id" Decode.string
        |> required "name" Decode.string
        |> required "definition" Decode.string
        |> required "pathway" Decode.string
        |> required "module" Decode.string
        |> optional "uproc_kegg_results" (Decode.list decoderKEGGResult) []


decoderKEGGResult : Decoder KEGGResult
decoderKEGGResult =
    decode KEGGResult
        |> required "uproc_kegg_result_id" Decode.int
        |> required "read_count" Decode.int
        |> required "sample" decoder


decoderSampleToCentrifuge : Decoder SampleToCentrifuge
decoderSampleToCentrifuge =
    decode SampleToCentrifuge
        |> required "sample_to_centrifuge_id" Decode.int
        |> required "num_reads" Decode.int
        |> required "num_unique_reads" Decode.int
        |> required "abundance" Decode.float
        |> required "centrifuge" decoderCentrifuge


decoderSampleToCentrifuge2 : Decoder SampleToCentrifuge2
decoderSampleToCentrifuge2 =
    decode SampleToCentrifuge2
        |> required "sample_to_centrifuge_id" Decode.int
        |> required "num_reads" Decode.int
        |> required "num_unique_reads" Decode.int
        |> required "abundance" Decode.float


decoderCentrifuge : Decoder Centrifuge
decoderCentrifuge =
    decode Centrifuge
        |> required "centrifuge_id" Decode.int
        |> required "tax_id" Decode.int
        |> required "name" Decode.string


decoderCentrifuge2 : Decoder Centrifuge2
decoderCentrifuge2 =
    decode Centrifuge2
        |> required "centrifuge_id" Decode.int
        |> required "tax_id" Decode.int
        |> required "name" Decode.string
        |> optional "samples" (Decode.list decoderCentrifugeSample) []


decoderCentrifugeSample : Decoder CentrifugeSample
decoderCentrifugeSample =
    decode CentrifugeSample
        |> required "sample_id" Decode.int
        |> required "sample_name" Decode.string
        |> required "project_id" Decode.int
        |> required "project" decoderProject
        |> required "sample_to_centrifuge" decoderSampleToCentrifuge2


decoderUser : Decoder User
decoderUser =
    decode User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> optional "first_name" Decode.string ""
        |> optional "last_name" Decode.string ""
        |> required "project_to_user" decoderProjectToUser


decoderProjectToUser : Decoder ProjectToUser
decoderProjectToUser =
    decode ProjectToUser
        |> required "permission" Decode.string


decoderUser2 : Decoder User2
decoderUser2 =
    decode User2
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> optional "first_name" Decode.string ""
        |> optional "last_name" Decode.string ""
        |> required "project_group_to_user" decoderProjectGroupToUser


decoderProjectGroupToUser : Decoder ProjectGroupToUser
decoderProjectGroupToUser =
    decode ProjectGroupToUser
        |> required "permission" Decode.string


oneOfJsonType : Decoder JsonType
oneOfJsonType =
    [ Decode.string
        |> Decode.map StrType
    , Decode.int
        |> Decode.map IntType
    , Decode.float
        |> Decode.map FloatType
    , Decode.value
        |> Decode.map ValueType
    ]
    |> Decode.oneOf


decoderSearchResult : Decoder SearchResult
decoderSearchResult =
    decode SearchResult
        |> required "attributes" (Decode.dict oneOfJsonType)
        |> optional "users" (Decode.list decoderUser) []


decoderSearchParamsResult : Decoder SearchParamsResult
decoderSearchParamsResult =
    decode SearchParamsResult
        |> required "param" Decode.string
        |> required "values" (Decode.list oneOfJsonType)
        |> required "units" Decode.string


encode : Sample -> Value
encode sample =
    Encode.object
        [ "sample_id" => Encode.int sample.sample_id
        , "project_id" => Encode.int sample.project_id
        , "sample_acc" => Encode.string sample.sample_acc
        , "sample_name" => Encode.string sample.sample_name
        , "sample_type" => Encode.string sample.sample_type
        , "sample_description" => Encode.string sample.sample_description
        , "url" => Encode.string sample.url
--        , "project" => Project.encode sample.project
        ]