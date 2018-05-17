module Data.Project exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))



type alias Project =
    { project_id : Int
    , project_name : String
    , project_code : String
    , project_type : String
    , description : String
    , url : String
    , read_file : String
    , meta_file : String
    , assembly_file : String
    , peptide_file : String
    , num_samples : String
    , domains : List Domain
    , investigators : List Investigator
    , publications : List Publication
    , samples : List Sample
    , sample_count : Int
    , project_groups : List ProjectGroup
    , available_types : List String
    , available_domains : List Domain
    , available_groups : List ProjectGroup
    , assembly_count : Int
    , combined_assembly_count : Int
    , users : List User
    , private : Int
    }


type alias ProjectGroup =
    { project_group_id : Int
    , group_name : String
    , user_count : Int
    , users : List User
    }


type alias Domain =
    { domain_id : Int
    , domain_name : String
    }


type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    }


type alias Publication =
    { publication_id : Int
    , title : String
    , pub_code : String
    , doi : String
    , author : String
    , pubmed_id : Int
    , journal : String
    , pub_date : String
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
    , sample_files : List SampleFile
    }


type alias SampleFile =
    { sample_file_id : Int
    , sample_id : Int
    , sample_file_type_id : Int
    , file : String
    }


type alias Assembly =
    { assembly_id : Int
    , assembly_name : String
    }


type alias CombinedAssembly =
    { combined_assembly_id : Int
    , assembly_name : String
    }


type alias User =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , permission : String
    }



-- SERIALIZATION --


decoder : Decoder Project
decoder =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string
        |> optional "project_code" Decode.string "NA"
        |> optional "project_type" Decode.string "NA"
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string ""
        |> optional "read_file" Decode.string "NA"
        |> optional "meta_file" Decode.string "NA"
        |> optional "assembly_file" Decode.string "NA"
        |> optional "peptide_file" Decode.string "NA"
        |> optional "num_samples" Decode.string ""
        |> optional "domains" (Decode.list decoderDomain) []
        |> optional "investigators" (Decode.list decoderInv) []
        |> optional "publications" (Decode.list decoderPub) []
        |> optional "samples" (Decode.list decoderSample) []
        |> optional "sample_count" Decode.int 0
        |> optional "project_groups" (Decode.list decoderProjectGroup) []
        |> optional "available_types" (Decode.list Decode.string) []
        |> optional "available_domains" (Decode.list decoderDomain) []
        |> optional "available_groups" (Decode.list decoderProjectGroup) []
        |> optional "assembly_count" Decode.int 0
        |> optional "combined_assembly_count" Decode.int 0
        |> optional "users" (Decode.list decoderUser) []
        |> optional "private" Decode.int 0


decoderProjectGroup : Decoder ProjectGroup
decoderProjectGroup =
    decode ProjectGroup
        |> required "project_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "user_count" Decode.int 0
        |> optional "users" (Decode.list decoderUser) []


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


decoderPub : Decoder Publication
decoderPub =
    decode Publication
        |> required "publication_id" Decode.int
        |> required "title" Decode.string
        |> optional "pub_code" Decode.string "NA"
        |> optional "doi" Decode.string "NA"
        |> optional "author" Decode.string "NA"
        |> optional "pubmed_id" Decode.int 0
        |> optional "journal" Decode.string "NA"
        |> optional "pub_date" Decode.string "NA"


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
        |> optional "sample_files" (Decode.list decoderSampleFile) []


decoderSampleFile : Decoder SampleFile
decoderSampleFile =
    decode SampleFile
        |> required "sample_file_id" Decode.int
        |> required "sample_id" Decode.int
        |> optional "sample_file_type_id" Decode.int 0
        |> required "file" Decode.string


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


decoderUser : Decoder User
decoderUser =
    decode User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> optional "first_name" Decode.string ""
        |> optional "last_name" Decode.string ""
        |> optional "permission" Decode.string ""


encode : Project -> Value
encode project =
    Encode.object
        [ "project_id" => Encode.int project.project_id
        , "project_name" => Encode.string project.project_name
        ]


encodeDomain : Domain -> Value
encodeDomain domain =
    Encode.object
        [ "domain_id" => Encode.int domain.domain_id
        , "domain_name" => Encode.string domain.domain_name
        ]


encodeInvestigator : Investigator -> Value
encodeInvestigator investigator =
    Encode.object
        [ "investigator_id" => Encode.int investigator.investigator_id
        , "investigator_name" => Encode.string investigator.investigator_name
        ]


encodeProjectGroup : ProjectGroup -> Value
encodeProjectGroup group =
    Encode.object
        [ "project_group_id" => Encode.int group.project_group_id
        , "group_name" => Encode.string group.group_name
        ]
