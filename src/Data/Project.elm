module Data.Project exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))


type alias Domain =
    { domain_id : Int
    , domain_name : String
    }


type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    }


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
    , publications : List Publication
    , samples : List Sample
    , assemblies : List Assembly
    , combined_assemblies : List CombinedAssembly
    , project_groups : List ProjectGroup
    }


type alias ProjectGroup =
    { project_group_id : Int
    , group_name : String
    , project_to_project_group : ProjectToGroup
    }


type alias ProjectToGroup =
    { project_to_project_group_id : Int
    , project_id : Int
    , project_group_id : Int
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
    }


type alias Assembly =
    { assembly_id : Int
    , assembly_name : String
    }


type alias CombinedAssembly =
    { combined_assembly_id : Int
    , assembly_name : String
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
        |> optional "read_file" Decode.string "NA"
        |> optional "meta_file" Decode.string "NA"
        |> optional "assembly_file" Decode.string "NA"
        |> optional "peptide_file" Decode.string "NA"
        |> optional "num_samples" Decode.string ""
        |> optional "domains" (Decode.list decoderDomain) []
        |> optional "investigators" (Decode.list decoderInv) []
        |> optional "publications" (Decode.list decoderPub) []
        |> optional "samples" (Decode.list decoderSample) []
        |> optional "assemblies" (Decode.list decoderAssembly) []
        |> optional "combined_assemblies" (Decode.list decoderCombinedAssembly) []
        |> optional "project_groups" (Decode.list decoderProjectGroup) []


decoderProjectGroup : Decoder ProjectGroup
decoderProjectGroup =
    decode ProjectGroup
        |> required "project_group_id" Decode.int
        |> required "group_name" Decode.string
        |> required "project_to_project_group" decoderProjectToGroup


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


decoderProjectToGroup : Decoder ProjectToGroup
decoderProjectToGroup =
    decode ProjectToGroup
        |> required "project_to_project_group_id" Decode.int
        |> required "project_id" Decode.int
        |> required "project_group_id" Decode.int


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


encode : Project -> Value
encode inv =
    Encode.object
        [ "project_id" => Encode.int inv.project_id
        , "project_name" => Encode.string inv.project_name
        ]
