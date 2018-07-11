module Data.User exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))



type alias User =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , email : String
    , date : String
    , orcid : String
    , projects : List Project
    , project_groups : List ProjectGroup
    , log : List LogEntry
    }


type alias LogEntry =
    { id : String
--    , type_ : String
    , title : String
    , date : String
--    , user_id : Int
--    , user_name : String
    , url : String
    }


type alias Project =
    { project_id : Int
    , project_name : String
    , project_code : String
    , project_type : String
    , url : String
    , investigators : List Investigator
    , publications : List Publication
    , samples : List Sample
    , sample_count : Int
    , users : List User2
    }


type alias User2 =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , email : String
    , permconn : ProjectToUser
    }


type alias ProjectToUser =
    { permission : String
    }


type alias ProjectGroup =
    { project_group_id : Int
    , group_name : String
    , description : String
    , url : String
    , projects : List Project
    , users : List User3
    }


type alias User3 =
    { user_id : Int
    , user_name : String
    , first_name : String
    , last_name : String
    , permconn : ProjectGroupToUser
    }


type alias ProjectGroupToUser =
    { permission : String
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
    , sample_name : String
    , sample_acc : String
    , sample_type : String
    , project : Project2
    , sample_files : List SampleFile
    , sample_file_count : Int
    }


type alias Project2 =
    { project_id : Int
    , project_name : String
    }


type alias SampleFile =
    { sample_file_id : Int
    , sample_id : Int
    }


type alias Login =
    { login_id : Int
    , user : User
    , login_date : String
    }



-- SERIALIZATION --


decoder : Decoder User
decoder =
    decode User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> optional "email" Decode.string ""
        |> required "date" Decode.string
        |> optional "orcid" Decode.string ""
        |> optional "projects" (Decode.list decoderProject) []
        |> optional "project_groups" (Decode.list decoderProjectGroup) []
        |> optional "log" (Decode.list decoderLogEntry) []


decoderUser2 : Decoder User2
decoderUser2 =
    decode User2
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> optional "email" Decode.string ""
        |> required "project_to_user" decoderProjectToUser


decoderProjectToUser : Decoder ProjectToUser
decoderProjectToUser =
    decode ProjectToUser
        |> required "permission" Decode.string


decoderProjectGroup : Decoder ProjectGroup
decoderProjectGroup =
    decode ProjectGroup
        |> required "project_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string "NA"
        |> optional "projects" (Decode.list decoderProject) []
        |> optional "users" (Decode.list decoderUser3) []


decoderUser3 : Decoder User3
decoderUser3 =
    decode User3
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> required "project_group_to_user" decoderProjectGroupToUser


decoderProjectGroupToUser : Decoder ProjectGroupToUser
decoderProjectGroupToUser =
    decode ProjectGroupToUser
        |> required "permission" Decode.string


decoderLogEntry : Decoder LogEntry
decoderLogEntry =
    decode LogEntry
        |> required "_id" Decode.string
--        |> required "type" Decode.string
        |> required "title" Decode.string
        |> required "date" Decode.string
--        |> required "user_id" Decode.int
--        |> required "user_name" Decode.string
        |> required "url" Decode.string


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string
        |> optional "project_code" Decode.string ""
        |> optional "project_type" Decode.string ""
        |> optional "url" Decode.string ""
        |> optional "investigators" (Decode.list decoderInvestigator) []
        |> optional "publications" (Decode.list decoderPublication) []
        |> optional "samples" (Decode.list decoderSample) []
        |> optional "sample_count" Decode.int 0
        |> optional "users" (Decode.list decoderUser2) []


decoderInvestigator : Decoder Investigator
decoderInvestigator =
    decode Investigator
        |> required "investigator_id" Decode.int
        |> required "investigator_name" Decode.string
        |> optional "institution" Decode.string "NA"


decoderPublication : Decoder Publication
decoderPublication =
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
        |> required "sample_name" Decode.string
        |> optional "sample_acc" Decode.string ""
        |> optional "sample_type" Decode.string ""
        |> required "project" decoderProject2
        |> optional "sample_files" (Decode.list decoderSampleFile) []
        |> optional "sample_file_count" Decode.int 0


decoderProject2 : Decoder Project2
decoderProject2 =
    decode Project2
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string


decoderSampleFile : Decoder SampleFile
decoderSampleFile =
    decode SampleFile
        |> required "sample_file_id" Decode.int
        |> required "sample_id" Decode.int


decoderLogin : Decoder Login
decoderLogin =
    decode Login
        |> required "login_id" Decode.int
        |> required "user" decoder
        |> optional "login_date" Decode.string ""


encode : User -> Value
encode user =
    Encode.object
        [ "user_id" => Encode.int user.user_id
        , "user_name" => Encode.string user.user_name
        , "first_name" => Encode.string user.first_name
        , "last_name" => Encode.string user.last_name
        , "email" => Encode.string user.email
        , "date" => Encode.string user.date
        , "orcid" => Encode.string user.orcid
        ]
