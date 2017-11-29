module Data.Publication exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)



type alias Publication =
    { publication_id : Int
    , project_id : Int
    , pub_code : String
    , doi : String
    , author : String
    , title : String
    , pubmed_id : Int
    , pub_date : String
    , project : Maybe Project
    , project_files : List ProjectFile
    }


type alias Project =
    { project_id : Int
    , project_name : String
    }


type alias ProjectFile =
    { profile_file_id : Int
    , project_id : Int
    , project_file_type : ProjectFileType
    , file : String
    , description : String
    }


type alias ProjectFileType =
    { profile_file_type_id : Int
    , type_ : String
    }



-- SERIALIZATION --


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string


decoderProjectFile : Decoder ProjectFile
decoderProjectFile =
    decode ProjectFile
        |> required "project_file_id" Decode.int
        |> required "project_id" Decode.int
        |> required "project_file_type" decoderProjectFileType
        |> required "file" Decode.string
        |> optional "description" Decode.string ""


decoderProjectFileType : Decoder ProjectFileType
decoderProjectFileType =
    decode ProjectFileType
        |> required "project_file_type_id" Decode.int
        |> required "type" Decode.string


decoder : Decoder Publication
decoder =
    decode Publication
        |> required "publication_id" Decode.int
        |> optional "project_id" Decode.int 0
        |> optional "pub_code" Decode.string "NA"
        |> optional "doi" Decode.string "NA"
        |> optional "author" Decode.string "NA"
        |> required "title" Decode.string
        |> optional "pubmed_id" Decode.int 0
        |> optional "pub_date" Decode.string "NA"
        |> optional "project" (Decode.nullable decoderProject) Nothing
        |> optional "project_files" (Decode.list decoderProjectFile) []