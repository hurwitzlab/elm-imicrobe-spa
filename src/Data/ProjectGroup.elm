module Data.ProjectGroup exposing (Project, ProjectGroup, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)



type alias ProjectGroup =
    { project_group_id : Int
    , group_name : String
    , description : String
    , url : String
    , projects : List Project
    , users : List User
    }


type alias Project =
    { project_id : Int
    , project_name : String
    }


type alias User =
    { user_id : Int
    , user_name : String
    }



-- SERIALIZATION --


decoder : Decoder ProjectGroup
decoder =
    decode ProjectGroup
        |> required "project_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string "NA"
        |> optional "projects" (Decode.list decoderProject) []
        |> optional "users" (Decode.list decoderUser) []


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string


decoderUser : Decoder User
decoderUser =
    decode User
        |> required "user_id" Decode.int
        |> required "user_name" Decode.string
