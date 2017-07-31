module Data.ProjectGroup exposing (Project, ProjectGroup, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))


type alias Project =
    { project_id : Int
    , project_name : String
    , project_to_project_group : ProjectToGroup
    }


type alias ProjectToGroup =
    { project_to_project_group_id : Int
    , project_id : Int
    , project_group_id : Int
    }


type alias ProjectGroup =
    { project_group_id : Int
    , group_name : String
    , description : String
    , url : String
    , projects : List Project
    }



-- SERIALIZATION --


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string
        |> required "project_to_project_group" decoderProjectToGroup


decoderProjectToGroup : Decoder ProjectToGroup
decoderProjectToGroup =
    decode ProjectToGroup
        |> required "project_to_project_group_id" Decode.int
        |> required "project_id" Decode.int
        |> required "project_group_id" Decode.int


decoder : Decoder ProjectGroup
decoder =
    decode ProjectGroup
        |> required "project_group_id" Decode.int
        |> required "group_name" Decode.string
        |> optional "description" Decode.string "NA"
        |> optional "url" Decode.string "NA"
        |> optional "projects" (Decode.list decoderProject) []



{--
encode : ProjectGroup -> Value
encode inv =
    Encode.object
        [ "investigator_id" => Encode.int inv.investigator_id
        , "investigator_name" => Encode.string inv.investigator_name
        , "institution" => Encode.string inv.institution
        ]
        --}
