module Data.Domain exposing (Domain, Project, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra


type alias Domain =
    { domain_id : Int
    , domain_name : String
    , projects : List Project
    }


type alias Project =
    { project_id : Int
    , project_name : String
    , project_to_domain : ProjectToDomain
    }


type alias ProjectToDomain =
    { project_to_domain_id : Int
    , project_id : Int
    , domain_id : Int
    }



-- SERIALIZATION --


decoderProjectToDomain : Decoder ProjectToDomain
decoderProjectToDomain =
    decode ProjectToDomain
        |> required "project_to_domain_id" Decode.int
        |> required "project_id" Decode.int
        |> required "domain_id" Decode.int


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string
        |> required "project_to_domain" decoderProjectToDomain


decoder : Decoder Domain
decoder =
    decode Domain
        |> required "domain_id" Decode.int
        |> required "domain_name" Decode.string
        |> optional "projects" (Decode.list decoderProject) []



{--
encode : Domain -> Value
encode inv =
    Encode.object
        [ "investigator_id" => Encode.int inv.investigator_id
        , "investigator_name" => Encode.string inv.investigator_name
        , "institution" => Encode.string inv.institution
        ]
        --}
