module Data.Investigator exposing (Investigator, Project, Sample, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))


type alias Project =
    { project_id : Int
    , project_name : String
    }


type alias Sample =
    { sample_id : Int
    , sample_name : String
    , sample_type : String
    , latitude : String
    , longitude : String
    }


type alias Investigator =
    { investigator_id : Int
    , investigator_name : String
    , institution : String
    , projects : List Project
    , samples : List Sample
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
        |> optional "sample_type" Decode.string "NA"
        |> optional "latitude" Decode.string ""
        |> optional "longitude" Decode.string ""


decoder : Decoder Investigator
decoder =
    decode Investigator
        |> required "investigator_id" Decode.int
        |> required "investigator_name" Decode.string
        |> required "institution" Decode.string
        |> optional "projects" (Decode.list decoderProject) []
        |> optional "samples" (Decode.list decoderSample) []



{--
encode : Investigator -> Value
encode inv =
    Encode.object
        [ "investigator_id" => Encode.int inv.investigator_id
        , "investigator_name" => Encode.string inv.investigator_name
        , "institution" => Encode.string inv.institution
        ]
        --}
