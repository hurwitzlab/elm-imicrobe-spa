module Data.Publication exposing (Publication, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra


type alias Publication =
    { publication_id : Int
    , project_id : Int
    , pub_code : String
    , doi : String
    , author : String
    , title : String
    , pubmed_id : Int
    , pub_date : String
    , project : Project
    }


type alias Project =
    { project_id : Int
    , project_name : String
    }



-- SERIALIZATION --


decoderProject : Decoder Project
decoderProject =
    decode Project
        |> required "project_id" Decode.int
        |> required "project_name" Decode.string


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
        |> required "project" decoderProject



-- handle NULL!
-- |> optional "projects" (Decode.list decoderProject) []
{--
encode : Publication -> Value
encode inv =
    Encode.object
        [ "investigator_id" => Encode.int inv.investigator_id
        , "investigator_name" => Encode.string inv.investigator_name
        , "institution" => Encode.string inv.institution
        ]
        --}
