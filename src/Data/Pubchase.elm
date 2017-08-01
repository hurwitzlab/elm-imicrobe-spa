module Data.Pubchase exposing (Article, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra


type alias Article =
    { pubchase_id : Int
    , article_id : Int
    , title : String
    , journal_title : String
    , doi : String
    , authors : String
    , article_date : String
    , created_on : String
    , url : String
    }



-- SERIALIZATION --


decoder : Decoder Article
decoder =
    decode Article
        |> required "pubchase_id" Decode.int
        |> required "article_id" Decode.int
        |> required "title" Decode.string
        |> optional "journal_title" Decode.string "NA"
        |> optional "doi" Decode.string "NA"
        |> optional "authors" Decode.string "NA"
        |> optional "article_date" Decode.string "NA"
        |> optional "created_on" Decode.string "NA"
        |> optional "url" Decode.string "NA"



{--
encode : Pubchase -> Value
encode inv =
    Encode.object
        [ "investigator_id" => Encode.int inv.investigator_id
        , "investigator_name" => Encode.string inv.investigator_name
        , "institution" => Encode.string inv.institution
        ]
        --}
