module Data.Pubchase exposing (Article, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)



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
        |> optional "article_id" Decode.int 0
        |> optional "title" Decode.string "NA"
        |> optional "journal_title" Decode.string "NA"
        |> optional "doi" Decode.string "NA"
        |> optional "authors" Decode.string "NA"
        |> optional "article_date" Decode.string "NA"
        |> optional "created_on" Decode.string "NA"
        |> optional "url" Decode.string "NA"
