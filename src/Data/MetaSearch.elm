module Data.MetaSearch exposing (SearchResult, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)


type alias SearchResult =
    { id : Int
    , table_name : String
    , name : String
    }



-- SERIALIZATION --


decoder : Decoder SearchResult
decoder =
    decode SearchResult
        |> required "id" Decode.int
        |> required "table_name" Decode.string
        |> required "name" Decode.string