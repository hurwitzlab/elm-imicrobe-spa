module Data.App exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))



type alias App =
    { app_id : Int
    , app_name : String
    , is_active : Int
    , app_tags : List AppTag
    , app_data_types : List AppDataType
    }


type alias AppRun =
    { app_run_id : Int
    , app_id : Int
    , user_id : Int
    , app_ran_at : String
    , params : String
    }


type alias AppTag =
    { app_tag_id : Int
    , value : String
    }


type alias AppDataType =
    { app_data_type_id : Int
    , name : String
    }


type alias FileBrowser =
    { id : String
    , username : String
    , token : String
    , path : String
    }



-- SERIALIZATION --


decoder : Decoder App
decoder =
    decode App
        |> required "app_id" Decode.int
        |> required "app_name" Decode.string
        |> optional "is_active" Decode.int 1
        |> optional "app_tags" (Decode.list decoderAppTag) []
        |> optional "app_data_types" (Decode.list decoderAppDataType) []


decoderAppRun : Decoder AppRun
decoderAppRun =
    decode AppRun
        |> required "app_run_id" Decode.int
        |> required "app_id" Decode.int
        |> required "user_id" Decode.int
        |> optional "app_ran_at" Decode.string ""
        |> optional "params" Decode.string ""


decoderAppTag : Decoder AppTag
decoderAppTag =
    decode AppTag
        |> required "app_tag_id" Decode.int
        |> required "value" Decode.string


decoderAppDataType : Decoder AppDataType
decoderAppDataType =
    decode AppDataType
        |> required "app_data_type_id" Decode.int
        |> required "name" Decode.string


encodeAppRun : AppRun -> Encode.Value
encodeAppRun run =
    Encode.object
        [ "app_id" => Encode.int run.app_id
        , "user_id" => Encode.int run.user_id
        , "app_ran_at" => Encode.string run.app_ran_at
        ]
