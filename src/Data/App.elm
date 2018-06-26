module Data.App exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))



type alias App =
    { app_id : Int
    , app_name : String
    , is_active : Bool
    , is_maintenance : Bool
    , provider_name : String
    , app_tags : List AppTag
    , app_data_types : List AppDataType
    , app_results : List AppResult
    }


type alias AppRun =
    { app_run_id : Int
    , app_id : Int
    , user_id : Maybe Int
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
    , alias_ : String
    }


type alias AppResult =
    { app_result_id : Int
    , path : String
    , app_data_type : AppDataType
    }


type alias FileBrowser =
    { id : String
    , username : String
    , token : String
    , path : String
    , source : String
    }



-- SERIALIZATION --


decoder : Decoder App
decoder =
    decode App
        |> required "app_id" Decode.int
        |> required "app_name" Decode.string
        |> required "is_active" Decode.bool
        |> required "is_maintenance" Decode.bool
        |> optional "provider_name" Decode.string ""
        |> optional "app_tags" (Decode.list decoderAppTag) []
        |> optional "app_data_types" (Decode.list decoderAppDataType) []
        |> optional "app_results" (Decode.list decoderAppResult) []


decoderAppRun : Decoder AppRun
decoderAppRun =
    decode AppRun
        |> required "app_run_id" Decode.int
        |> required "app_id" Decode.int
        |> optional "user_id" (Decode.nullable Decode.int) Nothing
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
        |> optional "alias" Decode.string ""


decoderAppResult : Decoder AppResult
decoderAppResult =
    decode AppResult
        |> required "app_result_id" Decode.int
        |> required "path" Decode.string
        |> required "app_data_type" decoderAppDataType


encodeAppRun : Int -> String -> Encode.Value
encodeAppRun app_id params =
    Encode.object
        [ "app_id" => Encode.int app_id
        , "params" => Encode.string params
        ]
