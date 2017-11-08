module Data.App exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
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


encodeAppRun : Int -> Maybe Int -> String -> Encode.Value
encodeAppRun app_id user_id params =
    Encode.object
        [ "app_id" => Encode.int app_id
        , "user_id" => EncodeExtra.maybe Encode.int user_id
        , "params" => Encode.string params
        ]
