module Data.App exposing (App, FileBrowser, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra


type alias App =
    { app_id : Int
    , app_name : String
    , is_active : Int
    }


type alias AppRun =
    { app_run_id : Int
    , app_id : Int
    , user_id : Int
    , app_ran_at : String
    , params : String
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


decoderAppRun : Decoder AppRun
decoderAppRun =
    decode AppRun
        |> required "app_run_id" Decode.int
        |> required "app_id" Decode.int
        |> required "user_id" Decode.int
        |> optional "app_id" Decode.string ""
        |> optional "params" Decode.string ""


{--
encode : App -> Value
encode inv =
    Encode.object
        [ "investigator_id" => Encode.int inv.investigator_id
        , "investigator_name" => Encode.string inv.investigator_name
        , "institution" => Encode.string inv.institution
        ]
        --}
