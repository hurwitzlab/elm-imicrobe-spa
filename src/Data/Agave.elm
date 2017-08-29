module Data.Agave exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode
import Util exposing ((=>))



type alias App =
    { name : String
    , helpURI : String
    , shortDescription : String
    , inputs : List AppInput
    , parameters : List AppParameter
    }


type alias AppInput =
    { id : String
    , details : Details
    }


type alias AppParameter =
    { id : String
    , details : Details
    , value : Value
    }


type alias Details =
    { label : String
    }


type alias Value =
    { order : Int
    , type_ : String
    , enum_values : List (List (String, String))
    }


type alias JobRequest =
    { name : String
    , app_id : String
    , archive : Bool
    , inputs : List JobInput
    , parameters : List JobParameter
    , notifications : List Notification
    }


type alias JobInput =
    { id : String
    , value : String
    }


type alias JobParameter =
    { id : String
    , value : String
    }


type alias Notification =
    { url : String
    , event : String
    }


type alias Job =
    { id : String
    , name : String
    , app_id : String
    }


type alias JobStatus =
    { id : String
    }


type alias Jobs =
    { jobs : List Job
    }



-- SERIALIZATION --


decoderApp : Decoder App
decoderApp =
    decode App
        |> required "name" Decode.string
        |> required "helpURI" Decode.string
        |> required "shortDescription" Decode.string
        |> required "inputs" (Decode.list decoderAppInput)
        |> required "parameters" (Decode.list decoderAppParameter)


decoderAppInput : Decoder AppInput
decoderAppInput =
    decode AppInput
        |> required "id" Decode.string
        |> required "details" decoderDetails


decoderAppParameter : Decoder AppParameter
decoderAppParameter =
    decode AppParameter
        |> required "id" Decode.string
        |> required "details" decoderDetails
        |> required "value" decoderValue


decoderDetails : Decoder Details
decoderDetails =
    decode Details
        |> required "label" Decode.string


decoderValue : Decoder Value
decoderValue =
    decode Value
        |> required "order" Decode.int
        |> required "type" Decode.string
        |> required "enum_values" (Decode.list (Decode.keyValuePairs Decode.string))


decoderJobStatus : Decoder JobStatus
decoderJobStatus =
    decode JobStatus
        |> required "id" Decode.string


decoderJobs : Decoder Jobs
decoderJobs =
    decode Jobs
        |> required "jobs" (Decode.list decoderJob)


decoderJob : Decoder Job
decoderJob =
    decode Job
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "app_id" Decode.string


encodeJobRequest : JobRequest -> Encode.Value
encodeJobRequest request =
    Encode.object
        [ "name" => Encode.string request.name
        , "appId" => Encode.string request.app_id
        , "archive" => Encode.bool request.archive
--        , "inputs" => Encode.list (List.map encodeJobInput request.inputs)
        , "inputs" => Encode.object (List.map (\i -> (i.id, (Encode.string i.value))) request.inputs)
--        , "parameters" => Encode.list (List.map encodeJobParameter request.parameters)
        , "parameters" => Encode.object (List.map (\p-> (p.id, (Encode.string p.value))) request.parameters)
        , "notifications" => Encode.list (List.map encodeNotification request.notifications)
        ]


encodeJobInput : JobInput -> Encode.Value
encodeJobInput input =
    Encode.object
        [ input.id => Encode.string input.value ]


encodeJobParameter : JobParameter -> Encode.Value
encodeJobParameter param =
    Encode.object
        [ param.id => Encode.string param.value ]


encodeNotification : Notification -> Encode.Value
encodeNotification notification =
    Encode.object
        [ "url" => Encode.string notification.url
        , "event" => Encode.string notification.event
        ]