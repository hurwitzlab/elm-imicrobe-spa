module Data.Agave exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode
import Util exposing ((=>))



type alias App =
    { name : String
    , helpURI : String
    , shortDescription : String
    , version : String
    , tags : List String
    , inputs : List AppInput
    , parameters : List AppParameter
    }


type alias AppInput =
    { id : String
    , details : Details
    , value : InputValue
    , semantics : Semantics
    }


type alias AppParameter =
    { id : String
    , details : Details
    , value : ParameterValue
    }


type alias Details =
    { label : String
    }


type alias Semantics =
    { filesTypes : List String
    , minCardinality : Int
    , maxCardinality : Int
    , ontology : List String
    }


type alias InputValue =
    { order : Int
    , default : DefaultValue
    , required : Bool
    , visible : Bool
    }


type DefaultValue
    = StringValue String
    | ArrayValue (List String)


type alias ParameterValue = -- TODO change this to use variable decoding per http://folkertdev.nl/blog/elm-messy-json-value/, see DefaultValue in this file as example
    { order : Int
    , type_ : String
    , default : String
    , enum_values : Maybe (List (List (String, String)))
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
    , startTime : String
    , endTime : String
    , status : String
    }


type alias JobStatus =
    { id : String
    }



-- SERIALIZATION --


decoderApp : Decoder App
decoderApp =
    decode App
        |> required "name" Decode.string
        |> required "helpURI" Decode.string
        |> required "shortDescription" Decode.string
        |> required "version" Decode.string
        |> optional "tags" (Decode.list Decode.string) []
        |> required "inputs" (Decode.list decoderAppInput)
        |> required "parameters" (Decode.list decoderAppParameter)


decoderAppInput : Decoder AppInput
decoderAppInput =
    decode AppInput
        |> required "id" Decode.string
        |> required "details" decoderDetails
        |> required "value" decoderInputValue
        |> required "semantics" decoderSemantics


decoderAppParameter : Decoder AppParameter
decoderAppParameter =
    decode AppParameter
        |> required "id" Decode.string
        |> required "details" decoderDetails
        |> required "value" decoderParameterValue


decoderDetails : Decoder Details
decoderDetails =
    decode Details
        |> required "label" Decode.string


decoderSemantics : Decoder Semantics
decoderSemantics =
    decode Semantics
        |> required "fileTypes" (Decode.list Decode.string)
        |> optional "minCardinality" Decode.int 0
        |> optional "maxCardinality" Decode.int 0
        |> optional "ontology" (Decode.list Decode.string) []


decoderInputValue : Decoder InputValue
decoderInputValue =
    decode InputValue
        |> required "order" Decode.int
        |> optional "default" decoderDefaultValue (StringValue "")
        |> optional "required" Decode.bool True
        |> optional "visible" Decode.bool True


-- FIXME this is workaround for Mash app having [""] default value where other apps use "".
-- Need to properly support variable default type.
decoderDefaultValue : Decoder DefaultValue
decoderDefaultValue =
    Decode.succeed (StringValue "")


decoderParameterValue : Decoder ParameterValue
decoderParameterValue =
    decode ParameterValue
        |> required "order" Decode.int
        |> required "type" Decode.string
        |> optional "default" Decode.string ""
        |> optional "enum_values" (Decode.nullable (Decode.list (Decode.keyValuePairs Decode.string))) Nothing


decoderJobStatus : Decoder JobStatus
decoderJobStatus =
    decode JobStatus
        |> required "id" Decode.string


decoderJob : Decoder Job
decoderJob =
    decode Job
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> required "appId" Decode.string
        |> optional "startTime" Decode.string ""
        |> optional "endTime" Decode.string ""
        |> optional "status" Decode.string ""


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
