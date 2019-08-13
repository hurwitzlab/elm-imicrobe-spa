module Data.Agave exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)



type alias Profile =
    { email : String
    , first_name : String
    , last_name : String
    , username : String
    , uid : Int
    }


type alias App =
    { name : String
    , helpURI : String
    , shortDescription : String
    , longDescription : String
    , version : String
    , tags : List String
    , isPublic : Bool
    , defaultMaxRunTime : String
    , defaultQueue : String
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
    , argument : String
    , description : String
    }


type alias Semantics =
    { filesTypes : List String
    , minCardinality : Int
    , maxCardinality : Int
    , ontology : List String
    }


type alias InputValue =
    { order : Int
    , default : ValueType
    , required : Bool
    , visible : Bool
    }


type alias ParameterValue = -- TODO change this to use variable decoding per http://folkertdev.nl/blog/elm-messy-json-value/, see ValueType in this file as example
    { order : Int
    , visible : Bool
    , type_ : String
    , default : ValueType
    , enum_values : Maybe (List (List (String, String)))
    }


type ValueType
    = StringValue String
    | ArrayValue (List String)
    | BoolValue Bool -- workaround for BowtieBatch app
    | NumberValue Float


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
    , value : List String
    }


type alias JobParameter =
    { id : String
    , value : ValueType
    }


type alias Notification =
    { url : String
    , event : String
    }


type alias Job =
    { id : String
    , name : String
    , owner : String
    , app_id : String
    , created : String -- time string in CT time zone
    , ended : String -- time string in CT time zone
    , status : String
    , inputs : Dict String JobInputValue
    , parameters : Dict String ValueType
    }


type JobInputValue
    = JobInputString String
    | JobInputArray (List String)


type alias JobStatus =
    { id : String
    }


type alias JobError =
    { status : String
    , message : String
    }


type alias JobOutput =
    { name : String
    , path : String
    , type_ : String
    }


type alias JobHistory =
    { status : String
    , created : String
    , createdBy : String
    , description : String
    }


type alias FileResult =
    { name : String
    , path : String
    , type_ : String
    , format : String
    , mimeType : String
    , lastModified : String
    , length : Int
    }


type alias UploadResult =
    { name : String
    , path : String
    }


type alias PermissionResult =
    { username : String
    , permission : Permission
    , recursive : Bool
    }


type alias Permission =
    { read : Bool
    , write : Bool
    , execute : Bool
    }



-- SERIALIZATION --


decoderProfile : Decoder Profile
decoderProfile =
    decode Profile
        |> required "email" Decode.string
        |> required "first_name" Decode.string
        |> required "last_name" Decode.string
        |> required "username" Decode.string
        |> required "uid" Decode.int


decoderApp : Decoder App
decoderApp =
    decode App
        |> required "name" Decode.string
        |> required "helpURI" Decode.string
        |> required "shortDescription" Decode.string
        |> optional "longDescription" Decode.string ""
        |> required "version" Decode.string
        |> optional "tags" (Decode.list Decode.string) []
        |> optional "isPublic" Decode.bool False
        |> optional "defaultMaxRunTime" Decode.string ""
        |> optional "defaultQueue" Decode.string ""
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
        |> optional "argument" Decode.string ""
        |> optional "description" Decode.string ""


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
        |> optional "default" decoderValueType (StringValue "")
        |> optional "required" Decode.bool True
        |> optional "visible" Decode.bool True


decoderParameterValue : Decoder ParameterValue
decoderParameterValue =
    decode ParameterValue
        |> required "order" Decode.int
        |> optional "visible" Decode.bool True
        |> required "type" Decode.string
        |> optional "default" decoderValueType (StringValue "")
        |> optional "enum_values" (Decode.nullable (Decode.list (Decode.keyValuePairs Decode.string))) Nothing


decoderValueType : Decoder ValueType
decoderValueType =
    Decode.oneOf
        [ Decode.map StringValue Decode.string
        , Decode.map ArrayValue (Decode.list Decode.string)
        , Decode.map BoolValue Decode.bool
        , Decode.map NumberValue Decode.float
        ]


decoderJobStatus : Decoder JobStatus
decoderJobStatus =
    decode JobStatus
        |> required "id" Decode.string


decoderJobError : Decoder JobError
decoderJobError =
    decode JobError
        |> required "status" Decode.string
        |> required "message" Decode.string


decoderJob : Decoder Job
decoderJob =
    decode Job
        |> required "id" Decode.string
        |> required "name" Decode.string
        |> optional "owner" Decode.string ""
        |> required "appId" Decode.string
        |> optional "created" Decode.string ""
        |> optional "ended" Decode.string ""
        |> optional "status" Decode.string ""
        |> optional "inputs" (Decode.dict decoderJobInput) Dict.empty
        |> optional "parameters" (Decode.dict decoderValueType) Dict.empty


decoderJobInput : Decoder JobInputValue
decoderJobInput =
    Decode.oneOf
        [ Decode.map JobInputString Decode.string
        , Decode.map JobInputArray (Decode.list Decode.string)
        ]


decoderJobOutput : Decoder JobOutput
decoderJobOutput =
    decode JobOutput
        |> required "name" Decode.string
        |> required "path" Decode.string
        |> required "type" Decode.string


decoderJobHistory : Decoder JobHistory
decoderJobHistory =
    decode JobHistory
        |> required "status" Decode.string
        |> required "created" Decode.string
        |> required "createdBy" Decode.string
        |> required "description" Decode.string


decoderFileResult : Decoder FileResult
decoderFileResult =
    decode FileResult
        |> required "name" Decode.string
        |> required "path" Decode.string
        |> required "type" Decode.string
        |> required "format" Decode.string
        |> required "mimeType" Decode.string
        |> required "lastModified" Decode.string
        |> required "length" Decode.int


decoderUploadResult : Decoder UploadResult
decoderUploadResult =
    decode UploadResult
        |> required "name" Decode.string
        |> required "path" Decode.string


decoderPermissionResult : Decoder PermissionResult
decoderPermissionResult =
    decode PermissionResult
        |> required "username" Decode.string
        |> required "permission" decoderPermission
        |> required "recursive" Decode.bool


decoderPermission : Decoder Permission
decoderPermission =
    decode Permission
        |> required "read" Decode.bool
        |> required "write" Decode.bool
        |> required "execute" Decode.bool


encodeProfile : Profile -> Value
encodeProfile profile =
    Encode.object
        [ ( "email", Encode.string profile.email )
        , ( "first_name", Encode.string profile.first_name )
        , ( "last_name", Encode.string profile.last_name )
        , ( "username", Encode.string profile.username )
        ]


encodeJobRequest : JobRequest -> List (String, String) -> Encode.Value
encodeJobRequest request settings =
    Encode.object
        ([ ( "name", Encode.string request.name )
        , ( "appId", Encode.string request.app_id )
        , ( "archive", Encode.bool request.archive )
--        , ( "inputs", Encode.list (List.map encodeJobInput request.inputs) )
        , ( "inputs", Encode.object (List.map (\i -> (i.id, (Encode.list (List.map Encode.string i.value)))) request.inputs) )
        , ( "parameters", Encode.object (List.map encodeJobParameter request.parameters) )
--        , ( "parameters", Encode.object (List.map (\p-> (p.id, (Encode.string p.value))) request.parameters) )
        , ( "notifications", Encode.list (List.map encodeNotification request.notifications) )
        ]
        ++ (settings |> List.map (Tuple.mapSecond Encode.string))
        )


encodeJobInput : JobInput -> Encode.Value
encodeJobInput input =
    Encode.object
        [ ( input.id, Encode.list (List.map Encode.string input.value) ) ]


encodeJobParameter : JobParameter -> (String, Encode.Value)
encodeJobParameter param =
    ( param.id, encodeValueType param.value )


encodeValueType : ValueType -> Encode.Value
encodeValueType value =
    case value of
        StringValue s ->
            Encode.string s

        ArrayValue l ->
            Encode.list (List.map Encode.string l)

        BoolValue b ->
            Encode.bool b

        NumberValue n ->
            Encode.float n


encodeNotification : Notification -> Encode.Value
encodeNotification notification =
    Encode.object
        [ ( "url", Encode.string notification.url )
        , ( "event", Encode.string notification.event )
        ]
