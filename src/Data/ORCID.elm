module Data.ORCID exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)



type alias TokenResponse =
    { access_token : String
    , token_type : String
    , refresh_token : String
    , expires_in : Int
    , scope : String
    , orcid : String
    }


type alias Record =
    { person : Person
    }


type alias Person =
    { name : Name
    }


type alias Name =
    { family_name : Value
    , given_names : Value
    }


type alias Value =
    { value : String
    }



-- SERIALIZATION --


decoderTokenResponse : Decoder TokenResponse
decoderTokenResponse =
    decode TokenResponse
        |> required "access_token" Decode.string
        |> required "token_type" Decode.string
        |> required "refresh_token" Decode.string
        |> required "expires_in" Decode.int
        |> required "scope" Decode.string
        |> required "orcid" Decode.string


decoderRecord : Decoder Record
decoderRecord =
    decode Record
        |> required "person" decoderPerson


decoderPerson : Decoder Person
decoderPerson =
    decode Person
        |> required "name" decoderName


decoderName : Decoder Name
decoderName =
    decode Name
        |> required "family_name" decoderValue
        |> required "given_names" decoderValue


decoderValue : Decoder Value
decoderValue =
    decode Value
        |> required "value" Decode.string
