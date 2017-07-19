module Request.Profile exposing (get)

import Data.Profile as Profile exposing (Profile)
import Http
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, optional)


host : String
host =
    "https://agave.iplantc.org/profiles/v2/me"


type alias ProfileResponse =
    { status : String
    , result : Profile
    }


responseDecoder : Decoder ProfileResponse
responseDecoder =
  decode ProfileResponse
    |> required "status" string --TODO make sure status is "success"
    |> required "result" Profile.decoder


get : String -> Http.Request ProfileResponse
get token =
    let
        headers =
            [ (Http.header "Authorization" ("Bearer " ++ token)) ]

    in
        Http.request
            { method = "GET"
            , headers = headers
            , url = host
            , body = Http.emptyBody
            , expect = Http.expectJson responseDecoder
            , timeout = Nothing
            , withCredentials = False
            }
