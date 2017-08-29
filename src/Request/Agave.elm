module Request.Agave exposing (..)

import Data.Profile as Profile exposing (Profile)
import Data.Agave as Agave exposing (App, Jobs, JobRequest, JobStatus, encodeJobRequest)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)



-- TODO move into config file
baseUrl : String
baseUrl =
    "https://agave.iplantc.org"


type alias Response a =
    { status : String
    , result : a
    }


responseDecoder : Decoder a -> Decoder (Response a)
responseDecoder decoder =
    decode Response
        |> required "status" string --TODO make sure status is "success"
        |> required "result" decoder


getProfile : String -> Http.Request (Response Profile)
getProfile token =
    let
        url =
            baseUrl ++ "/profiles/v2/me"

        headers =
            [ Http.header "Authorization" token ]
    in
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (responseDecoder Profile.decoder)
        , timeout = Nothing
        , withCredentials = False
        }


getApp : String -> String -> Http.Request (Response App)
getApp token name =
    let
        url =
            baseUrl ++ "/apps/v2/" ++ name

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderApp))
        |> HttpBuilder.toRequest


getJobs : String -> Http.Request (Response Jobs)
getJobs token =
    let
        url =
            baseUrl ++ "/jobs/v2/"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderJobs))
        |> HttpBuilder.toRequest


launchJob : String -> JobRequest -> Http.Request (Response JobStatus)
launchJob token request =
    let
        url =
            baseUrl ++ "/jobs/v2"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody (encodeJobRequest request)
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderJobStatus))
        |> HttpBuilder.toRequest