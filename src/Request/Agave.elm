module Request.Agave exposing (..)

import Data.Profile as Profile exposing (Profile)
import Data.Agave as Agave exposing (..)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Config exposing (agaveBaseUrl)



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
            agaveBaseUrl ++ "/profiles/v2/me"

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
            agaveBaseUrl ++ "/apps/v2/" ++ name

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderApp))
        |> HttpBuilder.toRequest


getJobs : String -> Http.Request (Response (List Job))
getJobs token =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list Agave.decoderJob)))
        |> HttpBuilder.toRequest


getJob : String -> String -> Http.Request (Response Job)
getJob token id =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderJob))
        |> HttpBuilder.toRequest


getJobOutputs : String -> String -> String -> Http.Request (Response (List JobOutput))
getJobOutputs username token id =
    let
        url =
            -- Changed Agave endpoint for PlanB support
            --agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/outputs/listings"
            agaveBaseUrl ++ "/files/v2/listings/" ++ username ++ "/archive/jobs/job-" ++ id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list Agave.decoderJobOutput)))
        |> HttpBuilder.toRequest


getJobOutput : String -> String -> String -> String -> Http.Request String
getJobOutput username token id path =
    let
        url =
            -- Changed Agave endpoint after adding archive=True
            --agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/outputs/media/" ++ path
            agaveBaseUrl ++ "/files/v2/media/" ++ username ++ "/archive/jobs/job-" ++ id ++ "/" ++ path

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


launchJob : String -> JobRequest -> Http.Request (Response JobStatus)
launchJob token request =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody (encodeJobRequest request)
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderJobStatus))
        |> HttpBuilder.toRequest