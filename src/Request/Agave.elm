module Request.Agave exposing (..)

import Data.Agave as Agave exposing (..)
import Http
import HttpBuilder
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode exposing (Value)
import Util exposing (removeTrailingSlash, (=>))
import Config exposing (agaveBaseUrl)



-- TODO move to Data.Agave
type alias Response a =
    { status : String
    , result : a
    }


-- TODO move to Data.Agave
type alias EmptyResponse =
    { status : String
    }


responseDecoder : Decoder a -> Decoder (Response a)
responseDecoder decoder =
    decode Response
        |> required "status" string --TODO make sure status is "success"
        |> required "result" decoder


emptyResponseDecoder : Decoder EmptyResponse
emptyResponseDecoder =
    decode EmptyResponse
        |> required "status" string --TODO make sure status is "success"


getProfile : String -> Http.Request (Response Profile)
getProfile token =
    let
        url =
            agaveBaseUrl ++ "/profiles/v2/me"

        headers =
            [ Http.header "Authorization" token ]
    in
    Http.request -- TODO convert to HttpBuilder
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (responseDecoder decoderProfile)
        , timeout = Nothing
        , withCredentials = False
        }


searchProfiles : String -> String -> Http.Request (Response (List Profile))
searchProfiles token username =
    let
        url =
            agaveBaseUrl ++ "/profiles/v2"

        headers =
            [( "Authorization", token)]

        queryParams =
            [( "username", username )]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderProfile)))
        |> HttpBuilder.toRequest


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


getJobHistory : String -> String -> Http.Request (Response (List JobHistory))
getJobHistory token id =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/history"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list Agave.decoderJobHistory)))
        |> HttpBuilder.toRequest


getJobOutputs : String -> String -> String -> Maybe String -> Http.Request (Response (List JobOutput))
getJobOutputs username token id path =
    let
        baseUrl =
            -- Changed Agave endpoint for PlanB support
            --agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/outputs/listings"
            agaveBaseUrl ++ "/files/v2/listings/" ++ username ++ "/archive/jobs/job-" ++ id

        url =
            case path of
                Nothing ->
                    baseUrl

                Just path ->
                    baseUrl ++ "/" ++ path

        headers =
            [( "Authorization", token)]

        queryParams =
            [("limit", "9999")]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list Agave.decoderJobOutput)))
        |> HttpBuilder.toRequest


getFileList : String -> String -> Http.Request (Response (List FileResult))
getFileList token path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/listings/" ++ path

        headers =
            [( "Authorization", token)]

        queryParams =
            [("limit", "9999")]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list Agave.decoderFileResult)))
        |> HttpBuilder.toRequest


getFileRange : String -> String -> Maybe (Int, Int) -> Http.Request String
getFileRange token path range =
    let
        url =
            -- Changed Agave endpoint after adding archive=True
            --agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/outputs/media/" ++ path
            agaveBaseUrl ++ "/files/v2/media/" ++ (removeTrailingSlash path)

        authHeader =
            ( "Authorization", token)

        headers =
            case range of
                Nothing ->
                    [ authHeader ]
                Just (start, end) ->
                    [ authHeader
                    , ( "Range", "bytes=" ++ (toString start) ++ "-" ++ (toString end) )
                    ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


getFile : String -> String -> Http.Request String
getFile token path =
    getFileRange token path Nothing


getJobOutput : String -> String -> String -> String -> Http.Request String
getJobOutput username token id path =
    let
        jobPath =
            username ++ "/archive/jobs/job-" ++ id ++ "/" ++ path
    in
    getFile token jobPath


launchJob : String -> JobRequest -> List (String, String) -> Http.Request (Response JobStatus)
launchJob token request settings =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody (encodeJobRequest request settings)
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderJobStatus))
        |> HttpBuilder.toRequest


shareJob : String -> String -> String -> String -> Http.Request (Response JobStatus)
shareJob token id username permission =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id ++ "/pems/" ++ username

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "permission" => Encode.string permission ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder Agave.decoderJobStatus))
        |> HttpBuilder.toRequest


stopJob : String -> String -> Http.Request String
stopJob token id =
    let
        url =
            agaveBaseUrl ++ "/jobs/v2/" ++ id

        headers =
            [( "Authorization", token) ]

        body =
            Encode.object
                [ "action" => Encode.string "stop" ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


mkdir : String -> String -> String -> Http.Request EmptyResponse
mkdir token path dirname =
    let
        url =
            agaveBaseUrl ++ "/files/v2/media/" ++ (removeTrailingSlash path)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "action" => Encode.string "mkdir"
                , "path" => Encode.string dirname
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson emptyResponseDecoder)
        |> HttpBuilder.toRequest


delete : String -> String -> Http.Request EmptyResponse
delete token path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/media/" ++ (removeTrailingSlash path)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson emptyResponseDecoder)
        |> HttpBuilder.toRequest


getFilePermission : String -> String -> Http.Request (Response (List PermissionResult))
getFilePermission token path =
    let
        url =
            agaveBaseUrl ++ "/files/v2/pems/system/data.iplantcollaborative.org/" ++ (removeTrailingSlash path)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (responseDecoder (Decode.list decoderPermissionResult)))
        |> HttpBuilder.toRequest


setFilePermission : String -> String -> String -> Bool -> String -> Http.Request EmptyResponse
setFilePermission token username permission recursive path =
        let
        url =
            agaveBaseUrl ++ "/files/v2/pems/system/data.iplantcollaborative.org/" ++ (removeTrailingSlash path)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "username" => Encode.string username
                , "permission" => Encode.string permission
                , "recursive" => Encode.bool recursive
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson emptyResponseDecoder)
        |> HttpBuilder.toRequest
