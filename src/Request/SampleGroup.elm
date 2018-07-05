module Request.SampleGroup exposing (..)

import Data.SampleGroup exposing (..)
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))
import Config exposing (apiBaseUrl)



list : String -> Http.Request (List SampleGroup)
list token =
    let
        url =
            apiBaseUrl ++ "/sample_groups"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoder))
        |> HttpBuilder.toRequest


get : String -> Int -> Http.Request SampleGroup
get token id =
    let
        url =
            apiBaseUrl ++ "/sample_groups/" ++ toString id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


create : String -> String -> List Int -> Http.Request SampleGroup
create token group_name sample_ids =
    let
        url =
            apiBaseUrl ++ "/sample_groups"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "group_name" => Encode.string group_name
                , "sample_ids" => (List.map Encode.int sample_ids |> Encode.list)
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


remove : String -> Int -> Http.Request String
remove token sample_group_id =
    let
        url =
            apiBaseUrl ++ "/sample_groups/" ++ (toString sample_group_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


removeSample : String -> Int -> Int -> Http.Request SampleGroup
removeSample token sample_group_id sample_id =
    let
        url =
            apiBaseUrl ++ "/sample_groups/" ++ (toString sample_group_id) ++ "/samples/" ++ (toString sample_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


removeAllSamples : String -> Int -> Http.Request SampleGroup
removeAllSamples token sample_group_id =
    let
        url =
            apiBaseUrl ++ "/sample_groups/" ++ (toString sample_group_id) ++ "/samples"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
