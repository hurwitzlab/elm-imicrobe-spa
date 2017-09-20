module Request.Sample exposing (list, get, getSome, files)

import Data.Session as Session exposing (Session)
import Data.Sample as Sample exposing (Sample, SampleFile, decoderSampleFile)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import String exposing (join)
import Config exposing (apiBaseUrl)


list : Http.Request (List Sample)
list =
    let
        url =
            apiBaseUrl ++ "/samples"

        decoder =
            Decode.list Sample.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Sample
get id =
    HttpBuilder.get (apiBaseUrl ++ "/samples/" ++ toString id)
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


getSome : List Int -> Http.Request (List Sample)
getSome id_list =
    let
        url =
            apiBaseUrl ++ "/samples/?id=" ++ (join "," (List.map toString id_list))

        decoder =
            Decode.list Sample.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


files : List Int -> Http.Request (List SampleFile)
files id_list =
    let
        url =
            apiBaseUrl ++ "/samples/files/?id=" ++ (join "," (List.map toString id_list))

        decoder =
            Decode.list decoderSampleFile
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest