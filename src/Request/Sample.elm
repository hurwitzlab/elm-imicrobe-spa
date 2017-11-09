module Request.Sample exposing (..)

import Data.Sample as Sample exposing (..)
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


proteins : Int -> Http.Request Proteins
proteins id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString id) ++ "/proteins"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoderProteins)
        |> HttpBuilder.toRequest


centrifuge_results : Int -> Http.Request (List SampleToCentrifuge)
centrifuge_results id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString id) ++ "/centrifuge_results"

        decoder =
            Decode.list decoderSampleToCentrifuge
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


taxonomy_search : String -> Http.Request (List Centrifuge2)
taxonomy_search query =
    let
        url =
            apiBaseUrl ++ "/samples/taxonomy_search/" ++ query

        decoder =
            Decode.list decoderCentrifuge2
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


protein_pfam_search : String -> Http.Request (List PFAMProtein)
protein_pfam_search query =
    let
        url =
            apiBaseUrl ++ "/samples/protein_search/pfam/" ++ query
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoderPFAMProtein))
        |> HttpBuilder.toRequest


protein_kegg_search : String -> Http.Request (List KEGGProtein)
protein_kegg_search query =
    let
        url =
            apiBaseUrl ++ "/samples/protein_search/kegg/" ++ query
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoderKEGGProtein))
        |> HttpBuilder.toRequest
