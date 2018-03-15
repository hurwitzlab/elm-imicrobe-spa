module Request.Sample exposing (..)

import Data.Sample as Sample exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))
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


create : String -> Int -> String -> Http.Request Sample
create token project_id sample_name =
    let
        url =
            apiBaseUrl ++ "/samples"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "sample_name" => Encode.string sample_name
                , "project_id" => Encode.int project_id
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


update : String -> Int -> String -> String -> String -> Http.Request Sample
update token sample_id sample_name sample_code sample_type =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "sample_name" => Encode.string sample_name
                , "sample_code" => Encode.string sample_code
                , "sample_type" => Encode.string sample_type
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


remove : String -> Int -> Http.Request String
remove token sample_id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


addAttribute : String -> Int -> String -> String -> String -> Http.Request Sample
addAttribute token sample_id attr_type attr_aliases attr_value =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id) ++ "/attributes"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "attr_type" => Encode.string attr_type
                , "attr_aliases" => Encode.string attr_aliases
                , "attr_value" => Encode.string attr_value
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


updateAttribute : String -> Int -> Int -> String -> String -> String -> Http.Request Sample
updateAttribute token sample_id attr_id attr_type attr_aliases attr_value =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id) ++ "/attributes/" ++ (toString attr_id)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "attr_type" => Encode.string attr_type
                , "attr_aliases" => Encode.string attr_aliases
                , "attr_value" => Encode.string attr_value
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


removeAttribute : String -> Int -> Int -> Http.Request Sample
removeAttribute token sample_id sample_attr_id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id) ++ "/attributes/" ++ (toString sample_attr_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


addFiles : String -> Int -> List String -> Http.Request Sample
addFiles token sample_id files =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id) ++ "/files"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "files" => Encode.list (List.map Encode.string files)
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


removeFile : String -> Int -> Int -> Http.Request Sample
removeFile token sample_id sample_file_id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id) ++ "/files/" ++ (toString sample_file_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest
