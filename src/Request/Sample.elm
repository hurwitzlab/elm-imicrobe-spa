module Request.Sample exposing (..)

import Data.Sample as Sample exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import RemoteData exposing (WebData, sendRequest)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Dict exposing (Dict)
import Exts.Dict as EDict
import Util exposing ((=>))
import Config exposing (apiBaseUrl)



list : String -> Http.Request (List Sample)
list token =
    let
        url =
            apiBaseUrl ++ "/samples"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list Sample.decoder))
        |> HttpBuilder.toRequest


get : String -> Int -> Http.Request Sample
get token id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ toString id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest


getSome : String -> List Int -> Http.Request (List Sample)
getSome token id_list =
    let
        url =
            apiBaseUrl ++ "/samples"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "ids" => Encode.string (id_list |> List.map toString |> String.join ",") ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list Sample.decoder))
        |> HttpBuilder.toRequest


getParams : Http.Request (Dict.Dict String String)
getParams =
    let
        url =
            apiBaseUrl ++ "/samples/search_params"

        decoder =
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


getParamValues :
    String
    -> Dict String (List String)
    -> Dict String (List Sample.JsonType)
    -> Dict String String
    -> Http.Request (Dict String (List Sample.JsonType))
getParamValues optionName optionValues possibleOptionValues params =
    let
        url =
            apiBaseUrl ++ "/samples/search_param_values"

        decoder =
            Decode.dict (Decode.list Sample.oneOfJsonType)

        body =
            Encode.object
                [ ( "param", Encode.string optionName )
                , ( "query", serializeForm optionValues possibleOptionValues params )
                ]
                |> Http.jsonBody
    in
    Http.post url body decoder


search : Dict String (List String) -> Dict String (List Sample.JsonType) -> Dict String String -> Cmd (WebData (List SearchResult))--Cmd (WebData (List (Dict String JsonType)))
search optionValues possibleOptionValues params =
    let
        url =
            apiBaseUrl ++ "/samples/search"

        body =
            serializeForm optionValues possibleOptionValues params
                |> Http.jsonBody

        decoder =
            Decode.list decoderSearchResult--(Decode.dict Sample.oneOfJsonType)
    in
    Http.post url body decoder
        |> RemoteData.sendRequest


serializeForm :
    Dict String (List String)
    -> Dict String (List JsonType)
    -> Dict String String
    -> Encode.Value
serializeForm optionValues possibleOptionValues paramTypes =
    let
        mkFloats =
            List.filterMap (String.toFloat >> Result.toMaybe)

        encodeVals param vals =
            let
                paramName =
                    case
                        String.startsWith "min__" param
                            || String.startsWith "max__" param
                    of
                        True ->
                            String.dropLeft 5 param

                        False ->
                            param

                dataType =
                    EDict.getWithDefault "string" paramName paramTypes

                enc f xs =
                    case xs of
                        [] ->
                            Encode.null

                        x :: [] ->
                            f x

                        _ ->
                            Encode.list (List.map f xs)
            in
            case dataType of
                "number" ->
                    enc Encode.float (mkFloats vals)

                _ ->
                    enc Encode.string vals
    in
    Dict.toList optionValues
        |> List.map (\( k, vs ) -> ( k, encodeVals k vs ))
        |> Encode.object


files : String -> List Int -> Http.Request (List SampleFile)
files token id_list =
    let
        url =
            apiBaseUrl ++ "/samples/files"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "ids" => Encode.string (id_list |> List.map toString |> String.join ",") ]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoderSampleFile))
        |> HttpBuilder.toRequest


proteins : String -> Int -> Http.Request Proteins
proteins token id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString id) ++ "/proteins"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoderProteins)
        |> HttpBuilder.toRequest


centrifuge_results : String -> Int -> Http.Request (List SampleToCentrifuge)
centrifuge_results token id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString id) ++ "/centrifuge_results"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoderSampleToCentrifuge))
        |> HttpBuilder.toRequest


taxonomy_search : String -> String -> Http.Request (List Centrifuge2)
taxonomy_search token query =
    let
        url =
            apiBaseUrl ++ "/samples/taxonomy_search/" ++ query

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoderCentrifuge2))
        |> HttpBuilder.toRequest


protein_pfam_search : String -> String -> Http.Request (List PFAMProtein)
protein_pfam_search token query =
    let
        url =
            apiBaseUrl ++ "/samples/protein_search/pfam/" ++ query

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoderPFAMProtein))
        |> HttpBuilder.toRequest


protein_kegg_search : String -> String -> Http.Request (List KEGGProtein)
protein_kegg_search token query =
    let
        url =
            apiBaseUrl ++ "/samples/protein_search/kegg/" ++ query

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
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


updateFile : String -> Int -> Int -> Int -> Http.Request String
updateFile token sample_id sample_file_id sample_file_type_id =
    let
        url =
            apiBaseUrl ++ "/samples/" ++ (toString sample_id) ++ "/files/" ++ (toString sample_file_id)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "type_id" => Encode.int sample_file_type_id
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Decode.string)
        |> HttpBuilder.toRequest
