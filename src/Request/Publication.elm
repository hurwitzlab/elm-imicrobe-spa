module Request.Publication exposing (..)

import Data.Publication as Publication exposing (Publication)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EncodeExtra
import Util exposing ((=>))
import Config exposing (apiBaseUrl)



list : Http.Request (List Publication)
list =
    let
        url =
            apiBaseUrl ++ "/publications"

        decoder =
            Decode.list Publication.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Publication
get id =
    let
        url =
            apiBaseUrl ++ "/publications/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson Publication.decoder)
        |> HttpBuilder.toRequest


create : String -> Int -> String -> String -> String -> Maybe Int -> String -> Http.Request Publication
create token project_id publication_title publication_authors publication_date publication_pubmed_id publication_doi =
    let
        url =
            apiBaseUrl ++ "/publications"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "project_id" => Encode.int project_id
                , "title" => Encode.string publication_title
                , "authors" => Encode.string publication_authors
                , "date" => Encode.string publication_date
                , "pubmed_id" => EncodeExtra.maybe Encode.int publication_pubmed_id
                , "doi" => Encode.string publication_doi
                ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Publication.decoder)
        |> HttpBuilder.toRequest


update : String -> Int -> String -> String -> String -> Maybe Int -> String -> Http.Request Publication
update token publication_id publication_title publication_authors publication_date publication_pubmed_id publication_doi =
    let
        url =
            apiBaseUrl ++ "/publications/" ++ (toString publication_id)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "title" => Encode.string publication_title
                , "authors" => Encode.string publication_authors
                , "date" => Encode.string publication_date
                , "pubmed_id" => EncodeExtra.maybe Encode.int publication_pubmed_id
                , "doi" => Encode.string publication_doi
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Publication.decoder)
        |> HttpBuilder.toRequest


remove : String -> Int -> Http.Request String
remove token publication_id =
    let
        url =
            apiBaseUrl ++ "/publications/" ++ (toString publication_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest
