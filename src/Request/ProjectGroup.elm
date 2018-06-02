module Request.ProjectGroup exposing (..)

import Data.ProjectGroup as ProjectGroup exposing (ProjectGroup)
import Http
import HttpBuilder
import Json.Decode as Decode
import Config exposing (apiBaseUrl)


list : String -> Http.Request (List ProjectGroup)
list token =
    let
        url =
            apiBaseUrl ++ "/project_groups"

        headers =
            [( "Authorization", token)]

        decoder =
            Decode.list ProjectGroup.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : String -> Int -> Http.Request ProjectGroup
get token id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ toString id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson ProjectGroup.decoder)
        |> HttpBuilder.toRequest


searchByName : String -> String -> Http.Request (List ProjectGroup)
searchByName token term =
    let
        url =
            apiBaseUrl ++ "/project_groups"

        headers =
            [( "Authorization", token)]

        decoder =
            Decode.list ProjectGroup.decoder

        queryParams =
            [("term", term)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


addProject : String -> Int -> Int -> Http.Request (List ProjectGroup)
addProject token project_group_id project_id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ (toString project_group_id) ++ "/projects/" ++ (toString project_id)

        decoder =
            Decode.list ProjectGroup.decoder

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


removeProject : String -> Int -> Int -> Http.Request String
removeProject token project_group_id project_id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ (toString project_group_id) ++ "/projects/" ++ (toString project_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson Decode.string)
        |> HttpBuilder.toRequest
