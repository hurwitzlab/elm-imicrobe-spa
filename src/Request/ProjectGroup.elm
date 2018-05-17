module Request.ProjectGroup exposing (..)

import Data.ProjectGroup as ProjectGroup exposing (ProjectGroup)
import Http
import HttpBuilder
import Json.Decode as Decode
import Config exposing (apiBaseUrl)


list : Http.Request (List ProjectGroup)
list =
    let
        url =
            apiBaseUrl ++ "/project_groups"

        decoder =
            Decode.list ProjectGroup.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request ProjectGroup
get id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson ProjectGroup.decoder)
        |> HttpBuilder.toRequest


searchByName : String -> Http.Request (List ProjectGroup)
searchByName term =
    let
        url =
            apiBaseUrl ++ "/project_groups"

        decoder =
            Decode.list ProjectGroup.decoder

        queryParams =
            [("term", term)]
    in
    HttpBuilder.get url
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
