module Request.ProjectGroup exposing (..)

import Data.ProjectGroup as ProjectGroup exposing (ProjectGroup, Project, User, decoder, decoderProject, decoderUser)
import Http
import HttpBuilder
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))
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


addProject : String -> Int -> Int -> Http.Request ProjectGroup
addProject token project_group_id project_id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ (toString project_group_id) ++ "/projects/" ++ (toString project_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


removeProject : String -> Int -> Int -> Http.Request ProjectGroup
removeProject token project_group_id project_id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ (toString project_group_id) ++ "/projects/" ++ (toString project_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


addUser : String -> Int -> Int -> String -> Http.Request (List User)
addUser token project_group_id user_id permission =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ (toString project_group_id) ++ "/users/" ++ (toString user_id)

        decoder =
            Decode.list decoderUser

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "permission" => Encode.string permission ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


removeUser : String -> Int -> Int -> Http.Request (List User)
removeUser token project_group_id user_id =
    let
        url =
            apiBaseUrl ++ "/project_groups/" ++ (toString project_group_id) ++ "/users/" ++ (toString user_id)

        headers =
            [( "Authorization", token)]

        decoder =
            Decode.list decoderUser
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest