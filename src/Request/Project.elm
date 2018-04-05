module Request.Project exposing (..)

import Data.Project as Project exposing (Project, Domain, ProjectGroup, Assembly, CombinedAssembly, encodeDomain, encodeProjectGroup)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Util exposing ((=>))
import Config exposing (apiBaseUrl)


list : Http.Request (List Project)
list =
    let
        url =
            apiBaseUrl ++ "/projects"

        decoder =
            Decode.list Project.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : String -> Int -> Http.Request Project
get token id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ toString id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson Project.decoder)
        |> HttpBuilder.toRequest


getAssemblies : Int -> Http.Request (List Assembly)
getAssemblies id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString id) ++ "/assemblies"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list Project.decoderAssembly))
        |> HttpBuilder.toRequest


getCombinedAssemblies : Int -> Http.Request (List CombinedAssembly)
getCombinedAssemblies id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString id) ++ "/combined_assemblies"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list Project.decoderCombinedAssembly))
        |> HttpBuilder.toRequest


create : String -> String -> Http.Request Project
create token project_name =
    let
        url =
            apiBaseUrl ++ "/projects"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "project_name" => Encode.string project_name ]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Project.decoder)
        |> HttpBuilder.toRequest


update : String -> Int -> String -> String -> String -> String -> List Domain -> List ProjectGroup -> Http.Request Project
update token project_id project_name project_code project_type project_url domains groups =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString project_id)

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ "project_name" => Encode.string project_name
                , "project_code" => Encode.string project_code
                , "project_type" => Encode.string project_type
                , "project_url" => Encode.string project_url
                , "domains" => Encode.list (List.map encodeDomain domains)
                , "groups" => Encode.list (List.map encodeProjectGroup groups)
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson Project.decoder)
        |> HttpBuilder.toRequest


remove : String -> Int -> Http.Request String
remove token project_id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString project_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest


addInvestigatorToProject : String -> Int -> Int -> Http.Request Project
addInvestigatorToProject token project_id investigator_id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString project_id) ++ "/investigators/" ++ (toString investigator_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.put url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson Project.decoder)
        |> HttpBuilder.toRequest


removeInvestigatorFromProject : String -> Int -> Int -> Http.Request String
removeInvestigatorFromProject token project_id investigator_id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ (toString project_id) ++ "/investigators/" ++ (toString investigator_id)

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.delete url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect Http.expectString
        |> HttpBuilder.toRequest
