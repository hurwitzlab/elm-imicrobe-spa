module Request.Project exposing (..)

import Data.Project as Project exposing (Project, Assembly, CombinedAssembly)
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


get : Int -> Http.Request Project
get id =
    let
        url =
            apiBaseUrl ++ "/projects/" ++ toString id
    in
    HttpBuilder.get url
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
