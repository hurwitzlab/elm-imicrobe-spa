module Request.Project exposing (get, list)

import Data.Project as Project exposing (Project)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
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
