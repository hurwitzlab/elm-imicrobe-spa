module Request.CombinedAssembly exposing (get, list)

import Data.CombinedAssembly as CombinedAssembly exposing (CombinedAssembly)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Config exposing (apiBaseUrl)


list : Http.Request (List CombinedAssembly)
list =
    let
        url =
            apiBaseUrl ++ "/combined_assemblies"

        decoder =
            Decode.list CombinedAssembly.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request CombinedAssembly
get id =
    let
        url =
            apiBaseUrl ++ "/combined_assemblies/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson CombinedAssembly.decoder)
        |> HttpBuilder.toRequest
