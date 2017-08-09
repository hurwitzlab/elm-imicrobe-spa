module Request.Assembly exposing (get, list)

import Data.Assembly as Assembly exposing (Assembly)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Util exposing (apiHost)


list : Http.Request (List Assembly)
list =
    let
        url =
            apiHost ++ "/assemblies"

        decoder =
            Decode.list Assembly.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Assembly
get id =
    let
        url =
            apiHost ++ "/assemblies/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson Assembly.decoder)
        |> HttpBuilder.toRequest
