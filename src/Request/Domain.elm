module Request.Domain exposing (get, list)

import Data.Domain as Domain exposing (Domain)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode


host : String
host =
    "http://localhost:3006"


list : Http.Request (List Domain)
list =
    let
        url =
            host ++ "/domains"

        decoder =
            Decode.list Domain.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Domain
get id =
    let
        url =
            host ++ "/domains/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson Domain.decoder)
        |> HttpBuilder.toRequest
