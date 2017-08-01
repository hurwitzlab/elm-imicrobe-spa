module Request.Domain exposing (get, list)

import Data.Domain as Domain exposing (Domain)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Util exposing (apiHost)


list : Http.Request (List Domain)
list =
    let
        url =
            apiHost ++ "/domains"

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
            apiHost ++ "/domains/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson Domain.decoder)
        |> HttpBuilder.toRequest
