module Request.Investigator exposing (get, list)

import Data.Investigator as Investigator exposing (Investigator)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra
import Config exposing (apiBaseUrl)


list : Http.Request (List Investigator)
list =
    let
        url =
            apiBaseUrl ++ "/investigators"

        decoder =
            Decode.list Investigator.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Investigator
get id =
    let
        url =
            apiBaseUrl ++ "/investigators/" ++ toString id

        decoder =
            Investigator.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
