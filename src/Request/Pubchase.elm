module Request.Pubchase exposing (list)

import Data.Pubchase as Pubchase exposing (Article)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Config exposing (apiBaseUrl)


list : Http.Request (List Article)
list =
    let
        url =
            apiBaseUrl ++ "/pubchase"

        decoder =
            Decode.list Pubchase.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
