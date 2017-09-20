module Request.Search exposing (get)

import Data.Search as Search exposing (SearchResult)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Config exposing (apiBaseUrl)


get : String -> Http.Request (List SearchResult)
get query =
    let
        url =
            apiBaseUrl ++ "/search/" ++ query

        decoder =
            Decode.list Search.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
