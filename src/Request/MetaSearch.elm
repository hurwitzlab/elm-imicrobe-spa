module Request.MetaSearch exposing (getParams)

import Data.MetaSearch as MetaSearch exposing (SearchResult)
import Dict
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
            Decode.list MetaSearch.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


getParams : Http.Request (Dict.Dict String String)
getParams =
    let
        url =
            apiBaseUrl ++ "/search_params"

        decoder =
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest

