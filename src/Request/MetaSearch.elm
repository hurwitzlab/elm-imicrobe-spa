module Request.MetaSearch exposing (getParams)

import Data.MetaSearch as MetaSearch exposing (SearchResult)
import Dict
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Util exposing (apiHost)


get : String -> Http.Request (List SearchResult)
get query =
    let
        url =
            apiHost ++ "/search/" ++ query

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
            apiHost ++ "/search_params"

        decoder =
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest

