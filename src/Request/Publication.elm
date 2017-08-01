module Request.Publication exposing (get, list)

import Data.Publication as Publication exposing (Publication)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Util exposing (apiHost)


list : Http.Request (List Publication)
list =
    let
        url =
            apiHost ++ "/publications"

        decoder =
            Decode.list Publication.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Publication
get id =
    let
        url =
            apiHost ++ "/publications/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson Publication.decoder)
        |> HttpBuilder.toRequest
