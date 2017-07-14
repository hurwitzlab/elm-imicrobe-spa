module Request.Sample exposing (get, list)

import Data.Sample as Sample exposing (Sample)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode


host =
    -- "https://www.imicrobe.us
    "http://localhost:3006"


list =
    let
        url =
            host ++ "/samples"

        decoder =
            Decode.list Sample.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get id =
    let
        url =
            host ++ "/samples/" ++ toString id

        decoder =
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
