module Request.Sample exposing (get, list)

import Data.Sample as Sample exposing (Sample)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode


host : String
host =
    -- "https://www.imicrobe.us
    "http://localhost:3006"


list : Http.Request (List Sample)
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


get : Int -> Http.Request Sample
get id =
    HttpBuilder.get (host ++ "/samples/" ++ toString id)
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest
