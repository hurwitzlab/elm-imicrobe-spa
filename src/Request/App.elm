module Request.App exposing (get, list)

import Data.App as App exposing (App)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Util exposing (apiHost)


list : Http.Request (List App)
list =
    let
        url =
            apiHost ++ "/apps"

        decoder =
            Decode.list App.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request App
get id =
    HttpBuilder.get (apiHost ++ "/apps/" ++ toString id)
        |> HttpBuilder.withExpect (Http.expectJson App.decoder)
        |> HttpBuilder.toRequest
