module Request.Sample exposing (list, get, getSome)

import Data.Sample as Sample exposing (Sample)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import String exposing (join)
import Util exposing (apiHost)


list : Http.Request (List Sample)
list =
    let
        url =
            apiHost ++ "/samples"

        decoder =
            Decode.list Sample.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request Sample
get id =
    HttpBuilder.get (apiHost ++ "/samples/" ++ toString id)
        |> HttpBuilder.withExpect (Http.expectJson Sample.decoder)
        |> HttpBuilder.toRequest

getSome : List Int -> Http.Request (List Sample)
getSome id_list =
    let
        url =
            apiHost ++ "/samples/?id=" ++ (join "," (List.map toString id_list))

        decoder =
            Decode.list Sample.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest