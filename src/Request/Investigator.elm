module Request.Investigator exposing (..)

import Data.Investigator as Investigator exposing (Investigator)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Config exposing (apiBaseUrl)


list : Http.Request (List Investigator)
list =
    let
        url =
            apiBaseUrl ++ "/investigators"
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list Investigator.decoder))
        |> HttpBuilder.toRequest


get : Int -> Http.Request Investigator
get id =
    let
        url =
            apiBaseUrl ++ "/investigators/" ++ toString id
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Investigator.decoder))
        |> HttpBuilder.toRequest


searchByName : String -> Http.Request (List Investigator)
searchByName term =
    let
        url =
            apiBaseUrl ++ "/investigators/" ++ term
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list Investigator.decoder))
        |> HttpBuilder.toRequest
