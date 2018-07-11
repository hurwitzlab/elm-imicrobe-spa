module Request.User exposing (..)

import Data.User as User exposing (..)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder, string)
import Config exposing (apiBaseUrl)



get : String -> Http.Request User
get token =
    let
        url =
            apiBaseUrl ++ "/users"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


getByUsername : String -> String -> Http.Request User
getByUsername token name =
    let
        url =
            apiBaseUrl ++ "/users/" ++ name

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


searchByName : String -> String -> Http.Request (List User)
searchByName token term =
    let
        url =
            apiBaseUrl ++ "/users/search"

        headers =
            [( "Authorization", token)]

        queryParams =
            [("term", term)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withQueryParams queryParams
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list decoder))
        |> HttpBuilder.toRequest


recordLogin : String -> Http.Request Login
recordLogin token =
    let
        url =
            apiBaseUrl ++ "/users/login"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoderLogin)
        |> HttpBuilder.toRequest
