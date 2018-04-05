module Request.User exposing (..)

import Data.User as User exposing (User, Login)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



list : String -> Http.Request (List User)
list token =
    let
        url =
            apiBaseUrl ++ "/users"

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson (Decode.list User.decoder))
        |> HttpBuilder.toRequest


get : String -> Int -> Http.Request User
get token id =
    let
        url =
            apiBaseUrl ++ "/users/" ++ toString id

        headers =
            [( "Authorization", token)]
    in
    HttpBuilder.get url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson User.decoder)
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
        |> HttpBuilder.withExpect (Http.expectJson User.decoder)
        |> HttpBuilder.toRequest


recordLogin : String -> String -> Http.Request Login
recordLogin token username =
    let
        url =
            apiBaseUrl ++ "/users/login"

        headers =
            [( "Authorization", token)]

        body =
            Encode.object
                [ ("user_name", Encode.string username)
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson User.decoderLogin)
        |> HttpBuilder.toRequest
