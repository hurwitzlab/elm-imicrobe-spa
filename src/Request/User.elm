module Request.User exposing (..)

import Data.User as User exposing (User, Login)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



list : Http.Request (List User)
list =
    let
        url =
            apiBaseUrl ++ "/users"

        decoder =
            Decode.list User.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request User
get id =
    HttpBuilder.get (apiBaseUrl ++ "/users/" ++ toString id)
        |> HttpBuilder.withExpect (Http.expectJson User.decoder)
        |> HttpBuilder.toRequest


getByUsername : String -> Http.Request User
getByUsername name =
    HttpBuilder.get (apiBaseUrl ++ "/users/" ++ name)
        |> HttpBuilder.withExpect (Http.expectJson User.decoder)
        |> HttpBuilder.toRequest


recordLogin : String -> Http.Request Login
recordLogin username =
    let
        url =
            apiBaseUrl ++ "/login"

        body =
            Encode.object
                [ ("user_name", Encode.string username)
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson User.decoderLogin)
        |> HttpBuilder.toRequest
