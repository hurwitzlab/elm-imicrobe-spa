module Request.ORCID exposing (..)

import Data.ORCID exposing (TokenResponse, Record, decoderTokenResponse, decoderRecord)
import Http
import HttpBuilder
import Config exposing (apiBaseUrl, orcidBaseUrl)



authenticate : String -> String -> Maybe Int -> String -> Http.Request TokenResponse
authenticate provider code user_id redirect_uri =
    let
        body =
            [ ("provider", provider)
            , ("code", code)
            , ("user_id", Maybe.withDefault 0 user_id |> toString)
            , ("redirect_uri", redirect_uri)
            ]
    in
    HttpBuilder.post (apiBaseUrl ++ "/authenticate")
        |> HttpBuilder.withUrlEncodedBody body
        |> HttpBuilder.withExpect (Http.expectJson decoderTokenResponse)
        |> HttpBuilder.toRequest


getRecord : String -> String -> Http.Request Record
getRecord id token =
    let
        headers =
            [( "Authorization", token )]
    in
    HttpBuilder.get (orcidBaseUrl ++ "/" ++ id ++ "/record")
        |> HttpBuilder.withHeaders headers
        |> HttpBuilder.withExpect (Http.expectJson decoderRecord)
        |> HttpBuilder.toRequest
