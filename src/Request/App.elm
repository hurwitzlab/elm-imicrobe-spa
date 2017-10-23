module Request.App exposing (get, list, run)

import Data.App as App exposing (App, AppRun, decoderAppRun, encodeAppRun)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



list : Http.Request (List App)
list =
    let
        url =
            apiBaseUrl ++ "/apps"

        decoder =
            Decode.list App.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get : Int -> Http.Request App
get id =
    HttpBuilder.get (apiBaseUrl ++ "/apps/" ++ toString id)
        |> HttpBuilder.withExpect (Http.expectJson App.decoder)
        |> HttpBuilder.toRequest


run : Int -> Int -> String -> Http.Request AppRun
run app_id user_id params =
    let
        url =
            apiBaseUrl ++ "/apps/runs"

        body =
            Encode.object
                [ ("app_id", Encode.int app_id)
                , ("user_id", Encode.int user_id)
                , ("params", Encode.string params)
                ]
    in
    HttpBuilder.post url
        |> HttpBuilder.withJsonBody body
        |> HttpBuilder.withExpect (Http.expectJson decoderAppRun)
        |> HttpBuilder.toRequest