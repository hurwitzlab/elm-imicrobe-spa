module Request.Login exposing (Login, record)

import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode
import Config exposing (apiBaseUrl)



type alias Login =
    { login_id : Int
    , user_id : Int
    , login_date : String
    }


decoder : Decoder Login
decoder =
    decode Login
        |> required "login_id" Decode.int
        |> required "user_id" Decode.int
        |> optional "login_date" Decode.string ""


record : String -> Http.Request Login
record username =
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
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest