module Request.Investigator exposing (get, list)

import Data.Investigator as Investigator exposing (Investigator)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra


-- list : Http.Request (List Investigator)


list =
    let
        url =
            "https://www.imicrobe.us/investigator/list.json"

        decoder =
            Decode.list (Decode.dict Decode.string)
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest



-- get : Int -> Http.Request Profile


get id =
    let
        url =
            "https://www.imicrobe.us/investigator/view" ++ toString id ++ ".json"

        decoder =
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
