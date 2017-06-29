module Request.Investigator exposing (list)

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
            -- Decode.list Investigator
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest



{--
get : Int -> Http.Request Profile
get username maybeToken =
    apiUrl ("/profiles/" ++ User.usernameToString username)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" Profile.decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest
--}
