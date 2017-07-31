module Request.ProjectGroup exposing (get, list)

import Data.ProjectGroup as ProjectGroup exposing (ProjectGroup)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Json.Encode as Encode
import Json.Encode.Extra as EncodeExtra


host : String
host =
    "http://localhost:3006"



-- list : Http.Request (List ProjectGroup)


list =
    let
        url =
            host ++ "/project_groups"

        decoder =
            Decode.list ProjectGroup.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest


get id =
    let
        url =
            host ++ "/project_groups/" ++ toString id

        decoder =
            ProjectGroup.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
