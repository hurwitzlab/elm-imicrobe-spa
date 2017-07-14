module Request.Project exposing (get, list)

import Data.Project as Project exposing (Project)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode


host =
    -- "https://www.imicrobe.us
    "http://localhost:3006"



-- list : Http.Request (List Project)


list =
    let
        url =
            -- "https://www.imicrobe.us/project/list.json"
            host ++ "/projects"

        decoder =
            -- Decode.list (Decode.dict Decode.string)
            Decode.list Project.decoder
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest



-- get : Int -> Http.Request Profile


get id =
    let
        url =
            -- "https://www.imicrobe.us/project/view/" ++ toString id ++ ".json"
            host ++ "/projects/" ++ toString id

        decoder =
            Decode.dict Decode.string
    in
    HttpBuilder.get url
        |> HttpBuilder.withExpect (Http.expectJson decoder)
        |> HttpBuilder.toRequest
