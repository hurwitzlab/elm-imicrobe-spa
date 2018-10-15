module Page.Error exposing (PageLoadError, pageLoadError, handleLoadError, redirectLoadError, errorString, errorMessage, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Html exposing (Html, div, h1, img, main_, a, p, text)
import Html.Attributes exposing (alt, class, id, tabindex)
import View.Page as Page exposing (ActivePage)
import Route
import Http
import Json.Decode as Decode
import Data.Agave exposing (decoderJobError)



-- MODEL --


type PageLoadError
    = PageLoadError Model


type alias Model =
    { activePage : ActivePage
    , error : Http.Error
    }


pageLoadError : ActivePage -> Http.Error -> PageLoadError
pageLoadError activePage error =
    PageLoadError { activePage = activePage, error = error }


handleLoadError : Http.Error -> PageLoadError
handleLoadError error =
    pageLoadError Page.Home error


redirectLoadError : PageLoadError -> Cmd msg
redirectLoadError (PageLoadError model) =
    case model.error of
        Http.BadStatus response ->
            case response.status.code of
                401 -> Route.modifyUrl Route.Login -- redirect to Login page

                _ -> Cmd.none

        _ -> Cmd.none


errorString : Http.Error -> String
errorString error =
    case error of
        Http.NetworkError ->
            "Cannot connect to remote host"

        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    "Unauthorized"

                403 ->
                    "Permission denied"

                _ ->
                    if String.length response.body == 0 then
                        "Bad status"
                    else
                        case Decode.decodeString decoderJobError response.body of
                            Ok result ->
                                result.message

                            _ ->
                                response.body

        _ ->
            toString error


errorMessage : Http.Error -> Html msg
errorMessage error =
    case error of
        Http.BadStatus response ->
            case response.status.code of
                403 ->
                    div []
                        [ text "You do not have access to this resource.  Please make sure you are "
                        , a [ Route.href Route.Login ] [ text "logged-in"]
                        , text " and try again."
                        ]

                _ ->
                    errorString error |> text

        _ ->
            errorString error |> text



-- VIEW --


view : PageLoadError -> Html msg
view (PageLoadError model) =
    main_ [ id "content", class "container", tabindex -1 ]
        [ div [ class "page-header" ]
            [ h1 [] [ text "Error" ] ]
        , div [ class "row" ]
            [ div [ class "alert alert-danger" ] [ (errorMessage model.error) ] ]
        ]