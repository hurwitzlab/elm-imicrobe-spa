module Page.Error exposing (PageLoadError, pageLoadError, handleLoadError, handleLoadErrorWithLogin, redirectLoadError, statusCode, errorString, errorMessage, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Html exposing (Html, div, h1, img, main_, a, p, text)
import Html.Attributes exposing (alt, class, id, tabindex, href)
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
    , isLoggedIn : Bool
    }


pageLoadError : ActivePage -> Http.Error -> Bool -> PageLoadError
pageLoadError activePage error isLoggedIn =
    PageLoadError { activePage = activePage, error = error, isLoggedIn = isLoggedIn }


handleLoadError : Http.Error -> PageLoadError
handleLoadError error =
    pageLoadError Page.Home error False


handleLoadErrorWithLogin : Bool -> Http.Error -> PageLoadError
handleLoadErrorWithLogin isLoggedIn error =
    pageLoadError Page.Home error isLoggedIn


redirectLoadError : PageLoadError -> Cmd msg
redirectLoadError (PageLoadError model) =
    case model.error of
        Http.BadStatus response ->
            case response.status.code of
                401 -> Route.modifyUrl Route.Login -- redirect to Login page

                _ -> Cmd.none

        _ -> Cmd.none


statusCode : PageLoadError -> Maybe Int
statusCode (PageLoadError model) =
    case model.error of
        Http.BadStatus response ->
            Just response.status.code

        _ ->
            Nothing


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


errorMessage : Model -> Html msg
errorMessage model =
    let
        loginMessage =
            if model.isLoggedIn then
                text ""
            else
                p []
                    [ text "Please "
                    , a [ Route.href Route.Login ] [ text "sign-in"]
                    , text " and try again."
                    ]
    in
    case model.error of
        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    div []
                        [ p [] [ text "Access to this part of the site requires a CyVerse account." ]
                        , p []
                            [ text "If you do not have an account, please sign up at the "
                            , a [ href "https://user.cyverse.org/" ] [ text "CyVerse User Portal" ]
                            , text "."
                            ]
                        , p [] [ text "You will be redirected to the CyVerse login page in a few seconds ..." ]
                        ]

                403 ->
                    div []
                        [ p []
                            [ text "You do not have permission to access this resource." ]
                        , loginMessage
                        ]

                _ ->
                    errorString model.error |> text

        _ ->
            errorString model.error |> text



-- VIEW --


view : PageLoadError -> Html msg
view (PageLoadError model) =
    main_ [ id "content", class "container", tabindex -1 ]
        [ div [ class "page-header" ]
            [ h1 [] [ text "Error" ] ]
        , div [ class "row" ]
            [ div [ class "alert alert-danger" ] [ (errorMessage model) ] ]
        ]