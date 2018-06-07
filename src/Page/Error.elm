module Page.Error exposing (PageLoadError, pageLoadError, handleLoadError, redirectLoadError, errorMessage, view)

{-| The page that renders when there was an error trying to load another page,
for example a Page Not Found error.
-}

import Html exposing (Html, div, h1, img, main_, a, p, text)
import Html.Attributes exposing (alt, class, id, tabindex)
import View.Page as Page exposing (ActivePage)
import Route
import Http



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


errorMessage : Http.Error -> Html msg
errorMessage error =
    case error of
        Http.NetworkError ->
            text "Cannot connect to remote host"

        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    text "Unauthorized"

                403 ->
                    div []
                        [ text "You do not have access to this resource.  Please make sure you are "
                        , a [ Route.href Route.Login ] [ text "logged-in"]
                        , text " and try again."
                        ]

                _ ->
                    case String.length response.body of
                        0 ->
                            text "Bad status"

                        _ ->
                            text response.body

        _ ->
            toString error |> text



-- VIEW --


view : PageLoadError -> Html msg
view (PageLoadError model) =
    main_ [ id "content", class "container", tabindex -1 ]
        [ div [ class "page-header" ]
            [ h1 [] [ text "Error" ] ]
        , div [ class "row" ]
            [ div [ class "alert alert-danger" ] [ (errorMessage model.error) ] ]
        ]