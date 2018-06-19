module Page.Profile exposing (Model, Msg, init, update, view)

import Data.Agave as Agave exposing (Profile)
import Data.User as User exposing (User)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Request.User
import Task exposing (Task)
import OAuth
import OAuth.AuthorizationCode
import Config exposing (orcidOAuthUrl, orcidClientId)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , profile : Profile
    , user : User
    , redirectUri : String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadProfile =
            Request.Agave.getProfile session.token |> Http.toTask |> Task.map .result

        loadUser username =
            Request.User.getByUsername session.token username |> Http.toTask
    in
    loadProfile
        |> Task.andThen
            (\profile ->
                (loadUser profile.username
                    |> Task.andThen
                        (\user ->
                            Task.succeed
                                { pageTitle = "Profile"
                                , profile = profile
                                , user = user
                                , redirectUri = session.url
                            }
                        )
                )
            )
            |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = ORCIDLogin


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ORCIDLogin ->
            model
                ! [ OAuth.AuthorizationCode.authorize
                        { clientId = Config.orcidClientId
                        , redirectUri = model.redirectUri
                        , responseType = OAuth.Code
                        , scope = [ "/authenticate" ]
                        , state = Just "000"
                        , url = Config.orcidOAuthUrl
                        }
                  ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        profile =
            model.profile

        orcid =
            if model.user.orcid == "" then
                button [ class "btn btn-default btn-xs", onClick ORCIDLogin ] [ text "Create or connect an ORCID" ]
            else
                text model.user.orcid
    in
    div [ class "container" ]
        [ div [ class "page-header" ]
            [ h1 [] [ text model.pageTitle ] ]
        , table [ class "table" ]
            [ colgroup []
                [ col [ class "col-md-2" ] [] ]
            , tr []
                [ th [] [ text "Username" ]
                , td [] [ text profile.username ]
                ]
            , tr []
                [ th [] [ text "Full name" ]
                , td [] [ text (profile.first_name ++ " " ++ profile.last_name) ]
                ]
            , tr []
                [ th [] [ text "ORCID" ]
                , td [] [ orcid ]
                ]
            ]
        , div [ class "alert alert-info" ]
            [ p []
                [ text "The user information shown above was obtained from your "
                , a [ href "http://www.cyverse.org/", target "_blank" ] [ text "CyVerse" ]
                , text " account."
                ]
            , p []
                [ text "For details please see the "
                , a [ href "https://user.cyverse.org/", target "_blank" ] [ text "CyVerse User Portal" ]
                , text "."
                ]
            ]
        ]
