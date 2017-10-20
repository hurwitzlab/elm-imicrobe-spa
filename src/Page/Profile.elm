module Page.Profile exposing (Model, Msg, init, update, view)

import Data.Profile as Profile exposing (Profile)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , token : String
    , profile : Profile
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Profile"

        loadProfile =
            Request.Agave.getProfile session.token |> Http.toTask |> Task.map .result
    in
    Task.map3 Model title (Task.succeed session.token) loadProfile
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    let
        profile =
            model.profile
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
            ]
        , div [ class "alert alert-info" ]
            [ p []
                [ text "The user information shown above was obtained from your "
                , a [ href "http://www.cyverse.org/" ] [ text "CyVerse" ]
                , text " account."
                ]
            , p []
                [ text "For details please see the "
                , a [ href "https://user.cyverse.org/" ] [ text "CyVerse User Portal" ]
                , text "."
                ]
            ]
        ]
