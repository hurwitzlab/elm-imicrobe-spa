module Page.Investigator exposing (Model, Msg, init, update, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Investigator
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , investigator_id : Int
    , investigator : Dict.Dict String String
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Investigator"

        loadInvestigator =
            Request.Investigator.get id |> Http.toTask

        handleLoadError err =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home (toString err)
    in
    Task.map3 Model title (Task.succeed id) loadInvestigator
        |> Task.mapError handleLoadError



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
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ text (toString model.investigator) ]
            ]
        ]
