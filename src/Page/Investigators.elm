module Page.Investigators exposing (Model, Msg, init, update, view)

import Dict
import Exts.Dict as EDict
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
    , investigators : List (Dict.Dict String String)
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Investigators"

        loadInvestigators =
            Request.Investigator.list |> Http.toTask

        handleLoadError err =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home (toString err)
    in
    Task.map2 Model title loadInvestigators
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
            , div [] [ viewInvestigators model.investigators ]
            ]
        ]


viewInvestigators invs =
    case List.length invs of
        0 ->
            text "No investigators"

        _ ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Institution" ]
                        ]
                    ]
                , tbody []
                    (List.map rowInv invs)
                ]


rowInv inv =
    let
        id =
            EDict.getWithDefault "0" "investigator_id" inv

        name =
            EDict.getWithDefault "NA" "investigator_name" inv

        inst =
            EDict.getWithDefault "NA" "institution" inv
    in
    tr []
        [ td [] [ a [ href ("/investigators/" ++ id) ] [ text name ] ]
        , td [] [ text inst ]
        ]
