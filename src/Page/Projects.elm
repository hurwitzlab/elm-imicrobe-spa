module Page.Projects exposing (Model, Msg, init, update, view)

import Dict
import Exts.Dict as EDict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Project
import Route
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , projects : List (Dict.Dict String String)
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Projects"

        loadProjects =
            Request.Project.list |> Http.toTask

        handleLoadError err =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home (toString err)
    in
    Task.map2 Model title loadProjects
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
            , div [] [ viewProjects model.projects ]
            ]
        ]


viewProjects projects =
    case List.length projects of
        0 ->
            text "No projects"

        _ ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Institution" ]
                        ]
                    ]
                , tbody []
                    (List.map rowProject projects)
                ]


rowProject project =
    let
        id_s =
            EDict.getWithDefault "0" "project_id" project

        id =
            case String.toInt id_s of
                Ok i ->
                    i

                _ ->
                    0

        name =
            EDict.getWithDefault "NA" "project_name" project

        inst =
            EDict.getWithDefault "NA" "institution" project
    in
    tr []
        -- [ td [] [ a [ Route.href (Route.P id) ] [ text name ] ]
        [ td [] [ text name ]
        , td [] [ text inst ]
        ]
