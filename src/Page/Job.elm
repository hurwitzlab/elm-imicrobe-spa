module Page.Job exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Agave as Agave
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Agave
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , job_id : String
    , job : Agave.Job
    }


init : Session -> String -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Job"

        loadJob =
            Request.Agave.getJob session.token id |> Http.toTask |> Task.map .result
    in
    Task.map3 Model title (Task.succeed id) loadJob
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
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.job.name ]
                    ]
                ]
            , viewJob model.job
            ]
        ]


viewJob : Agave.Job -> Html msg
viewJob job =
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text job.name ]
            ]
        , tr []
            [ th [] [ text "App" ]
            , td [] [ text job.app_id ]
            ]
        , tr []
            [ th [] [ text "Start Time" ]
            , td [] [ text job.startTime ]
            ]
        , tr []
            [ th [] [ text "End Time" ]
            , td [] [ text job.endTime ]
            ]
        , tr []
            [ th [] [ text "Status" ]
            , td [] [ text job.status ]
            ]
        ]