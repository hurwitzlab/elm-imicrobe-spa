module Page.ProjectGroup exposing (Model, Msg, init, update, view)

import Data.ProjectGroup
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.ProjectGroup
import Route
import Task exposing (Task)
import View.Page as Page



---- MODEL ----


type alias Model =
    { pageTitle : String
    , projectGroupId : Int
    , projectGroup : Data.ProjectGroup.ProjectGroup
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Project Group"

        loadProjectGroup =
            Request.ProjectGroup.get id |> Http.toTask
    in
    Task.map3 Model title (Task.succeed id) loadProjectGroup
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
                        [ text model.projectGroup.group_name ]
                    ]
                ]
            , viewProjectGroup model.projectGroup
            , viewProjects model.projectGroup.projects
            ]
        ]


viewProjectGroup : Data.ProjectGroup.ProjectGroup -> Html msg
viewProjectGroup group =
    table [ class "table" ]
        [ tr []
            [ th [ class "nowrap" ] [ text "Group Name" ]
            , td [] [ text group.group_name ]
            ]
        , tr []
            [ th [] [ text "Description" ]
            , td [] [ text group.description ]
            ]
        , tr []
            [ th [] [ text "URL" ]
            , td [] [ viewUrl group.url ]
            ]
        ]


viewUrl : String -> Html msg
viewUrl url =
    case String.startsWith "http" url of
        True ->
            a [ href url ] [ text url ]

        _ ->
            text url


viewProjects : List Data.ProjectGroup.Project -> Html msg
viewProjects projects =
    let
        numProjects =
            List.length projects

        label =
            case numProjects of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numProjects)
                        ]

        body =
            case numProjects of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (List.map viewProject projects) ]
    in
    div []
        [ h2 []
            [ text "Projects "
            , label
            ]
        , body
        ]


viewProject : Data.ProjectGroup.Project -> Html msg
viewProject project =
    tr []
        [ td []
            [ a [ Route.href (Route.Project project.project_id) ]
                [ text project.project_name ]
            ]
        ]

