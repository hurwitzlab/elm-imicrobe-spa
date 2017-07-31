module Page.ProjectGroup exposing (Model, Msg, init, update, view)

import Data.ProjectGroup
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
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

        handleLoadError err =
            -- If a resource task fail load error page
            let
                errMsg =
                    case err of
                        Http.BadStatus response ->
                            case String.length response.body of
                                0 ->
                                    "Bad status"

                                _ ->
                                    response.body

                        _ ->
                            toString err
            in
            Error.pageLoadError Page.Home errMsg
    in
    Task.map3 Model title (Task.succeed id) loadProjectGroup
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
            , viewProjectGroup model.projectGroup
            ]
        ]


viewProjectGroup : Data.ProjectGroup.ProjectGroup -> Html msg
viewProjectGroup group =
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Group Name" ]
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
        , tr []
            [ th []
                [ text
                    ("Projects ("
                        ++ toString (List.length group.projects)
                        ++ ")"
                    )
                ]
            , td [] [ viewProjects group.projects ]
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
    case List.length projects of
        0 ->
            text "None"

        _ ->
            table [] (List.map projectRow projects)


projectRow : Data.ProjectGroup.Project -> Html msg
projectRow project =
    tr []
        [ th [] [ text "Project" ]
        , td []
            [ a [ Route.href (Route.Project project.project_id) ]
                [ text project.project_name ]
            ]
        ]
