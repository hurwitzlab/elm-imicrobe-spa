module Page.ProjectGroup exposing (Model, Msg, init, update, view)

import Data.ProjectGroup exposing (ProjectGroup, Project, User)
import Data.Session exposing (Session)
import Data.User
import Data.Project
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.ProjectGroup
import Route
import Task exposing (Task)
import View.Spinner exposing (spinner)
import View.SearchableDropdown
import View.Dialog exposing (confirmationDialogConfig)
import Request.User
import Request.Project
import Util exposing ((=>), capitalize)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , projectGroupId : Int
    , projectGroup : ProjectGroup
    , isEditable : Bool
    , showAddUserDialog : Bool
    , showAddUserBusy : Bool
    , userDropdownState : View.SearchableDropdown.State
    , showAddProjectDialog : Bool
    , showAddProjectBusy : Bool
    , projectDropdownState : View.SearchableDropdown.State
    , confirmationDialog : Maybe (Dialog.Config Msg)
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        loadProjectGroup =
            Request.ProjectGroup.get session.token id |> Http.toTask

        userId =
            Maybe.map .user_id session.user

        isEditable group =
            case userId of
                Nothing ->
                    False

                Just userId ->
                    group.users
                        |> List.any (\u -> u.user_id == userId && (u.permconn.permission == "owner" || u.permconn.permission == "read-write"))
    in
    loadProjectGroup
        |> Task.andThen
            (\group ->
                Task.succeed
                    { pageTitle = "Project Group"
                    , projectGroupId = group.project_group_id
                    , projectGroup = group
                    , isEditable = isEditable group
                    , showAddUserDialog = False
                    , showAddUserBusy = False
                    , userDropdownState = View.SearchableDropdown.init
                    , showAddProjectDialog = False
                    , showAddProjectBusy = False
                    , projectDropdownState = View.SearchableDropdown.init
                    , confirmationDialog = Nothing
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = OpenAddUserDialog
    | CloseAddUserDialog
    | OpenAddProjectDialog
    | CloseAddProjectDialog
    | SetUserName String
    | SearchUsersCompleted (Result Http.Error (List Data.User.User))
    | AddUser String Int String
    | AddUserCompleted (Result Http.Error (List User))
    | RemoveUser Int
    | RemoveUserCompleted (Result Http.Error (List User))
    | SetProjectName String
    | SearchProjectsCompleted (Result Http.Error (List Data.Project.Project))
    | AddProject Int String
    | AddProjectCompleted (Result Http.Error ProjectGroup)
    | RemoveProject Int
    | RemoveProjectCompleted (Result Http.Error ProjectGroup)
    | OpenConfirmationDialog String Msg
    | CloseConfirmationDialog


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        OpenAddUserDialog ->
            { model | showAddUserDialog = True, showAddUserBusy = False } => Cmd.none

        CloseAddUserDialog ->
            { model | showAddUserDialog = False } => Cmd.none

        OpenAddProjectDialog ->
            { model | showAddProjectDialog = True } => Cmd.none

        CloseAddProjectDialog ->
            { model | showAddProjectDialog = False } => Cmd.none

        SetUserName name ->
            let
                dropdownState =
                    model.userDropdownState
            in
            if String.length name >= 3 then
                let
                    searchByName =
                        Request.User.searchByName session.token name |> Http.toTask
                in
                { model | userDropdownState = { dropdownState | value = name } } => Task.attempt SearchUsersCompleted searchByName
            else
                { model | userDropdownState = { dropdownState | value = name, results = [] } } => Cmd.none

        SearchUsersCompleted (Ok users) ->
            let
                userDisplayName user =
                    user.first_name ++ " " ++ user.last_name ++ " (" ++ user.user_name ++ ")"

                results =
                    List.map (\u -> (u.user_id, userDisplayName u)) users

                dropdownState =
                    model.userDropdownState
            in
            { model | userDropdownState = { dropdownState | results = results } } => Cmd.none

        SearchUsersCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "SearchUsersCompleted" (toString error)
            in
            model => Cmd.none

        AddUser permission id _ ->
            let
                dropdownState =
                    model.userDropdownState

                addUser =
                    Request.ProjectGroup.addUser session.token model.projectGroupId id permission |> Http.toTask
            in
            { model | showAddUserBusy = True, userDropdownState = { dropdownState | value = "", results = [] } } => Task.attempt AddUserCompleted addUser

        AddUserCompleted (Ok users) ->
            let
                projectGroup =
                    model.projectGroup
            in
            { model | showAddUserDialog = False, projectGroup = { projectGroup | users = users } } => Cmd.none

        AddUserCompleted (Err error) ->
            let
                _ = Debug.log "AddUserCompleted" (toString error)
            in
            model => Cmd.none

        RemoveUser userId ->
            let
                dropdownState =
                    model.userDropdownState

                removeUser =
                    Request.ProjectGroup.removeUser session.token model.projectGroupId userId |> Http.toTask
            in
            model => Task.attempt RemoveUserCompleted removeUser

        RemoveUserCompleted (Ok users) ->
            let
                projectGroup =
                    model.projectGroup
            in
            { model | confirmationDialog = Nothing, projectGroup = { projectGroup | users = users } } => Cmd.none

        RemoveUserCompleted (Err error) ->
            let
                _ = Debug.log "RemoveUserCompleted" (toString error)
            in
            model => Cmd.none

        SetProjectName name ->
            let
                dropdownState =
                    model.projectDropdownState
            in
            if String.length name >= 3 then
                let
                    searchByName =
                        Request.Project.searchByName session.token name |> Http.toTask
                in
                { model | projectDropdownState = { dropdownState | value = name } } => Task.attempt SearchProjectsCompleted searchByName
            else
                { model | projectDropdownState = { dropdownState | value = name, results = [] } } => Cmd.none

        SearchProjectsCompleted (Ok projects) ->
            let
                results =
                    List.map (\p -> (p.project_id, p.project_name)) projects

                dropdownState =
                    model.projectDropdownState
            in
            { model | projectDropdownState = { dropdownState | results = results } } => Cmd.none

        SearchProjectsCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "SearchProjectsCompleted" (toString error)
            in
            model => Cmd.none

        AddProject id name ->
            let
                dropdownState =
                    model.projectDropdownState

                addUser =
                    Request.ProjectGroup.addProject session.token model.projectGroupId id |> Http.toTask
            in
            { model | showAddProjectBusy = True, projectDropdownState = { dropdownState | value = "", results = [] } } => Task.attempt AddProjectCompleted addUser

        AddProjectCompleted (Ok group) ->
            let
                projectGroup =
                    model.projectGroup
            in
            { model | showAddProjectDialog = False, projectGroup = group } => Cmd.none

        AddProjectCompleted (Err error) ->
            let
                _ = Debug.log "AddUserCompleted" (toString error)
            in
            model => Cmd.none

        RemoveProject projectId ->
            let
                removeUser =
                    Request.ProjectGroup.removeProject session.token model.projectGroupId projectId |> Http.toTask
            in
            model => Task.attempt RemoveProjectCompleted removeUser

        RemoveProjectCompleted (Ok group) ->
            { model | confirmationDialog = Nothing, projectGroup = group } => Cmd.none

        RemoveProjectCompleted (Err error) ->
            let
                _ = Debug.log "RemoveProjectCompleted" (toString error)
            in
            model => Cmd.none

        OpenConfirmationDialog confirmationText yesMsg ->
            let
                dialog =
                    confirmationDialogConfig confirmationText CloseConfirmationDialog yesMsg
            in
            { model | confirmationDialog = Just dialog } => Cmd.none

        CloseConfirmationDialog ->
            { model | confirmationDialog = Nothing } => Cmd.none



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
            , viewProjects model.projectGroup.projects model.isEditable
            , viewUsers model.projectGroup.users model.isEditable
            ]
        , Dialog.view
            (if model.showAddUserDialog then
                Just (addUserDialogConfig model)
            else if model.showAddProjectDialog then
                Just (addProjectDialogConfig model)
            else if (model.confirmationDialog /= Nothing) then
                model.confirmationDialog
            else
                Nothing
            )
        ]


viewProjectGroup : ProjectGroup -> Html Msg
viewProjectGroup group =
    table [ class "table" ]
        [ colgroup []
            [ col [ class "col-md-1" ] [] ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text group.group_name ]
            ]
        , tr []
            [ th [ class "top" ] [ text "Description" ]
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
            a [ href url, target "_blank" ] [ text url ]

        _ ->
            text url


viewProjects : List Project -> Bool -> Html Msg
viewProjects projects isEditable =
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

        addButton =
            if isEditable then
                button [ class "btn btn-default btn-sm pull-right", onClick OpenAddProjectDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Project" ]
            else
                text ""

        body =
            case numProjects of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (List.map (viewProject isEditable) projects) ]
    in
    div []
        [ h2 []
            [ text "Projects "
            , label
            , addButton
            ]
        , body
        ]


viewProject : Bool -> Project -> Html Msg
viewProject isEditable project =
    let
        removeButton =
            if isEditable then
                button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Are you sure you want to remove this project from the group?" (RemoveProject project.project_id)) ] [ text "Remove" ]
            else
                text ""
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Project project.project_id) ]
                [ text project.project_name ]
            ]
        , td [] [ removeButton ]
        ]


viewUsers : List User -> Bool -> Html Msg
viewUsers users isEditable =
    let
        numUsers =
            List.length users

        label =
            case numUsers of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numUsers)
                        ]
        addButton =
            if isEditable then
                button [ class "btn btn-default btn-sm pull-right", onClick OpenAddUserDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add User" ]
            else
                text ""

        sortByNameAndPerm a b =
            if a.permconn.permission == "owner" then
                LT
            else if b.permconn.permission == "owner" then
                GT
            else
                compare (userDisplayName a) (userDisplayName b)

        body =
            case numUsers of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        [ tbody []
                            (tr []
                                [ th [] [ text "Name" ]
                                , th [] [ text "Username" ]
                                , th [] [ text "Permission" ]
                                , th [] []
                                ] ::
                            (List.sortWith sortByNameAndPerm users |> List.map (viewUser isEditable)))
                        ]
    in
    div []
        [ h2 []
            [ text "Users "
            , label
            , addButton
            ]
        , body
        ]


viewUser : Bool -> User -> Html Msg
viewUser isEditable user =
    let
        removeButton =
            if isEditable && user.permconn.permission /= "owner" then
                button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Are you sure you want to remove this user from the group?" (RemoveUser user.user_id)) ] [ text "Remove" ]
            else
                text ""

        permissions =
            [ "read-only", "read-write" ]

        makeOption name =
            option [ value name, selected (name == user.permconn.permission) ] [ text (capitalize name) ]

        permissionDropdown =
            if isEditable && user.permconn.permission /= "owner" then
                select [ onInput (\p -> AddUser p user.user_id user.user_name) ]
                    (List.map makeOption permissions)
            else
                text (capitalize user.permconn.permission)
    in
    tr []
        [ td [] [ userDisplayName user |> text ]
        , td [] [ text user.user_name ]
        , td [] [ permissionDropdown ]
        , td [] [ removeButton ]
        ]


userDisplayName : User -> String
userDisplayName user =
    user.first_name ++ " " ++ user.last_name


addProjectDialogConfig : Model -> Dialog.Config Msg
addProjectDialogConfig model =
    let
        content =
            if model.showAddProjectBusy then
                spinner
            else
                div [ class "form-group" ]
                    [ View.SearchableDropdown.view projectDropdownConfig model.projectDropdownState
                    ]
        footer =
            let
                disable =
                    disabled model.showAddProjectBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseAddProjectDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", disable ] [ text "OK" ]
                    ]
    in
    { closeMessage = Just CloseAddProjectDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add Project" ])
    , body = Just content
    , footer = Just footer
    }


projectDropdownConfig : View.SearchableDropdown.Config Msg Msg
projectDropdownConfig =
    { placeholder = "Enter the name of the project to add"
    , autofocus = False
    , inputMsg = SetProjectName
    , selectMsg = AddProject
    }


addUserDialogConfig : Model -> Dialog.Config Msg
addUserDialogConfig model =
    let
        content =
            if model.showAddUserBusy then
                spinner
            else
                div [ class "form-group" ]
                    [ View.SearchableDropdown.view userDropdownConfig model.userDropdownState
                    ]

        footer =
            let
                disable =
                    disabled model.showAddUserBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseAddUserDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", disable ] [ text "OK" ]
                    ]
    in
    { closeMessage = Just CloseAddUserDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add User" ])
    , body = Just content
    , footer = Just footer
    }


userDropdownConfig : View.SearchableDropdown.Config Msg Msg
userDropdownConfig =
    { placeholder = "Enter the name of the person to add"
    , autofocus = False
    , inputMsg = SetUserName
    , selectMsg = (AddUser "read-only")
    }
