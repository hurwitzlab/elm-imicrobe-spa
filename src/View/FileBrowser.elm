module View.FileBrowser exposing (..)

{-| A heavy-weight file browser component

"Heavy-weight" means that it has its own internal state and update routine, a practice which is discouraged in Elm.
However there is so much functionality packed into this module that it seems justified in this case.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Events exposing (onKeyDown)
import Table exposing (defaultCustomizations)
import Dialog
import Task exposing (Task)
import Time exposing (Time)
import Http
import Route
import Filesize
import View.Spinner exposing (spinner)
import View.Dialog exposing (confirmationDialogConfig)
import View.SearchableDropdown
import Data.Agave exposing (FileResult, PermissionResult, Permission)
import Data.Session exposing (Session)
import Request.Agave
import Ports
import List.Extra
import Util exposing ((=>))



-- MODEL


type Model
    = Model InternalModel


type alias InternalModel =
    { currentUserName : String
    , path : String
    , rootPath : String
    , homePath : String
    , sharedPath : String
    , selectedPaths : Maybe (List String)
    , pathFilter : String
    , contents : List FileResult
    , tableState : Table.State
    , isBusy : Bool
    , config : Config
    , showNewFolderDialog : Bool
    , showNewFolderBusy : Bool
    , newFolderName : String
    , showViewFileDialog : Bool
    , showViewFileBusy : Bool
    , filePath : Maybe String
    , fileContent : Maybe String
    , fileErrorMessage : Maybe String
    , showShareDialog : Bool
    , showShareBusy : Bool
    , doUserSearch : Bool
    , searchStartTime : Time
    , filePermissions : Maybe (List PermissionResult)
    , shareDropdownState : View.SearchableDropdown.State
    , errorMessage : Maybe String
    , confirmationDialog : Maybe (Dialog.Config Msg)
    }


type alias Config =
    { showMenuBar : Bool
    , showNewFolderButton : Bool
    , showUploadFileButton : Bool
    , allowDirSelection : Bool
    , allowMultiSelection : Bool
    , allowFileViewing : Bool
    , homePath : Maybe String
    }


defaultConfig : Config
defaultConfig =
    { showMenuBar = True
    , showNewFolderButton = True
    , showUploadFileButton = True
    , allowDirSelection = True
    , allowMultiSelection = False
    , allowFileViewing = True
    , homePath = Nothing
    }


init : Session -> Maybe Config -> Model --Task Http.Error Model
init session maybeConfig =
    let
        username =
            session.user |> Maybe.map .user_name |> Maybe.withDefault ""

        config =
            maybeConfig |> Maybe.withDefault defaultConfig

        startingPath =
            config.homePath |> Maybe.withDefault ("/" ++ username)
    in
    Model
        { currentUserName = username
        , path = startingPath
        , rootPath = startingPath
        , homePath = startingPath
        , sharedPath = "/shared"
        , selectedPaths = Nothing
        , pathFilter = "Home"
        , contents = []
        , tableState = Table.initialSort "Name"
        , isBusy = True
        , config = config
        , showNewFolderDialog = False
        , showNewFolderBusy = False
        , newFolderName = ""
        , showViewFileDialog = False
        , showViewFileBusy = False
        , filePath = Nothing
        , fileContent = Nothing
        , fileErrorMessage = Nothing
        , showShareDialog = False
        , showShareBusy = False
        , doUserSearch = False
        , searchStartTime = 0
        , filePermissions = Nothing
        , shareDropdownState = View.SearchableDropdown.init
        , errorMessage = Nothing
        , confirmationDialog = Nothing
        }


loadPath : String -> String -> Task Http.Error (List FileResult)
loadPath token path =
    Request.Agave.getFileList token path |> Http.toTask |> Task.map .result


maxViewFileSz = 5000



-- UPDATE


type Msg
    = SetFilter String
    | SetPath String
    | KeyDown Int
    | SelectPath String
    | RefreshPath
    | LoadPath String
    | LoadPathCompleted (Result Http.Error (List FileResult))
    | OpenPath String Int
    | OpenPathCompleted (Result Http.Error String)
    | CloseViewFileDialog
    | OpenNewFolderDialog
    | CloseNewFolderDialog
    | SetNewFolderName String
    | CreateNewFolderCompleted (Result Http.Error (Request.Agave.EmptyResponse))
    | CreateNewFolder
    | DeletePath String
    | DeletePathCompleted (Result Http.Error (Request.Agave.EmptyResponse))
    | OpenShareDialog String
    | CloseShareDialog
    | GetPermissionCompleted (Result Http.Error (List PermissionResult))
    | SetShareUserName String
    | SetSearchStartTime Time
    | SearchUsers Time
    | SearchUsersCompleted (Result Http.Error (List Data.Agave.Profile))
    | ShareWithUser String String String
    | ShareWithUserCompleted (Result Http.Error (Request.Agave.EmptyResponse))
    | OpenConfirmationDialog String Msg
    | CloseConfirmationDialog
    | UploadFile
    | SetTableState Table.State


update : Session -> Msg -> Model -> (Model, Cmd Msg)
update session msg (Model internalModel) =
    updateInternal session msg internalModel
        |> Tuple.mapFirst Model


updateInternal : Session -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal session msg model =
    case msg of
        SetFilter value ->
            let
                newPath =
                    case value of
                        "Shared" ->
                            model.sharedPath

                        _ -> -- Home
                            model.homePath
            in
            updateInternal session (LoadPath newPath) { model | path = newPath, pathFilter = value }

        SetPath path ->
            { model | path = path } => Cmd.none

        KeyDown key ->
            if key == 13 then -- enter key
                updateInternal session (LoadPath model.path) model
            else
                model => Cmd.none

        SelectPath path ->
            let
                newPaths =
                    case model.selectedPaths of
                        Nothing ->
                            Just (List.singleton path)

                        Just paths ->
                            if List.member path paths then
                                Just (List.filter (\p -> p /= path) paths) -- unselect
                            else
                                if model.config.allowMultiSelection then
                                    Just (path :: paths)
                                else
                                    Just (List.singleton path)
            in
            { model | selectedPaths = newPaths } => Cmd.none

        RefreshPath ->
            updateInternal session (LoadPath model.path) model

        LoadPath path ->
            { model | path = path, selectedPaths = Nothing, errorMessage = Nothing, isBusy = True } => Task.attempt LoadPathCompleted (loadPath session.token path)

        LoadPathCompleted (Ok files) ->
            let
                -- Manufacture a previous path
                previous =
                    { name = ".. (previous)"
                    , path = determinePreviousPath model.path
                    , type_ = "dir"
                    , format = ""
                    , length = 0
                    , lastModified = ""
                    , mimeType = ""
                    }

                -- Remove current path
                filtered =
                    List.filter (\f -> f.name /= ".") files

                newFiles =
                    -- Only show previous path if not at top-level
                    if model.path /= model.rootPath && model.path /= model.sharedPath then
                        previous :: filtered
                    else
                        filtered
            in
            { model | contents = newFiles, isBusy = False } => Cmd.none

        LoadPathCompleted (Err error) ->
            let
                (msg, cmd) =
                    case error of
                        Http.NetworkError ->
                            ("Cannot connect to remote host", Cmd.none)

                        Http.BadStatus response ->
                            case response.status.code of
                                401 -> ("Unauthorized", Route.modifyUrl Route.Login) -- redirect to Login page

                                403 -> ("Permission denied", Cmd.none)

                                _ ->
                                    case String.length response.body of
                                        0 ->
                                            ("Bad status", Cmd.none)

                                        _ ->
                                            (response.body, Cmd.none)

                        _ ->
                            (toString error, Cmd.none)
            in
            { model | errorMessage = (Just msg), isBusy = False } => cmd

        OpenPath path length ->
            let
                chunkSz =
                    Basics.min (length-1) (maxViewFileSz-1)

                openPath token path =
                    Request.Agave.getFileRange token path (Just (0, chunkSz)) |> Http.toTask
            in
            { model | showViewFileDialog = True, showViewFileBusy = True, filePath = Just path } => Task.attempt OpenPathCompleted (openPath session.token path)

        OpenPathCompleted (Ok data) ->
            { model | fileContent = Just data, showViewFileBusy = False } => Cmd.none

        OpenPathCompleted (Err error) ->
            { model | fileErrorMessage = (Just (toString error)) } => Cmd.none

        CloseViewFileDialog ->
            { model | showViewFileDialog = False } => Cmd.none

        OpenNewFolderDialog ->
            { model | showNewFolderDialog = True, showNewFolderBusy = False } => Cmd.none

        CloseNewFolderDialog ->
            { model | showNewFolderDialog = False } => Cmd.none

        SetNewFolderName name ->
            { model | newFolderName = name } => Cmd.none

        CreateNewFolder ->
            let
                createFolder =
                    Request.Agave.mkdir session.token model.path model.newFolderName |> Http.toTask
            in
            { model | showNewFolderBusy = True } => Task.attempt CreateNewFolderCompleted createFolder

        CreateNewFolderCompleted (Ok _) ->
            updateInternal session RefreshPath { model | showNewFolderDialog = False }

        CreateNewFolderCompleted (Err error) ->
            { model | showNewFolderDialog = False, errorMessage = Just (toString error) } => Cmd.none

        DeletePath path ->
            let
                delete =
                    Request.Agave.delete session.token path |> Http.toTask
            in
            { model | isBusy = True, confirmationDialog = Nothing } => Task.attempt DeletePathCompleted delete

        DeletePathCompleted (Ok _) ->
            updateInternal session RefreshPath model

        DeletePathCompleted (Err error) ->
            { model | isBusy = False } => Cmd.none

        OpenShareDialog path ->
            let
                getPermission =
                    Request.Agave.getFilePermission session.token path |> Http.toTask |> Task.map .result
            in
            { model | showShareDialog = True, showShareBusy = True } => Task.attempt GetPermissionCompleted getPermission

        CloseShareDialog ->
            { model | showShareDialog = False } => Cmd.none

        GetPermissionCompleted (Ok permissions) ->
            let
                notAllowed = --FIXME move to config file
                    [ "dooley", "vaughn", "rodsadmin", "jstubbs", "jfonner", "eriksf", "QuickShare"
                    , "admin2", "admin_proxy", "agave", "bisque-adm", "de-irods", "has_admin", "ibp-proxy"
                    , "ipc_admin", "ipcservices", "proxy-de-tools", "uk_admin", "uportal_admin2"
                    ]

                filtered =
                    List.filter (\p -> List.member p.username notAllowed |> not) permissions
            in
            { model | showShareBusy = False, filePermissions = Just filtered } => Cmd.none

        GetPermissionCompleted (Err error) -> -- TODO
            { model | filePermissions = Nothing } => Cmd.none

        SetShareUserName name ->
            let
                dropdownState =
                    model.shareDropdownState
            in
            if String.length name >= 3 then
                { model | shareDropdownState = { dropdownState | value = name }, doUserSearch = True } => Task.perform SetSearchStartTime Time.now
            else
                { model | shareDropdownState = { dropdownState | value = name, results = [] }, doUserSearch = False } => Cmd.none

        SetSearchStartTime time ->
            { model | searchStartTime = time } => Cmd.none

        SearchUsers time ->
            if model.doUserSearch && time - model.searchStartTime >= 500 * Time.millisecond then
                let
                    searchProfiles =
                        Request.Agave.searchProfiles session.token model.shareDropdownState.value |> Http.toTask |> Task.map .result
                in
                { model | doUserSearch = False } => Task.attempt SearchUsersCompleted searchProfiles
            else
                model => Cmd.none

        SearchUsersCompleted (Ok users) ->
            let
                userDisplayName user =
                    user.first_name ++ " " ++ user.last_name ++ " (" ++ user.username ++ ")"

                results =
                    List.map (\u -> (u.username, userDisplayName u)) users

                dropdownState =
                    model.shareDropdownState
            in
            { model | shareDropdownState = { dropdownState | results = results } } => Cmd.none

        SearchUsersCompleted (Err error) -> -- TODO
            let
                _ = Debug.log "SearchUsersCompleted" (toString error)
            in
            model => Cmd.none

        ShareWithUser permission username _ ->
            let
                dropdownState =
                    model.shareDropdownState

                newModel =
                    { model | showShareBusy = True, shareDropdownState = { dropdownState | value = "", results = [] } }

                firstSelected =
                    model.selectedPaths |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""
            in
            let
                noChange =
                    case model.filePermissions of
                        Nothing ->
                            False

                        Just permissions ->
                            List.any (\p -> p.username == username && (permissionDesc p.permission) == permission) permissions

                agavePerm =
                    case permission of
                         "read/write" ->
                             "READ_WRITE"

                         "none" ->
                              "NONE"

                         _ ->
                             "READ"

                shareFile =
                    Request.Agave.setFilePermission session.token username agavePerm firstSelected |> Http.toTask
            in
            if noChange then
                model => Cmd.none
            else
                newModel => Task.attempt ShareWithUserCompleted shareFile

        ShareWithUserCompleted (Ok _) ->
            let
                firstSelected =
                    model.selectedPaths |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""
            in
            updateInternal session (OpenShareDialog firstSelected) model

        ShareWithUserCompleted (Err error) -> -- TODO
            let
                _ = Debug.log "ShareWithUserCompleted" (toString error)
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

        UploadFile ->
            model => Ports.fileUploadOpenBrowser (session.token, model.path)

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none


determinePreviousPath : String -> String
determinePreviousPath path =
    let
        l =
            String.split "/" path
        n =
            List.length l
    in
    List.take (n-1) l |> String.join "/"



-- VIEW


view : Model -> Html Msg
view (Model {currentUserName, path, pathFilter, contents, tableState, selectedPaths, isBusy, errorMessage, confirmationDialog,
            showNewFolderDialog, showNewFolderBusy,
            showViewFileDialog, showViewFileBusy, filePath, fileContent, fileErrorMessage,
            showShareDialog, showShareBusy, filePermissions, shareDropdownState,
            config
            }) =
    let
        menuBar =
            div [ class "form-inline" ]
                [ div [ class "form-group" ]
                    [ div [ class "input-group" ]
                        [ div [ class "input-group-btn" ]
                            [ filterButton "Home"
                            , filterButton "Shared"
                            ]
                        , samp [] [ input [ class "form-control",  type_ "text", size 45, value path, onInput SetPath, onKeyDown KeyDown ] [] ]
                        , span [ class "input-group-btn" ]
                            [ button [ class "btn btn-default", type_ "button", onClick (LoadPath path) ] [ text "Go " ]
                            ]
                        ]
                    , button [ style [("visibility","hidden")] ] -- FIXME make a better spacer than this
                        [ text " " ]
                    , if (config.showNewFolderButton) then
                        button [ class "btn btn-default btn-sm margin-right", type_ "button", onClick OpenNewFolderDialog ]
                            [ span [ class "glyphicon glyphicon-folder-close" ] [], text " New Folder" ]
                      else
                        text ""
                    , if (config.showUploadFileButton) then
--                        div [ class "btn-group" ]
--                            [ button [ class "btn btn-default btn-sm dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ]
--                                [ span [ class "glyphicon glyphicon-cloud-upload" ] []
--                                , text " Upload File "
--                                , span [ class "caret" ] []
--                                ]
--                            , ul [ class "dropdown-menu" ]
--                                [ li [] [ a [ onClick UploadFile ] [ text "From local" ] ]
--                                , li [] [ a [] [ text "From URL (FTP/HTTP)" ] ]
--                                , li [] [ a [] [ text "From NCBI" ] ]
--                                , li [] [ a [] [ text "From EBI" ] ]
--                                ]
--                            ]
                            button [ class "btn btn-default btn-sm", type_ "button", onClick UploadFile ]
                                [ span [ class "glyphicon glyphicon-cloud-upload" ] []
                                , text " Upload File"
                                ]
                        else
                          text ""
                    ]
                ]

        filterButton label =
            let
                isActive =
                    label == pathFilter
            in
            button [ class "btn btn-default", classList [("active", isActive)], type_ "button", onClick (SetFilter label) ]
                [ text label ]

        firstSelected =
            selectedPaths |> Maybe.withDefault [] |> List.head |> Maybe.withDefault ""
    in
    div []
        [ if config.showMenuBar then
            menuBar
          else
            text ""
        , br [] []
        , if errorMessage /= Nothing then
            div [ class "alert alert-danger" ] [ text (Maybe.withDefault "An error occurred" errorMessage) ]
          else if isBusy then
            spinner
          else
            div [ style [("overflow-y","auto"),("height","100%")] ] --("height","60vh")] ]
                [ Table.view (tableConfig config selectedPaths) tableState contents ]
        , Dialog.view
            (if (confirmationDialog /= Nothing) then
                confirmationDialog
             else if showNewFolderDialog then
                Just (newFolderDialogConfig showNewFolderBusy)
             else if showViewFileDialog && filePath /= Nothing then
                Just (viewFileDialogConfig (filePath |> Maybe.withDefault "") (fileContent |> Maybe.withDefault "") showViewFileBusy fileErrorMessage)
             else if showShareDialog then
                Just (shareDialogConfig firstSelected (filePermissions |> Maybe.withDefault []) currentUserName shareDropdownState showShareBusy fileErrorMessage)
             else
                Nothing
            )
        , input [ type_ "file", id "fileToUpload", name "fileToUpload", style [("display","none")] ] [] -- hidden input for file upload plugin, "fileToUpload" name is required by Agave
        ]


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-hover"
    ]


toRowAttrs : Config -> Maybe (List String) -> FileResult -> List (Attribute Msg)
toRowAttrs config selectedPaths data =
    onClick (SelectPath data.path)
    :: (case selectedPaths of
            Nothing ->
                []

            Just selectedPaths ->
                if (List.member data.path selectedPaths && (data.type_ == "file" || config.allowDirSelection)) then
                    [ attribute "class" "active" ]
                else
                    []
       )
    |> List.append
        (if data.type_ == "dir" then
            [ onDoubleClick (LoadPath data.path) ]
        else if data.type_ == "file" && config.allowFileViewing then
            [ onDoubleClick (OpenPath data.path data.length) ]
        else
            []
        )

tableConfig : Config -> Maybe (List String) -> Table.Config FileResult Msg
tableConfig config selectedRowIds =
    Table.customConfig
        { toId = .path
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , sizeColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toRowAttrs config selectedRowIds }
        }


nameColumn : Table.Column FileResult Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter =
            Table.increasingOrDecreasingBy
                (\data ->
                    if data.type_ == "dir" then -- sort dirs before files
                        "..." ++ data.name
                    else data.name
                )
        }


nameLink : FileResult -> Table.HtmlDetails Msg
nameLink file =
    if file.type_ == "dir" then
        Table.HtmlDetails []
            [ a [ onClick (LoadPath file.path) ] [ text file.name ]
            ]
    else
        Table.HtmlDetails [] [ text file.name ]


sizeColumn : Table.Column FileResult Msg
sizeColumn =
    Table.veryCustomColumn
        { name = "Size"
        , viewData = (\file -> Table.HtmlDetails [] [ if (file.length > 0) then (text (Filesize.format file.length)) else text "" ])
        , sorter = Table.increasingOrDecreasingBy .length
        }


newFolderDialogConfig : Bool -> Dialog.Config Msg
newFolderDialogConfig isBusy =
    let
        content =
            if isBusy then
                spinner
            else
                input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new folder", onInput SetNewFolderName ] []

        footer =
            let
                disable =
                    disabled isBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseNewFolderDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick CreateNewFolder, disable ] [ text "OK" ]
                    ]
    in
    { closeMessage = Just CloseNewFolderDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Create New Folder" ])
    , body = Just content
    , footer = Just footer
    }


viewFileDialogConfig : String -> String -> Bool -> Maybe String -> Dialog.Config Msg
viewFileDialogConfig path data isBusy errorMsg =
    let
        content =
            if errorMsg /= Nothing then
                div [ class "alert alert-danger" ] [ Maybe.withDefault "" errorMsg |> text ]
            else if isBusy then
                spinner
            else
                div [ style [("max-height","60vh"), ("overflow-y", "auto")] ]
                    [ text path
                    , pre [] [ text data ]
                    ]

        header =
            h3 [] [ text "View File" ]

        footer =
            div []
                [ if errorMsg == Nothing && not isBusy then
                    em [ class "pull-left" ] [ "Showing first " ++ (toString maxViewFileSz) ++ " bytes only" |> text ]
                  else
                    text ""
                , button [ class "btn btn-primary", onClick CloseViewFileDialog ] [ text "Close" ]
                ]
    in
    { closeMessage = Nothing
    , containerClass = Nothing
    , header = Just header
    , body = Just content
    , footer = Just footer
    }


shareDialogConfig : String -> List PermissionResult -> String -> View.SearchableDropdown.State -> Bool -> Maybe String -> Dialog.Config Msg
shareDialogConfig path permissions currentUserName dropdownState isBusy errorMsg =
    let
        content =
            if errorMsg /= Nothing then
                div [ class "alert alert-danger" ] [ Maybe.withDefault "" errorMsg |> text ]
            else if isBusy then
                spinner
            else
                div []
                    [ text "Who has access"
                    , div [ class "scrollable", style [ ("max-height","30vh") ] ]
                        [ viewPermissions currentUserName permissions ]
                    , addPanel
                    ]

        addPanel =
            div []
                [ br [] []
                , hr [] []
                , br [] []
                , div [ class "form-group" ]
                    [ div [] [ text "Add a person:" ]
                    , div []
                        [ View.SearchableDropdown.view shareDropdownConfig dropdownState ]
                    ]
                , br [] []
                ]
    in
    { closeMessage = Just CloseShareDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Share Item" ])
    , body = Just content
    , footer = Nothing
    }


shareDropdownConfig : View.SearchableDropdown.Config Msg Msg
shareDropdownConfig =
    { placeholder = "Enter the name of the person to add "
    , autofocus = False
    , inputMsg = SetShareUserName
    , selectMsg = ShareWithUser "read-only"
    }


viewPermissions : String -> List PermissionResult -> Html Msg
viewPermissions currentUserName permissions =
    let
        isEditable =
            permissions
                |> List.any (\pr -> pr.username == currentUserName && pr.permission.write)
    in
    if permissions == [] then
        div [] [ text "Only you can see this item." ]
    else
        let
            sortByNameAndPerm a b =
                if a.username == currentUserName then
                    LT
                else if b.username == currentUserName then
                    GT
                else
                    compare a.username b.username
        in
        table [ class "table" ]
            [ tbody []
                (permissions
                    |> List.sortWith sortByNameAndPerm
                    |> List.map (\pr -> viewPermission (pr.username == currentUserName) isEditable pr.username pr.permission)
                )
            ]


viewPermission : Bool -> Bool -> String -> Permission -> Html Msg
viewPermission isMe isEditable username permission =
    tr []
        [ td []
            [ i [ class "fas fa-user" ] []
            , text " "
            , text username
            , if isMe then
                text " (you)"
              else
                text ""
            , if isEditable && not isMe then
                viewPermissionDropdown username permission
              else
                span [ class "pull-right" ] [ permissionDesc permission |> text ]
            ]
        ]


viewPermissionDropdown : String -> Permission -> Html Msg
viewPermissionDropdown username permission =
    div [ class "pull-right dropdown" ]
        [ button [ class "btn btn-default btn-xs dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ]
            [ permissionDesc permission |> text
            , text " "
            , span [ class "caret" ] []
            ]
        , ul [ class "dropdown-menu nowrap" ]
            [ li [] [ a [ onClick (ShareWithUser "read-only" username "") ] [ text "Read-only: can view but not modify" ] ]
            , li [] [ a [ onClick (ShareWithUser "read/write" username "") ] [ text "Read-write: can view, edit, and delete" ] ]
            , li [] [ a [ onClick (ShareWithUser "none" username "") ] [ text "Remove access" ] ]
            ]
        ]


permissionDesc : Permission -> String
permissionDesc permission =
    if permission.read then
        if permission.write then
            "read/write"
        else
            "read-only"
    else
        "none"



---- HELPER FUNCTIONS ----


numItems : Model -> Int
numItems (Model {contents}) =
    case List.Extra.uncons contents of
        Nothing ->
            0

        Just (first, rest) ->
            if first.name == ".. (previous)" then
                List.length rest
            else
                List.length contents


getSelected : Model -> List FileResult
getSelected (Model { selectedPaths, contents }) =
    case selectedPaths of
        Nothing ->
            []

        Just paths ->
            List.filter (\f -> List.member f.path paths) contents