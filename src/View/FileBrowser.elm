module View.FileBrowser exposing (..)

{-| A heavy-weight file browser component

"Heavy-weight" means that it has its own internal state and update routine, a practice which is discouraged in Elm.
However there is so much functionality packed into this module that it seems justified in this case.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Table exposing (defaultCustomizations)
import Dialog
import Task exposing (Task)
import Http
import Route
import Filesize
import View.Spinner exposing (spinner)
import View.Dialog exposing (confirmationDialogConfig)
import Data.Agave exposing (FileResult)
import Data.Session exposing (Session)
import Request.Agave
import Ports
import List.Extra
import Util exposing ((=>))



-- MODEL


type Model
    = Model InternalModel


type alias InternalModel =
    { path : String
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
    , errorMessage : Maybe String
    , confirmationDialog : Maybe (Dialog.Config Msg)
    }


type alias Config =
    { showNewFolderButton : Bool
    , showUploadFileButton : Bool
    , allowDirSelection : Bool
    , allowMultiSelection : Bool
    }


defaultConfig : Config
defaultConfig =
    { showNewFolderButton = True
    , showUploadFileButton = True
    , allowDirSelection = True
    , allowMultiSelection = False
    }


init : Session -> Maybe Config -> Model --Task Http.Error Model
init session maybeConfig =
    let
        user_name =
            session.user |> Maybe.map .user_name |> Maybe.withDefault ""

        startingPath =
            "/" ++ user_name

        config =
            maybeConfig |> Maybe.withDefault defaultConfig
    in
    Model
        { path = startingPath
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
        , errorMessage = Nothing
        , confirmationDialog = Nothing
        }


loadPath : String -> String -> Task Http.Error (List FileResult)
loadPath token path =
    Request.Agave.getFileList token path |> Http.toTask |> Task.map .result



-- UPDATE


type Msg
    = SetFilter String
    | SetPath String
    | SelectPath String
    | RefreshPath
    | LoadPath String
    | LoadPathCompleted (Result Http.Error (List FileResult))
    | OpenNewFolderDialog
    | CloseNewFolderDialog
    | SetNewFolderName String
    | CreateNewFolderCompleted (Result Http.Error (Request.Agave.EmptyResponse))
    | CreateNewFolder
    | DeletePath String
    | DeletePathCompleted (Result Http.Error (Request.Agave.EmptyResponse))
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
view (Model {path, pathFilter, contents, tableState, selectedPaths, isBusy, errorMessage, confirmationDialog, showNewFolderDialog, showNewFolderBusy, config}) =
    let
        filterButton label =
            let
                isActive =
                    label == pathFilter
            in
            button [ class "btn btn-default", classList [("active", isActive)], type_ "button", onClick (SetFilter label) ]
                [ text label ]
    in
    div []
        [ Html.form [ class "form-inline" ]
            [ div [ class "form-group" ]
                [ div [ class "input-group" ]
                    [ div [ class "input-group-btn" ]
                        [ filterButton "Home"
                        , filterButton "Shared"
                        ]
                    , input [ class "form-control", type_ "text", size 45, value path, onInput SetPath ] []
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
--                    div [ class "btn-group" ]
--                        [ button [ class "btn btn-default btn-sm dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ]
--                            [ span [ class "glyphicon glyphicon-cloud-upload" ] []
--                            , text " Upload File "
--                            , span [ class "caret" ] []
--                            ]
--                        , ul [ class "dropdown-menu" ]
--                            [ li [] [ a [ onClick UploadFile ] [ text "From local" ] ]
--                            , li [] [ a [] [ text "From URL (FTP/HTTP)" ] ]
--                            , li [] [ a [] [ text "From NCBI" ] ]
--                            , li [] [ a [] [ text "From EBI" ] ]
--                            ]
--                        ]
                        button [ class "btn btn-default btn-sm", type_ "button", onClick UploadFile ]
                            [ span [ class "glyphicon glyphicon-cloud-upload" ] []
                            , text " Upload File"
                            ]
                    else
                      text ""
                ]
            ]
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
                (\data -> if data.type_ == "dir" then "..." ++ data.name else data.name) -- sort dirs before files
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
            case isBusy of
                False ->
                    input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new folder", onInput SetNewFolderName ] []

                True ->
                    spinner

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