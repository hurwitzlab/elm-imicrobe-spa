module Page.Dashboard exposing (Model, Msg(..), init, update, view)

import Data.Session exposing (Session)
import Data.Project exposing (Project)
import Data.User exposing (User, Sample, LogEntry)
import Data.Agave exposing (FileResult)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Dialog
import Table exposing (defaultCustomizations)
import Http
import Route
import Request.Project
import Request.Sample
import Request.User
import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import View.FileBrowser as FileBrowser
import View.Spinner exposing (spinner)
import View.Project
import View.Sample
import View.Dialog exposing (confirmationDialogConfig)
import Ports



---- MODEL ----


type alias Model =
    { user : User
    , newProjectName : String
    , showNewProjectDialog : Bool
    , showNewProjectBusy : Bool
    , selectedContentType : ContentType
    , projectTableState : Table.State
    , sampleTableState : Table.State
    , dsTableState : Table.State
    , activityTableState : Table.State
    , selectedProjectRowId : Int
    , selectedSampleRowId : Int
    , selectedActivityRowId : String
    , fileBrowser : FileBrowser.Model
    , fileBrowserInitialized : Bool
    , showFileUploadDialog : Bool
    , fileUploadError : Maybe String
    , confirmationDialog : Maybe (Dialog.Config Msg)
    }


type ContentType
    = Project
    | Sample
    | Storage
    | Activity


init : Session -> Task PageLoadError Model
init session =
    let
        user_id =
            case session.user of
                Nothing -> 0

                Just user -> user.user_id
    in
    loadUser session.token user_id
        |> Task.andThen
            (\user ->
                Task.succeed
                    { user = user
                    , newProjectName = ""
                    , showNewProjectDialog = False
                    , showNewProjectBusy = False
                    , selectedContentType = Project
                    , projectTableState = Table.initialSort "Name"
                    , sampleTableState = Table.initialSort "Name"
                    , dsTableState = Table.initialSort "Name"
                    , activityTableState = Table.initialSort "Date"
                    , selectedProjectRowId = 0
                    , selectedSampleRowId = 0
                    , selectedActivityRowId = ""
                    , fileBrowser = FileBrowser.init session Nothing
                    , fileBrowserInitialized = False
                    , showFileUploadDialog = False
                    , fileUploadError = Nothing
                    , confirmationDialog = Nothing
                    }
            )
            |> Task.mapError Error.handleLoadError


loadUser : String -> Int -> Task Http.Error User
loadUser token id =
    Request.User.get token id |> Http.toTask



-- UPDATE --


type Msg
    = OpenNewProjectDialog
    | CloseNewProjectDialog
    | SetNewProjectName String
    | CreateNewProject
    | CreateNewProjectCompleted (Result Http.Error Project)
    | SetProjectTableState Table.State
    | SetSampleTableState Table.State
    | SetActivityTableState Table.State
    | SelectContent ContentType
    | RefreshContent
    | RefreshContentCompleted (Result Http.Error User)
    | SelectProjectRow Int
    | SelectSampleRow Int
    | SelectActivityRow String
    | RemoveProject Int
    | RemoveProjectCompleted (Result Http.Error String)
    | RemoveSample Int
    | RemoveSampleCompleted (Result Http.Error String)
    | OpenConfirmationDialog String Msg
    | CloseConfirmationDialog
    | FileBrowserMsg FileBrowser.Msg
    | UploadFileBegin (Maybe Ports.FileToUpload)
    | UploadFileEnd
    | UploadFileError
    | CloseFileUploadDialog


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        OpenNewProjectDialog ->
            { model | showNewProjectDialog = True } => Cmd.none

        CloseNewProjectDialog ->
            { model | showNewProjectDialog = False } => Cmd.none

        SetNewProjectName name ->
            { model | newProjectName = name } => Cmd.none

        CreateNewProject ->
            let
                createProject =
                    Request.Project.create session.token model.newProjectName |> Http.toTask
            in
            { model | showNewProjectBusy = True } => Task.attempt CreateNewProjectCompleted createProject

        CreateNewProjectCompleted (Ok project) ->
            model => Route.modifyUrl (Route.Project project.project_id)

        CreateNewProjectCompleted (Err error) ->
            model => Cmd.none

        SelectContent contentType ->
            case contentType of
                Storage ->
                    if not model.fileBrowserInitialized then
                        let
                            (subModel, subCmd) =
                                FileBrowser.update session FileBrowser.RefreshPath model.fileBrowser
                        in
                        { model | selectedContentType = contentType, fileBrowserInitialized = True, fileBrowser = subModel } => Cmd.map FileBrowserMsg subCmd
                    else
                        { model | selectedContentType = contentType } => Cmd.none

                _ ->
                    { model | selectedContentType = contentType } => Cmd.none

        RefreshContent ->
            case model.selectedContentType of
                Storage ->
                    let
                        (subModel, subCmd) =
                            FileBrowser.update session FileBrowser.RefreshPath model.fileBrowser
                    in
                    { model | fileBrowser = subModel } => Cmd.map FileBrowserMsg subCmd

                _ ->
                    model => Task.attempt RefreshContentCompleted (loadUser session.token model.user.user_id)

        RefreshContentCompleted (Ok user) ->
            { model | user = user } => Cmd.none

        RefreshContentCompleted (Err error) -> -- TODO finish me
            model => Cmd.none

        SetProjectTableState newState ->
            { model | projectTableState = newState } => Cmd.none

        SetSampleTableState newState ->
            { model | sampleTableState = newState } => Cmd.none

        SetActivityTableState newState ->
            { model | activityTableState = newState } => Cmd.none

        SelectProjectRow id ->
            let
                selectedRowId =
                    if model.selectedProjectRowId == id then
                        0 -- unselect
                    else
                        id
            in
            { model | selectedProjectRowId = selectedRowId } => Cmd.none

        SelectSampleRow id ->
            let
                selectedRowId =
                    if model.selectedSampleRowId == id then
                        0 -- unselect
                    else
                        id
            in
            { model | selectedSampleRowId = selectedRowId } => Cmd.none

        SelectActivityRow id ->
            let
                selectedRowId =
                    if model.selectedActivityRowId == id then
                        "" -- unselect
                    else
                        id
            in
            { model | selectedActivityRowId = selectedRowId } => Cmd.none

        RemoveProject id ->
            let
                removeProject =
                    Request.Project.remove session.token id |> Http.toTask
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemoveProjectCompleted removeProject

        RemoveProjectCompleted (Ok _) ->
            update session RefreshContent model

        RemoveProjectCompleted (Err error) -> -- TODO finish me
            model => Cmd.none

        RemoveSample id ->
            let
                removeSample =
                    Request.Sample.remove session.token id |> Http.toTask
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemoveSampleCompleted removeSample

        RemoveSampleCompleted (Ok _) ->
            update session RefreshContent model

        RemoveSampleCompleted (Err error) -> -- TODO finish me
            model => Cmd.none

        OpenConfirmationDialog confirmationText yesMsg ->
            let
                dialog =
                    confirmationDialogConfig confirmationText CloseConfirmationDialog yesMsg
            in
            { model | confirmationDialog = Just dialog } => Cmd.none

        CloseConfirmationDialog ->
            { model | confirmationDialog = Nothing } => Cmd.none

        FileBrowserMsg subMsg ->
            let
                ( newFileBrowser, subCmd ) =
                    FileBrowser.update session subMsg model.fileBrowser
            in
            { model | fileBrowser = newFileBrowser } => Cmd.map FileBrowserMsg subCmd

        UploadFileBegin file ->
            { model | showFileUploadDialog = True, fileUploadError = Nothing } => Cmd.none

        UploadFileEnd ->
            let
                (subModel, subCmd) =
                    FileBrowser.update session FileBrowser.RefreshPath model.fileBrowser
            in
            { model | showFileUploadDialog = False, fileBrowser = subModel } => Cmd.map FileBrowserMsg subCmd

        UploadFileError ->
            { model | fileUploadError = Just "An error occurred.  Please ensure that the file doesn't already exist and you have permission to write to the destination path." } => Cmd.none

        CloseFileUploadDialog ->
            { model | showFileUploadDialog = False } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-sm-2" ] [ viewMenu model ]
        , div [ class "col-sm-10" ] (viewContent model)
        , Dialog.view
            (if model.showNewProjectDialog then
                Just (newProjectDialogConfig model)
             else if model.showFileUploadDialog then
                Just (fileUploadDialogConfig model)
             else if model.confirmationDialog /= Nothing then
                model.confirmationDialog
             else
                Nothing
            )
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    let
        mkButton title type_ =
            button [ class "menu-button", classList [("active", model.selectedContentType == type_)], onClick (SelectContent type_) ] [ text title ]
    in
    div [ class "menu-panel" ]
        [ div [] [ mkButton "Projects" Project ]
        , div [] [ mkButton "Samples" Sample ]
        , div [] [ mkButton "Data Store" Storage ]
        , div [] [ mkButton "Activity" Activity ]
        ]


--viewNewButton : Html Msg
--viewNewButton =
--    div [ class "btn-group" ]
--        [ div [ class "menu-button-new" ]
--            [ button [ class "btn btn-default dropdown-toggle", attribute "aria-expanded" "false", attribute "aria-haspopup" "true", attribute "data-toggle" "dropdown", type_ "button" ]
--                [ text "NEW "
--                , span [ class "caret" ]
--                    []
--                ]
--            ]
--        , ul [ class "dropdown-menu" ]
--            [ li []
--                [ a [ onClick OpenNewProjectDialog ]
--                    [ text "Project" ]
--                ]
----            , li []
----                [ a [ href "#" ]
----                    [ text "Sample" ]
----                ]
--            ]
--        ]


viewContent : Model -> List (Html Msg)
viewContent model =
    let
        samples =
            samplesFromProjects model.user.projects

        (title, viewTable, count, buttonPanel) =
            case model.selectedContentType of
                Project ->
                    let
                        view =
                            if model.user.projects == [] then
                                div [ class "well" ]
                                    [ p [] [ text "You don't have any projects yet." ]
                                    , p []
                                        [ text "Click 'New Project' to create a new project or just click "
                                        , a [ onClick OpenNewProjectDialog ] [ text "here" ]
                                        , text "."
                                        ]
                                    ]
                            else
                                Table.view (projectTableConfig model.selectedProjectRowId model.user.user_id) model.projectTableState model.user.projects

                        newButton =
                            button [ class "btn btn-default pull-right", onClick OpenNewProjectDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " New Project" ]
                    in
                    ( "Projects", view, List.length model.user.projects, newButton )

                Sample ->
                    let
                        view =
                            if samples == [] then
                                div [ class "well" ]
                                    [ p [] [ text "You don't have any samples yet." ]
                                    , p []
                                        [ text "First you need a project: click 'New' then 'Project' to create a new project or just click "
                                        , a [ onClick OpenNewProjectDialog ] [ text "here" ]
                                        , text "."
                                        ]
                                    , p []
                                        [ text "To add samples to an existing project, open it and click 'Add Sample'."
                                        ]
                                    ]
                            else
                                Table.view (sampleTableConfig model.user.user_id model.user.projects model.selectedSampleRowId) model.sampleTableState samples
                    in
                    ( "Samples", view, List.length samples, text "")

                Storage ->
                    ( "Data Store", FileBrowser.view model.fileBrowser |> Html.map FileBrowserMsg, FileBrowser.numItems model.fileBrowser, text "")

                Activity ->
                    let
                        view =
                            if model.user.log == [] then
                                div [ class "well" ]
                                    [ p [] [ text "You don't have any activity yet." ]
                                    ]
                            else
                                Table.view (activityTableConfig model.selectedActivityRowId) model.activityTableState model.user.log
                    in
                    ( "Activity", view, List.length model.user.log, text "")

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ] [ text numStr ]

        viewHeader =
            div [ style [("color","dimgray"), ("font-weight","bold"), ("font-size","1.75em")] ] [ text title, text " ", numShowing, buttonPanel ]
    in
    [ div []
        [ div [ class "col-sm-9", style [("margin-bottom","1em")] ] [ viewHeader ]
        , div [ class "col-sm-3" ] []
        ]
    , div []
        [ div [ class "col-sm-9" ]
            [ div [ style [("height","80vh"), ("overflow-y","auto")] ] [ viewTable ]
            ]
        , div [ class "col-sm-3" ]
            [ viewInfo model ]
        ]
    ]


samplesFromProjects : List Data.User.Project -> List Sample
samplesFromProjects projects =
    List.map .samples projects |> List.concat


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-hover"
    ]


toProjectRowAttrs : Int -> Data.User.Project -> List (Attribute Msg)
toProjectRowAttrs selectedRowId data =
    onClick (SelectProjectRow data.project_id)
    :: (if (data.project_id == selectedRowId) then
            [ attribute "class" "active" ]
         else
            []
        )


toSampleRowAttrs : Int -> Data.User.Sample -> List (Attribute Msg)
toSampleRowAttrs selectedRowId data =
    onClick (SelectSampleRow data.sample_id)
    :: (if (data.sample_id == selectedRowId) then
            [ attribute "class" "active" ]
        else
            []
        )


toActivityRowAttrs : String -> Data.User.LogEntry -> List (Attribute Msg)
toActivityRowAttrs selectedRowId data =
    onClick (SelectActivityRow data.id)
    :: (if (data.id == selectedRowId) then
            [ attribute "class" "active" ]
        else
            []
        )


projectTableConfig : Int -> Int -> Table.Config Data.User.Project Msg
projectTableConfig selectedRowId currentUserId =
    Table.customConfig
        { toId = toString << .project_id
        , toMsg = SetProjectTableState
        , columns =
            [ projectNameColumn
            , Table.stringColumn "Type" .project_type
            , projectOwnerColumn currentUserId
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toProjectRowAttrs selectedRowId }
        }


projectNameColumn : Table.Column Data.User.Project Msg
projectNameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = projectLink
        , sorter = Table.unsortable
        }


projectLink : Data.User.Project -> Table.HtmlDetails Msg
projectLink project =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project project.project_id) ]
            [ text project.project_name ]
        ]


projectOwnerColumn : Int -> Table.Column Data.User.Project Msg
projectOwnerColumn currentUserId =
    Table.customColumn
        { name = "Owner"
        , viewData = viewProjectOwner currentUserId
        , sorter = Table.unsortable
        }


viewProjectOwner : Int -> Data.User.Project -> String
viewProjectOwner currentUserId project =
    case List.filter (\u -> u.permission == "owner") project.users of
        user :: [] ->
            if user.user_id == currentUserId then
                "You"
            else
                user.first_name ++ " " ++ user.last_name

        _ ->
            ""


sampleTableConfig : Int -> List Data.User.Project -> Int -> Table.Config Data.User.Sample Msg
sampleTableConfig currentUserId projects selectedRowId =
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetSampleTableState
        , columns =
            [ sampleNameColumn
            , Table.stringColumn "Type" .sample_type
            , sampleOwnerColumn currentUserId projects
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toSampleRowAttrs selectedRowId }
        }


sampleNameColumn : Table.Column Data.User.Sample Msg
sampleNameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = sampleLink
        , sorter = Table.unsortable
        }


sampleLink : Data.User.Sample -> Table.HtmlDetails Msg
sampleLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample sample.sample_id) ]
            [ text sample.sample_name ]
        ]


sampleOwnerColumn : Int -> List Data.User.Project -> Table.Column Data.User.Sample Msg
sampleOwnerColumn currentUserId projects =
    Table.customColumn
        { name = "Owner"
        , viewData = viewSampleOwner currentUserId projects
        , sorter = Table.unsortable
        }


viewSampleOwner : Int -> List Data.User.Project -> Data.User.Sample -> String
viewSampleOwner currentUserId projects sample =
    case List.filter (\p -> p.project_id == sample.project.project_id) projects of
        project :: [] ->
            case List.filter (\u -> u.permission == "owner") project.users of
                user :: [] ->
                    if user.user_id == currentUserId then
                        "You"
                    else
                        user.first_name ++ " " ++ user.last_name

                _ ->
                    ""

        _ ->
            ""


activityTableConfig : String -> Table.Config Data.User.LogEntry Msg
activityTableConfig selectedRowId =
    Table.customConfig
        { toId = .id
        , toMsg = SetActivityTableState
        , columns =
            [ dateColumn
            , Table.stringColumn "Description" .title
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toActivityRowAttrs selectedRowId }
        }


dateColumn : Table.Column Data.User.LogEntry Msg
dateColumn =
    Table.customColumn
        { name = "Date"
        , viewData = .date
        , sorter = Table.decreasingOrIncreasingBy .date
        }


viewInfo : Model -> Html Msg
viewInfo model =
    let
        samples =
            samplesFromProjects model.user.projects

        info =
            case model.selectedContentType of
                Project ->
                    case List.filter (\p -> p.project_id == model.selectedProjectRowId) model.user.projects of
                        [] ->
                            if model.user.projects == [] then
                                text ""
                            else
                                div []
                                    [ p [] [ text "Here are projects you created or are shared with you." ]
                                    , p [] [ text "Click on a project row to see detailed info." ]
                                    , p [] [ text "To create a new project click the 'New' button." ]
                                    ]

                        project :: _ ->
                            let
                                isDeleteable =
                                    project.users |> List.filter (\u -> u.permission == "owner") |> List.map .user_id |> List.member model.user.user_id
                            in
                            div []
                                [ View.Project.viewInfo project
                                , View.Project.viewActions project isDeleteable (OpenConfirmationDialog "Are you sure you want to remove this project and its associated samples?" (RemoveProject project.project_id))
                                ]

                Sample ->
                    case List.filter (\s -> s.sample_id == model.selectedSampleRowId) samples of
                        [] ->
                            if (samples == []) then
                                text ""
                            else
                                div []
                                    [ p [] [ text "Here are samples you created or are shared with you." ]
                                    , p [] [ text "Click on a sample row to see detailed info." ]
                                    , p [] [ text "To create a new sample, create/open a project and click 'Add Sample'." ]
                                    ]

                        sample :: _ ->
                            div []
                                [ View.Sample.viewInfo sample
                                , View.Sample.viewActions sample (OpenConfirmationDialog "Are you sure you want to remove this sample?" (RemoveSample sample.sample_id))
                                ]

                Storage ->
                    case FileBrowser.getSelected model.fileBrowser of
                        [] ->
                            p [] [ text "Here are the contents of your CyVerse Data Store home directory.", br [] [], text "Double-click to open a directory." ]

                        file :: _ ->
                            viewFileInfo file

                Activity ->
                    case List.filter (\entry -> entry.id == model.selectedActivityRowId) model.user.log of
                        [] ->
                            if model.user.log == [] then
                                text ""
                            else
                                div []
                                [ p [] [ text "Here is your activity on the site." ]
                                , p [] [ text "Click on a row to see detailed info." ]
                                ]

                        entry :: _ ->
                            viewActivity entry
    in
    div [ class "info-panel" ] [ info ]


viewFileInfo : FileResult -> Html Msg
viewFileInfo file =
    let
        myLocale =
            { usLocale | decimals = 0 }

        deleteText =
            "Are you sure you want to remove the " ++
                (if file.type_ == "dir" then
                    "directory"
                 else
                    file.type_
                ) ++
                " '" ++ file.name ++ "'?"

        deleteMsg =
            FileBrowserMsg (FileBrowser.OpenConfirmationDialog deleteText (FileBrowser.DeletePath file.path))

        deUrl =
            "https://de.cyverse.org/de/?type=data&folder=/iplant/home" ++ file.path --TODO move base url to config file
    in
    div [ class "table-responsive" ]
        [ table [ class "table info-table" ]
            [ tbody []
                [ colgroup []
                    [ col [ class "col-md-1" ] [] ]
                , tr []
                    [ th [] [ text "Name " ]
                    , td [] [ text file.name ]
                    ]
                , tr []
                    [ th [] [ text "Type " ]
                    , td [] [ text file.type_ ]
                    ]
                , tr []
                    [ th [] [ text "Size " ]
                    , td [] [ text ((file.length |> toFloat |> format myLocale) ++ " bytes") ]
                    ]
                , tr []
                    [ th [] [ text "Last modified " ]
                    , td [] [ text file.lastModified ]
                    ]
--                , tr []
--                    [ td [] [ button [ class "btn btn-link btn-xs" ]
--                        [ span [ class "glyphicon glyphicon-plus" ] [], text " Add to Sample" ] ]
--                    ]
                , tr []
                    [ td []
                        [ button [ class "btn btn-link btn-xs" ]
                            [ a [ href deUrl, target "_blank" ]
                                [ span [ class "glyphicon glyphicon-share-alt" ] [], text " View in CyVerse DE" ]
                            ]
                        ]
                    ]
--                , tr []
--                    [ td [] [ button [ class "btn btn-link btn-xs" ]
--                        [ span [ class "glyphicon glyphicon-cloud-download" ] [], text " Download" ] ]
--                    ]
                , tr []
                    [ td []
                        [ button [ class "btn btn-link btn-xs", onClick deleteMsg ]
                            [ span [ class "glyphicon glyphicon-trash" ] [], text " Delete"
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewActivity : LogEntry -> Html Msg
viewActivity entry =
    div [ class "table-responsive" ]
        [ table [ class "table info-table" ]
            [ tbody []
                [ colgroup []
                    [ col [ class "col-md-1" ] [] ]
                , tr []
                    [ th [] [ text "Description " ]
                    , td [] [ text entry.title ]
                    ]
                , tr []
                    [ th [] [ text "Date " ]
                    , td [] [ text entry.date ]
                    ]
                , tr []
                    [ th [] [ text "ID " ]
                    , td [] [ text entry.id ]
                    ]
                ]
            ]
        ]


newProjectDialogConfig : Model -> Dialog.Config Msg
newProjectDialogConfig model =
    let
        content =
            case model.showNewProjectBusy of
                False ->
                    input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new project", onInput SetNewProjectName ] []

                True ->
                    spinner

        footer =
            let
                disable =
                    disabled model.showNewProjectBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseNewProjectDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick CreateNewProject, disable ] [ text "OK" ]
                    ]
    in
    { closeMessage = Just CloseNewProjectDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "New Project" ])
    , body = Just content
    , footer = Just footer
    }


fileUploadDialogConfig : Model -> Dialog.Config Msg
fileUploadDialogConfig model =
    let
        content =
            case model.fileUploadError of
                Nothing ->
                    spinner

                Just error ->
                    text error

        footer =
            button [ class "btn btn-default", onClick CloseFileUploadDialog ] [ text "Cancel" ]
    in
    { closeMessage = Just CloseFileUploadDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Uploading File" ])
    , body = Just content
    , footer = Just footer
    }