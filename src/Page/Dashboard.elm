module Page.Dashboard exposing (Model, Msg(..), init, update, view)

import Data.Session exposing (Session)
import Data.Project exposing (Project)
import Data.User exposing (User)
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
import Request.User
import Request.Agave
import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import Util exposing ((=>))



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
    , selectedProjectRowId : Int
    , selectedSampleRowId : Int
    , selectedDsRowId : String
    , dataStoreFiles : List FileResult
    , topDsPath : String
    , currentDsPath : String
    , dsBusy : Bool
    , dsPathFilterType : String
    , errorMessage : Maybe String
    , newFolderName : String
    , showNewFolderDialog : Bool
    , showNewFolderBusy : Bool
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

        loadUser id =
            Request.User.get id |> Http.toTask
    in
    loadUser user_id
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
                    , selectedProjectRowId = 0
                    , selectedSampleRowId = 0
                    , selectedDsRowId = ""
                    , dataStoreFiles = []
                    , topDsPath = "/" ++ user.user_name
                    , currentDsPath = "/" ++ user.user_name
                    , dsBusy = False
                    , dsPathFilterType = "Home"
                    , errorMessage = Nothing
                    , newFolderName = ""
                    , showNewFolderDialog = False
                    , showNewFolderBusy = False
                    }
            )
            |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = OpenNewProjectDialog
    | CloseNewProjectDialog
    | SetNewProjectName String
    | CreateNewProject
    | CreateNewProjectCompleted (Result Http.Error Project)
    | SetProjectTableState Table.State
    | SetSampleTableState Table.State
    | SetDSTableState Table.State
    | SelectContent ContentType
    | SelectProjectRow Int
    | SelectSampleRow Int
    | SelectDsRow String
    | OpenDsRow String
    | RefreshDsPath
    | GetDataStoreCompleted (Result Http.Error (List FileResult))
    | SetDsPath String
    | FilterDsPathType String
    | OpenNewFolderDialog
    | CloseNewFolderDialog
    | SetNewFolderName String
    | CreateNewFolder
    | CreateNewFolderCompleted (Result Http.Error (Request.Agave.Response FileResult))
    | DeletePath String
    | DeletePathCompleted (Result Http.Error Request.Agave.EmptyResponse)


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
            let
                (cmd, busy) =
                    case contentType of
                        Storage ->
                            ( Task.attempt GetDataStoreCompleted (Request.Agave.getFileList session.token model.currentDsPath |> Http.toTask |> Task.map .result)
                            , True
                            )

                        _ ->
                            ( Cmd.none, False )
            in
            { model | selectedContentType = contentType, dsBusy = busy, errorMessage = Nothing } => cmd

        SetProjectTableState newState ->
            { model | projectTableState = newState } => Cmd.none

        SetSampleTableState newState ->
            { model | sampleTableState = newState } => Cmd.none

        SetDSTableState newState ->
            { model | sampleTableState = newState } => Cmd.none

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

        SelectDsRow id ->
            let
                selectedRowId =
                    if model.selectedDsRowId == id then
                        "" -- unselect
                    else
                        id
            in
            { model | selectedDsRowId = selectedRowId } => Cmd.none

        OpenDsRow id ->
            update session (SelectContent Storage) { model | currentDsPath = id }

        RefreshDsPath ->
            update session (OpenDsRow model.currentDsPath) model

        GetDataStoreCompleted (Ok files) ->
            let
                -- Manufacture a previous path
                previous =
                    { name = ".. (previous)"
                    , path = determinePreviousPath model.currentDsPath
                    , type_ = "dir"
                    , format = ""
                    , length = 0
                    , lastModified = ""
                    , permissions = ""
                    , mimeType = ""
                    , system = ""
                    }

                -- Remove current path
                filtered =
                    List.filter (\f -> f.name /= ".") files

                newFiles =
                    -- Only show previous path if not at top-level
                    if model.topDsPath /= model.currentDsPath then
                        previous :: filtered
                    else
                        filtered
            in
            { model | dataStoreFiles = newFiles, dsBusy = False } => Cmd.none

        GetDataStoreCompleted (Err error) ->
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
            { model | errorMessage = Just msg, dsBusy = False } => cmd

        SetDsPath path ->
            { model | currentDsPath = path } => Cmd.none

        FilterDsPathType filterType ->
            let
                path =
                    case filterType of
                        "Shared" ->
                            "/shared"

                        _ -> -- Home
                            "/" ++ model.user.user_name
            in
            update session (OpenDsRow path) { model | currentDsPath = path, dsPathFilterType = filterType }

        OpenNewFolderDialog ->
            { model | showNewFolderDialog = True } => Cmd.none

        CloseNewFolderDialog ->
            { model | showNewFolderDialog = False } => Cmd.none

        SetNewFolderName name ->
            { model | newFolderName = name } => Cmd.none

        CreateNewFolder ->
            let
                createFolder =
                    Request.Agave.mkdir session.token model.currentDsPath model.newFolderName |> Http.toTask
            in
            { model | showNewFolderBusy = True } => Task.attempt CreateNewFolderCompleted createFolder

        CreateNewFolderCompleted (Ok _) ->
            update session RefreshDsPath { model | showNewFolderDialog = False }

        CreateNewFolderCompleted (Err error) ->
            { model | showNewFolderDialog = False } => Cmd.none

        DeletePath path ->
            let
                delete =
                    Request.Agave.delete session.token path |> Http.toTask
            in
            { model | dsBusy = True } => Task.attempt DeletePathCompleted delete

        DeletePathCompleted (Ok _) ->
            update session RefreshDsPath { model | showNewFolderDialog = False }

        DeletePathCompleted (Err error) ->
            { model | dsBusy = False } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div []
        [ viewMenu model
        , viewContent model
        , viewInfo model
        , Dialog.view
            (if model.showNewProjectDialog then
                Just (newProjectDialogConfig model)
             else if model.showNewFolderDialog then
                Just (newFolderDialogConfig model)
             else
                Nothing
            )
        ]


viewMenu : Model -> Html Msg
viewMenu model =
    div [ class "col-sm-2" ]
        [ viewNewButton
        , div []
            [ button [ class "menu-button", onClick (SelectContent Project) ] [ text "Projects" ]
            ]
        , div []
            [ button [ class "menu-button", onClick (SelectContent Sample) ] [ text "Samples" ]
            ]
        , div []
            [ button [ class "menu-button", onClick (SelectContent Storage) ] [ text "Data Store" ]
            ]
        , div []
            [ button [ class "menu-button", onClick (SelectContent Activity) ] [ text "Activity" ] ]
        ]


viewNewButton : Html Msg
viewNewButton =
    div [ class "btn-group" ]
        [ button [ attribute "aria-expanded" "false", attribute "aria-haspopup" "true", class "btn btn-default dropdown-toggle", attribute "data-toggle" "dropdown", type_ "button" ]
            [ text "NEW "
            , span [ class "caret" ]
                []
            ]
        , ul [ class "dropdown-menu" ]
            [ li []
                [ a [ onClick OpenNewProjectDialog ]
                    [ text "Project" ]
                ]
            , li []
                [ a [ href "#" ]
                    [ text "Sample" ]
                ]
            ]
        ]


viewContent : Model -> Html Msg
viewContent model =
    let
        (title, viewTable, count) =
            case model.selectedContentType of
                Project ->
                    ( "Projects", Table.view (projectTableConfig model.selectedProjectRowId) model.projectTableState model.user.projects, List.length model.user.projects )

                Sample ->
                    ( "Samples", Table.view (sampleTableConfig model.selectedSampleRowId) model.sampleTableState model.user.samples, List.length model.user.samples )

                Storage ->
                    ( "Data Store", viewFileTable model, List.length model.dataStoreFiles )

                Activity ->
                    ( "Activity", text "", 0 )

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
            div [ style [("color","dimgray"), ("font-weight","bold"), ("font-size","1.75em")] ] [ text title, text " ", numShowing ]
    in
    div [ class "col-sm-7" ]
        [ viewHeader
        , br [] []
        , div [ style [("height","80vh"), ("overflow-y","auto")] ] [ viewTable ]
        ]


viewFileTable : Model -> Html Msg
viewFileTable model =
    let
        filterButton label =
            let
                classes =
                    if label == model.dsPathFilterType then
                        "btn btn-default active"
                    else
                        "btn btn-default"
            in
            button [ class classes, type_ "button", onClick (FilterDsPathType label) ] [ text label ]
    in
    div []
        [ Html.form [ class "form-inline" ]
            [ div [ class "form-group" ]
                [ --label [ type_ "text" ] [ text "Path: " ]
                div [ class "input-group" ]
                    [ div [ class "input-group-btn" ]
                        [ filterButton "Home"
                        , filterButton "Shared"
                        ]
                    , input [ class "form-control", type_ "text", size 45, value model.currentDsPath, onInput SetDsPath ] []
                    , span [ class "input-group-btn" ]
                        [ button [ class "btn btn-default", type_ "button", onClick (OpenDsRow model.currentDsPath) ] [ text "Go " ]
                        ]
                    ]
                , button [ style [("visibility","hidden")] ] [ text " " ] -- FIXME: spacer
                , button [ class "btn btn-default btn-sm", type_ "button", onClick OpenNewFolderDialog ] [ span [ class "glyphicon glyphicon-folder-close" ] [], text " New Folder" ]
                , div [ class "btn-group" ]
                    [ button [ class "btn btn-default btn-sm dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ]
                        [ span [ class "glyphicon glyphicon-cloud-upload" ] []
                        , text " Upload File "
                        , span [ class "caret" ] []
                        ]
                    , ul [ class "dropdown-menu" ]
                        [ li [] [ a [] [ text "From local" ] ]
                        , li [] [ a [] [ text "From URL (FTP/HTTP)" ] ]
                        , li [] [ a [] [ text "From NCBI" ] ]
                        , li [] [ a [] [ text "From EBI" ] ]
                        ]
                    ]
                ]
            ]
        , br [] []
        , case model.errorMessage of
            Nothing ->
                case model.dsBusy of
                    True ->
                        div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

                    False ->
                        Table.view (dsTableConfig model.selectedDsRowId) model.dsTableState model.dataStoreFiles

            Just msg ->
                div [ class "alert alert-danger" ] [ text msg ]
        ]


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


toDsRowAttrs : String -> FileResult -> List (Attribute Msg)
toDsRowAttrs selectedPath data =
    onClick (SelectDsRow data.path)
    :: onDoubleClick (OpenDsRow data.path)
    :: (if (data.path == selectedPath) then
            [ attribute "class" "active" ]
        else
            []
        )


projectTableConfig : Int -> Table.Config Data.User.Project Msg
projectTableConfig selectedRowId =
    Table.customConfig
        { toId = toString << .project_id
        , toMsg = SetProjectTableState
        , columns =
            [ projectNameColumn
            , Table.stringColumn "Type" .project_type
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


sampleTableConfig : Int -> Table.Config Data.User.Sample Msg
sampleTableConfig selectedRowId =
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetSampleTableState
        , columns =
            [ sampleNameColumn
            , Table.stringColumn "Type" .sample_type
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


dsTableConfig : String -> Table.Config FileResult Msg
dsTableConfig selectedRowId =
    Table.customConfig
        { toId = .path
        , toMsg = SetDSTableState
        , columns =
            [ dsNameColumn
            , dsSizeColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toDsRowAttrs selectedRowId }
        }


dsNameColumn : Table.Column FileResult Msg
dsNameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = dsLink
        , sorter =
            Table.increasingOrDecreasingBy
                (\data -> if data.type_ == "dir" then "..." ++ data.name else data.name) -- sort dirs before files
        }


dsLink : FileResult -> Table.HtmlDetails Msg
dsLink file =
    if file.type_ == "dir" then
        Table.HtmlDetails []
            [ a [ onClick (OpenDsRow file.path) ] [ text file.name ]
            ]
    else
        Table.HtmlDetails [] [ text file.name ]


dsSizeColumn : Table.Column FileResult Msg
dsSizeColumn =
    Table.veryCustomColumn
        { name = "Size"
        , viewData = (\file -> Table.HtmlDetails [] [ if (file.length > 0) then (text (toString file.length)) else text "" ])
        , sorter = Table.increasingOrDecreasingBy .length
        }


viewInfo : Model -> Html Msg
viewInfo model =
    let
        info =
            case model.selectedContentType of
                Project ->
                    case List.filter (\p -> p.project_id == model.selectedProjectRowId) model.user.projects of
                        [] ->
                            p [] [ text "Here are projects you've created.", br [] [], text "To create a new project click the 'New' button." ]

                        project :: _ ->
                            viewProjectInfo project

                Sample ->
                    case List.filter (\s -> s.sample_id == model.selectedSampleRowId) model.user.samples of
                        [] ->
                            p [] [ text "Here are samples you've created.", br [] [], text "To create a new sample click the 'New' button." ]

                        sample :: _ ->
                            viewSampleInfo sample

                Storage ->
                    case List.filter (\f -> f.path == model.selectedDsRowId) model.dataStoreFiles of
                        [] ->
                            p [] [ text "Here are the contents of your CyVerse Data Store home directory.", br [] [], text "Double-click to open a directory." ]

                        file :: _ ->
                            viewFileInfo file

                Activity ->
                    text ""
    in
    div [ class "col-sm-3" ] [ info ]


viewProjectInfo : Data.User.Project -> Html Msg
viewProjectInfo project =
    div []
        [ table [ class "info-table" ]
            [ tr []
                [ th [] [ text "Name " ]
                , td [] [ text project.project_name ]
                ]
            , tr []
                [ th [] [ text "Type " ]
                , td [] [ text project.project_type ]
                ]
            ]
        ]


viewSampleInfo : Data.User.Sample -> Html Msg
viewSampleInfo sample =
    div []
        [ table [ class "info-table" ]
            [ tr []
                [ th [] [ text "ID "]
                , td [] [ text (toString sample.sample_id) ]
                ]
            , tr []
                [ th [] [ text "Name " ]
                , td [] [ text sample.sample_name ]
                ]
            , tr []
                [ th [] [ text "Type " ]
                , td [] [ text sample.sample_type ]
                ]
            ]
        ]


viewFileInfo : FileResult -> Html Msg
viewFileInfo file =
    div []
        [ table [ class "info-table" ]
            [ tr []
                [ th [] [ text "Name " ]
                , td [] [ text file.name ]
                ]
            , tr []
                [ th [] [ text "Type " ]
                , td [] [ text file.type_ ]
                ]
            , tr []
                [ th [] [ text "Size " ]
                , td [] [ text (toString file.length) ]
                ]
            , tr []
                [ th [] [ text "Last modified " ]
                , td [] [ text file.lastModified ]
                ]
            , tr []
                [ td [] [ button [ class "btn btn-default btn-xs" ] [ span [ class "glyphicon glyphicon-plus" ] [], text "Add to Sample" ] ]
                ]
            , tr []
                [ td [] [ button [ class "btn btn-default btn-xs" ] [ span [ class "glyphicon glyphicon-share-alt" ] [], text "View in CyVerse DE" ] ]
                ]
            , tr []
                [ td [] [ button [ class "btn btn-default btn-xs" ] [ span [ class "glyphicon glyphicon-cloud-download" ] [], text "Download" ] ]
                ]
            , tr []
                [ td [] [ button [ class "btn btn-default btn-xs", onClick (DeletePath file.path) ] [ span [ class "glyphicon glyphicon-trash" ] [], text "Delete" ] ]
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
                    div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

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


newFolderDialogConfig : Model -> Dialog.Config Msg
newFolderDialogConfig model =
    let
        content =
            case model.showNewFolderBusy of
                False ->
                    input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new folder", onInput SetNewFolderName ] []

                True ->
                    div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

        footer =
            let
                disable =
                    disabled model.showNewFolderBusy
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


determinePreviousPath : String -> String
determinePreviousPath path =
    let
        l =
            String.split "/" path
        n =
            List.length l
    in
    List.take (n-1) l |> String.join "/"
