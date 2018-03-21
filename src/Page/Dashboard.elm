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
import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import Util exposing ((=>))
import View.FileBrowser as FileBrowser
import View.Spinner exposing (spinner)
import View.Project
import View.Sample



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
    , fileBrowser : FileBrowser.Model
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
                    , fileBrowser = FileBrowser.init session Nothing
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
    | SelectContent ContentType
    | SelectProjectRow Int
    | SelectSampleRow Int
    | FileBrowserMsg FileBrowser.Msg


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
                    let
                        (subModel, subCmd) =
                            FileBrowser.update session FileBrowser.RefreshPath model.fileBrowser
                    in
                    { model | selectedContentType = contentType, fileBrowser = subModel } => Cmd.map FileBrowserMsg subCmd

                _ ->
                    { model | selectedContentType = contentType } => Cmd.none


        SetProjectTableState newState ->
            { model | projectTableState = newState } => Cmd.none

        SetSampleTableState newState ->
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

        FileBrowserMsg subMsg ->
            let
                ( newFileBrowser, subCmd ) =
                    FileBrowser.update session subMsg model.fileBrowser
            in
            { model | fileBrowser = newFileBrowser } => Cmd.map FileBrowserMsg subCmd



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
                    ( "Data Store", FileBrowser.view model.fileBrowser |> Html.map FileBrowserMsg, FileBrowser.numItems model.fileBrowser )

                Activity ->
                    ( "Activity", text "Coming soon ...", 0 )

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
                            View.Project.viewInfo project

                Sample ->
                    case List.filter (\s -> s.sample_id == model.selectedSampleRowId) model.user.samples of
                        [] ->
                            p [] [ text "Here are samples you've created.", br [] [], text "To create a new sample click the 'New' button." ]

                        sample :: _ ->
                            View.Sample.viewInfo sample

                Storage ->
                    case FileBrowser.getSelected model.fileBrowser of
                        [] ->
                            p [] [ text "Here are the contents of your CyVerse Data Store home directory.", br [] [], text "Double-click to open a directory." ]

                        file :: _ ->
                            viewFileInfo file

                Activity ->
                    text ""
    in
    div [ class "col-sm-3" ] [ info ]


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
                , td [] [ text ((file.length |> toFloat |> format myLocale) ++ " bytes") ]
                ]
            , tr []
                [ th [] [ text "Last modified " ]
                , td [] [ text file.lastModified ]
                ]
            , tr []
                [ td [] [ button [ class "btn btn-link btn-xs" ]
                    [ span [ class "glyphicon glyphicon-plus" ] [], text " Add to Sample" ] ]
                ]
            , tr []
                [ td []
                    [ button [ class "btn btn-link btn-xs" ]
                        [ a [ href deUrl, target "_blank" ]
                            [ span [ class "glyphicon glyphicon-share-alt" ] [], text " View in CyVerse DE" ]
                        ]
                    ]
                ]
            , tr []
                [ td [] [ button [ class "btn btn-link btn-xs" ]
                    [ span [ class "glyphicon glyphicon-cloud-download" ] [], text " Download" ] ]
                ]
            , tr []
                [ td []
                    [ button [ class "btn btn-link btn-xs", onClick deleteMsg ]
                        [ span [ class "glyphicon glyphicon-trash" ] [], text " Delete"
                        ]
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
