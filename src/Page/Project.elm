module Page.Project exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Project exposing (Project, Investigator, Domain, Assembly, CombinedAssembly, Sample, Publication, ProjectGroup, User)
import Data.ProjectGroup
import Data.Sample
import Data.Investigator
import Data.User
import Data.Session as Session exposing (Session)
import Data.Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Dialog
import Http
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Page.Error as Error exposing (PageLoadError)
import Request.Project
import Request.ProjectGroup
import Request.Sample
import Request.Investigator
import Request.Publication
import Request.User
import Route
import Task exposing (Task)
import Table exposing (defaultCustomizations)
import View.Cart as Cart
import View.Spinner exposing (spinner)
import View.Dialog exposing (confirmationDialogConfig, errorDialogConfig)
import View.SearchableDropdown
import View.TagsDropdown
import View.Tags
import Util exposing ((=>), capitalize, pluralize)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , project_id : Int
    , project : Project
    , cart : Cart.Model
    , loadingAssemblies : Bool
    , loadedAssemblies : Bool
    , assemblies : List Assembly
    , assemblyTableState : Table.State
    , assemblyQuery : String
    , loadingCombinedAssemblies : Bool
    , loadedCombinedAssemblies : Bool
    , combined_assemblies : List CombinedAssembly
    , combinedAssemblyTableState : Table.State
    , combinedAssemblyQuery : String
    , isEditable : Bool
    , currentUserId : Maybe Int
    , confirmationDialog : Maybe (Dialog.Config Msg)
    , showShareDialog : Bool
    , showShareDialogBusy : Bool
    , shareDropdownState : View.SearchableDropdown.State
    , showEditInfoDialog : Bool
    , newProjectName : String
    , newProjectCode : String
    , newProjectType : String
    , newProjectURL : String
    , domainDropdownState : View.TagsDropdown.State
    , groupDropdownState : View.TagsDropdown.State
    , showNewSampleDialog : Bool
    , showNewSampleBusy : Bool
    , newSampleName : String
    , investigatorDropdownState : View.SearchableDropdown.State
    , newInvestigatorTagState : View.Tags.State
    , showAddOrEditPublicationDialog : Bool
    , showAddOrEditPublicationBusy : Bool
    , publicationIdToEdit : Maybe Int
    , newPublicationTitle : String
    , newPublicationAuthors : String
    , newPublicationDate : String
    , newPublicationPubMedID : String
    , newPublicationDOI : String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        loadProject =
            Request.Project.get session.token id |> Http.toTask

        userId =
            Maybe.map .user_id session.user

        isEditable project =
            case userId of
                Nothing ->
                    False

                Just userId ->
                    List.any (\u -> u.user_id == userId && (u.permission == "owner" || u.permission == "read-write")) project.users
    in
    loadProject
        |> Task.andThen
            (\project ->
                Task.succeed
                    { pageTitle = "Project"
                    , project_id = id
                    , project = project
                    , cart = Cart.init session.cart Cart.Editable
                    , loadingAssemblies = False
                    , loadedAssemblies = False
                    , assemblies = []
                    , assemblyTableState = Table.initialSort "Name"
                    , assemblyQuery = ""
                    , loadingCombinedAssemblies = False
                    , loadedCombinedAssemblies = False
                    , combined_assemblies = []
                    , combinedAssemblyTableState = Table.initialSort "Name"
                    , combinedAssemblyQuery = ""
                    , isEditable = isEditable project
                    , currentUserId = userId
                    , confirmationDialog = Nothing
                    , showShareDialog = False
                    , showShareDialogBusy = False
                    , shareDropdownState = View.SearchableDropdown.init
                    , showEditInfoDialog = False
                    , newProjectName = ""
                    , newProjectCode = ""
                    , newProjectType = ""
                    , newProjectURL = ""
                    , domainDropdownState = View.TagsDropdown.init (List.map (\d -> (d.domain_id, d.domain_name)) project.domains)
                    , groupDropdownState = View.TagsDropdown.init (List.map (\g -> (g.project_group_id, g.group_name)) project.project_groups)
                    , showNewSampleDialog = False
                    , showNewSampleBusy = False
                    , newSampleName = ""
                    , investigatorDropdownState = View.SearchableDropdown.init
                    , newInvestigatorTagState = View.Tags.init (List.map (\i -> (i.investigator_id, i.investigator_name)) project.investigators)
                    , showAddOrEditPublicationDialog = False
                    , showAddOrEditPublicationBusy = False
                    , publicationIdToEdit = Nothing
                    , newPublicationTitle = ""
                    , newPublicationAuthors = ""
                    , newPublicationDate = ""
                    , newPublicationPubMedID = ""
                    , newPublicationDOI = ""
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | GetAssemblies
    | SetAssemblies (List Assembly)
    | SetAssemblyQuery String
    | SetAssemblyTableState Table.State
    | GetCombinedAssemblies
    | SetCombinedAssemblies (List CombinedAssembly)
    | SetCombinedAssemblyQuery String
    | SetCombinedAssemblyTableState Table.State
    | OpenConfirmationDialog String Msg
    | CloseConfirmationDialog
    | OpenShareDialog
    | CloseShareDialog
    | SetShareUserName String
    | SearchUsersAndGroupsCompleted (Result Http.Error (List Data.User.User, List Data.ProjectGroup.ProjectGroup))
    | ShareWithUser String Int String --FIXME create new type for permission instead of using string
    | ShareWithUserCompleted (Result Http.Error Project)
    | AddToProjectGroupCompleted (Result Http.Error (List Data.ProjectGroup.ProjectGroup))
    | RemoveFromProjectGroup Int
    | RemoveFromProjectGroupCompleted (Result Http.Error String)
    | UnshareWithUser Int
    | UnshareWithUserCompleted (Result Http.Error String)
    | OpenEditInfoDialog
    | CloseEditInfoDialog
    | SetNewProjectName String
    | SetNewProjectCode String
    | SetNewProjectType String
    | SetNewProjectURL String
    | AddNewProjectDomain Int String
    | RemoveNewProjectDomain Int
    | UpdateProjectInfo
    | UpdateProjectInfoCompleted (Result Http.Error Project)
    | OpenNewSampleDialog
    | CloseNewSampleDialog
    | SetNewSampleName String
    | CreateNewSample
    | CreateNewSampleCompleted (Result Http.Error Data.Sample.Sample)
    | RemoveSample Int
    | RemoveSampleCompleted (Result Http.Error Project)
    | SetInvestigatorName String
    | SearchInvestigatorCompleted (Result Http.Error (List Data.Investigator.Investigator))
    | SelectInvestigatorToAdd Int String
    | RemoveInvestigator Int
    | OpenAddOrEditPublicationDialog (Maybe Publication)
    | CloseAddOrEditPublicationDialog
    | SetPublicationTitle String
    | SetPublicationAuthors String
    | SetPublicationDate String
    | SetPublicationPubMedID String
    | SetPublicationDOI String
    | AddPublication
    | AddPublicationCompleted (Result Http.Error Project)
    | UpdatePublication Int
    | UpdatePublicationCompleted (Result Http.Error Project)
    | RemovePublication Int
    | RemovePublicationCompleted (Result Http.Error Project)


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    let
        loadProject _ =
            Request.Project.get session.token model.project_id |> Http.toTask
    in
    case msg of
        CartMsg subMsg ->
            let
                _ = Debug.log "Samples.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart

        GetAssemblies ->
            let
                loadAssemblies =
                    Request.Project.getAssemblies model.project_id |> Http.toTask

                handleAssemblies assemblies =
                    case assemblies of
                        Ok assemblies ->
                            SetAssemblies assemblies

                        Err error ->
                            let
                                _ = Debug.log "Error" ("could not retrieve assemblies: " ++ (toString error))
                            in
                            SetAssemblies []
            in
            { model | loadingAssemblies = True } => Task.attempt handleAssemblies loadAssemblies => NoOp

        SetAssemblies assemblies ->
            { model | loadedAssemblies = True, assemblies = assemblies } => Cmd.none => NoOp

        SetAssemblyQuery newQuery ->
            { model | assemblyQuery = newQuery } => Cmd.none => NoOp

        SetAssemblyTableState newState ->
            { model | assemblyTableState = newState } => Cmd.none => NoOp

        GetCombinedAssemblies ->
            let
                loadCombinedAssemblies =
                    Request.Project.getCombinedAssemblies model.project_id |> Http.toTask

                handleCombinedAssemblies combined_assemblies =
                    case combined_assemblies of
                        Ok combined_assemblies ->
                            SetCombinedAssemblies combined_assemblies

                        Err error ->
                            let
                                _ = Debug.log "Error" ("could not retrieve combined_assemblies: " ++ (toString error))
                            in
                            SetCombinedAssemblies []
            in
            { model | loadingCombinedAssemblies = True } => Task.attempt handleCombinedAssemblies loadCombinedAssemblies => NoOp

        SetCombinedAssemblies combined_assemblies ->
            { model | loadedCombinedAssemblies = True, combined_assemblies = combined_assemblies } => Cmd.none => NoOp

        SetCombinedAssemblyQuery newQuery ->
            { model | combinedAssemblyQuery = newQuery } => Cmd.none => NoOp

        SetCombinedAssemblyTableState newState ->
            { model | combinedAssemblyTableState = newState } => Cmd.none => NoOp

        OpenConfirmationDialog confirmationText yesMsg ->
            let
                dialog =
                    confirmationDialogConfig confirmationText CloseConfirmationDialog yesMsg
            in
            { model | confirmationDialog = Just dialog } => Cmd.none => NoOp

        CloseConfirmationDialog ->
            { model | confirmationDialog = Nothing } => Cmd.none => NoOp

        OpenShareDialog ->
            { model | showShareDialog = True } => Cmd.none => NoOp

        CloseShareDialog ->
            { model | showShareDialog = False } => Cmd.none => NoOp

        SetShareUserName name ->
            let
                dropdownState =
                    model.shareDropdownState
            in
            if String.length name >= 3 then
                let
                    searchByName =
                        Task.map2 (\users groups -> (users, groups))
                            (Request.User.searchByName session.token name |> Http.toTask)
                            (Request.ProjectGroup.searchByName name |> Http.toTask)
                in
                { model | shareDropdownState = { dropdownState | value = name } } => Task.attempt SearchUsersAndGroupsCompleted searchByName => NoOp
            else
            { model | shareDropdownState = { dropdownState | value = name, results = [] } } => Cmd.none => NoOp

        SearchUsersAndGroupsCompleted (Ok (users, groups)) ->
            let
                userDisplayName user =
                    user.first_name ++ " " ++ user.last_name ++ " (" ++ user.user_name ++ ")"

                groupDisplayName group =
                    let
                        numUsers =
                            List.length group.users
                    in
                    "Group: " ++ group.group_name ++ " (" ++ (numUsers |> toString) ++ " " ++ (Util.pluralize "user" numUsers) ++ ")"

                results =
                    List.append
                        (List.map (\u -> (u.user_id, userDisplayName u)) users)
                        (List.map (\g -> (g.project_group_id, groupDisplayName g)) groups)

                dropdownState =
                    model.shareDropdownState
            in
            { model | shareDropdownState = { dropdownState | results = results } } => Cmd.none => NoOp

        SearchUsersAndGroupsCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "SearchUsersAndGroupsCompleted" (toString error)
            in
            model => Cmd.none => NoOp

        ShareWithUser permission id name ->
            let
                dropdownState =
                    model.shareDropdownState

                newModel =
                    { model | showShareDialogBusy = True, shareDropdownState = { dropdownState | value = "", results = [] } }
            in
            if String.startsWith "Group: " name then --FIXME total kludge
                let
                    noChange =
                        List.any (\g -> g.project_group_id == id) model.project.project_groups

                    addProjectToProjectGroup =
                        Request.ProjectGroup.addProject session.token id model.project_id |> Http.toTask
                in
                if noChange then
                    model => Cmd.none => NoOp
                else
                    newModel => Task.attempt AddToProjectGroupCompleted addProjectToProjectGroup => NoOp
            else
                let
                    noChange =
                        List.any (\u -> u.user_id == id && u.permission == permission) model.project.users

                    addUserToProject =
                            Request.Project.addUserToProject session.token model.project_id id permission |> Http.toTask
                in
                if noChange then
                    model => Cmd.none => NoOp
                else
                    newModel => Task.attempt ShareWithUserCompleted addUserToProject => NoOp

        ShareWithUserCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showShareDialogBusy = False, project = { newProject | users = project.users } } => Cmd.none => NoOp

        ShareWithUserCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "Error:" (toString error)
            in
            model => Cmd.none => NoOp

        AddToProjectGroupCompleted (Ok groups) ->
            let
                newProject =
                    model.project

                newGroups =
                    List.map (\g -> ProjectGroup g.project_group_id g.group_name g.user_count []) groups -- FIXME kludge due to multiple type defs
            in
            { model | showShareDialogBusy = False, project = { newProject | project_groups = newGroups } } => Cmd.none => NoOp

        AddToProjectGroupCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "Error:" (toString error)
            in
            model => Cmd.none => NoOp

        RemoveFromProjectGroup groupId ->
            let
                removeFromProjectGroup =
                    Request.ProjectGroup.removeProject session.token groupId model.project_id |> Http.toTask

                newProject =
                    model.project

                newGroups =
                    List.filter (\g -> g.project_group_id /= groupId) model.project.project_groups
            in
            { model | project = { newProject | project_groups = newGroups } } => Task.attempt RemoveFromProjectGroupCompleted removeFromProjectGroup => NoOp

        RemoveFromProjectGroupCompleted (Ok _) ->
            model => Cmd.none => NoOp

        RemoveFromProjectGroupCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "Error:" (toString error)
            in
            model => Cmd.none => NoOp

        UnshareWithUser id ->
            let
                removeUser =
                    Request.Project.removeUserFromProject session.token model.project_id id |> Http.toTask

                newProject =
                    model.project

                newUsers =
                    List.filter (\u -> u.user_id /= id) model.project.users
            in
            { model | project = { newProject | users = newUsers } } => Task.attempt UnshareWithUserCompleted removeUser => NoOp

        UnshareWithUserCompleted (Ok _) ->
            model => Cmd.none => NoOp

        UnshareWithUserCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "Error:" (toString error)
            in
            model => Cmd.none => NoOp

        OpenEditInfoDialog ->
            { model
                | showEditInfoDialog = True
                , newProjectName = model.project.project_name
                , newProjectCode = model.project.project_code
                , newProjectType = model.project.project_type
                , newProjectURL = model.project.url
             } => Cmd.none => NoOp

        CloseEditInfoDialog ->
            { model | showEditInfoDialog = False } => Cmd.none => NoOp

        SetNewProjectName name ->
            { model | newProjectName = name } => Cmd.none => NoOp

        SetNewProjectCode code ->
            { model | newProjectCode = code } => Cmd.none => NoOp

        SetNewProjectType type_ ->
            { model | newProjectType = type_ } => Cmd.none => NoOp

        SetNewProjectURL url ->
            { model | newProjectURL = url } => Cmd.none => NoOp

        AddNewProjectDomain id name ->
            let
                newDropdownState =
                    View.TagsDropdown.add id name model.domainDropdownState
            in
            { model | domainDropdownState = newDropdownState } => Cmd.none => NoOp

        RemoveNewProjectDomain id ->
            let
                newDropdownState =
                    View.TagsDropdown.remove id model.domainDropdownState
            in
            { model | domainDropdownState = newDropdownState } => Cmd.none => NoOp

        UpdateProjectInfo ->
            let
                domains =
                    View.TagsDropdown.selected model.domainDropdownState |> List.map (\t -> Domain (Tuple.first t) (Tuple.second t))

                investigators =
                    View.Tags.selected model.newInvestigatorTagState |> List.map (\t -> Investigator (Tuple.first t) (Tuple.second t) "")

                updateInfo =
                    Request.Project.update session.token model.project_id model.newProjectName model.newProjectCode model.newProjectType model.newProjectURL domains investigators |> Http.toTask
            in
            { model | showEditInfoDialog = False } => Task.attempt UpdateProjectInfoCompleted updateInfo => NoOp

        UpdateProjectInfoCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model
                | project =
                    { newProject
                        | project_name = project.project_name
                        , project_code = project.project_code
                        , project_type = project.project_type
                        , url = project.url
                        , domains = project.domains
                        , investigators = project.investigators
                        , project_groups = project.project_groups
                    }
            } => Cmd.none => NoOp

        UpdateProjectInfoCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        OpenNewSampleDialog ->
            { model | showNewSampleDialog = True } => Cmd.none => NoOp

        CloseNewSampleDialog ->
            { model | showNewSampleDialog = False } => Cmd.none => NoOp

        SetNewSampleName name ->
            { model | newSampleName = name } => Cmd.none => NoOp

        CreateNewSample ->
            let
                createSample =
                    Request.Sample.create session.token model.project_id model.newSampleName |> Http.toTask
            in
            { model | showNewSampleBusy = True } => Task.attempt CreateNewSampleCompleted createSample => NoOp

        CreateNewSampleCompleted (Ok sample) ->
            model => Route.modifyUrl (Route.Sample sample.sample_id) => NoOp

        CreateNewSampleCompleted (Err error) ->
            model => Cmd.none => NoOp

        RemoveSample sample_id ->
            let
                removeSample =
                    Request.Sample.remove session.token sample_id |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemoveSampleCompleted removeSample => NoOp

        RemoveSampleCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | project = { newProject | samples = project.samples } } => Cmd.none => NoOp

        RemoveSampleCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        SetInvestigatorName name ->
            let
                searchByName =
                    Request.Investigator.searchByName name |> Http.toTask
                        |> Task.attempt SearchInvestigatorCompleted

                cmd =
                    if String.length name > 0 then
                        searchByName
                    else
                        Cmd.none

                dropdownState =
                    model.investigatorDropdownState
            in
            { model | investigatorDropdownState = { dropdownState | value = name } } => cmd => NoOp

        SearchInvestigatorCompleted (Ok investigators) ->
            let
                results =
                    List.map (\i -> (i.investigator_id, i.investigator_name)) investigators

                dropdownState =
                    model.investigatorDropdownState
            in
            { model | investigatorDropdownState = { dropdownState | results = results } } => Cmd.none => NoOp

        SearchInvestigatorCompleted (Err error) -> -- TODO finish this
            model => Cmd.none => NoOp

        SelectInvestigatorToAdd id name ->
            let
                dropdownState =
                    model.investigatorDropdownState

                tagState =
                    View.Tags.add id name model.newInvestigatorTagState
            in
            { model
                | investigatorDropdownState = { dropdownState | value = "", results = [], selectedId = Just id }
                , newInvestigatorTagState = tagState
            } => Cmd.none => NoOp

        RemoveInvestigator id ->
            let
                tagState =
                    View.Tags.remove id model.newInvestigatorTagState
            in
            { model | newInvestigatorTagState = tagState } => Cmd.none => NoOp

        OpenAddOrEditPublicationDialog publication ->
            { model
                | showAddOrEditPublicationDialog = True
                , showAddOrEditPublicationBusy = False
                , publicationIdToEdit = Maybe.map .publication_id publication
                , newPublicationTitle = Maybe.withDefault "" (Maybe.map .title publication)
                , newPublicationAuthors = Maybe.withDefault "" (Maybe.map .author publication)
                , newPublicationDate = Maybe.withDefault "" (Maybe.map .pub_date publication)
                , newPublicationPubMedID = Maybe.withDefault "" (Maybe.map (.pubmed_id >> toString) publication)
                , newPublicationDOI = Maybe.withDefault "" (Maybe.map .doi publication)
            } => Cmd.none => NoOp

        CloseAddOrEditPublicationDialog ->
            { model | showAddOrEditPublicationDialog = False } => Cmd.none => NoOp

        SetPublicationTitle title ->
            { model | newPublicationTitle = title } => Cmd.none => NoOp

        SetPublicationAuthors authors ->
            { model | newPublicationAuthors = authors } => Cmd.none => NoOp

        SetPublicationDate date ->
            { model | newPublicationDate = date } => Cmd.none => NoOp

        SetPublicationPubMedID id ->
            { model | newPublicationPubMedID = id } => Cmd.none => NoOp

        SetPublicationDOI doi ->
            { model | newPublicationDOI = doi } => Cmd.none => NoOp

        AddPublication ->
            let
                pubmedId =
                    String.toInt model.newPublicationPubMedID |> Result.toMaybe

                createPublication =
                    Request.Publication.create session.token model.project_id model.newPublicationTitle model.newPublicationAuthors model.newPublicationDate pubmedId model.newPublicationDOI |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | showAddOrEditPublicationBusy = True } => Task.attempt AddPublicationCompleted createPublication => NoOp

        AddPublicationCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showAddOrEditPublicationDialog = False, project = { newProject | publications = project.publications } } => Cmd.none => NoOp

        AddPublicationCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            { model | showAddOrEditPublicationDialog = False } => Cmd.none => NoOp

        UpdatePublication pub_id ->
            let
                pubmedId =
                    String.toInt model.newPublicationPubMedID |> Result.toMaybe

                updatePublication =
                    Request.Publication.update session.token pub_id model.newPublicationTitle model.newPublicationAuthors model.newPublicationDate pubmedId model.newPublicationDOI |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | confirmationDialog = Nothing, showAddOrEditPublicationBusy = True } => Task.attempt UpdatePublicationCompleted updatePublication => NoOp

        UpdatePublicationCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showAddOrEditPublicationDialog = False, project = { newProject | publications = project.publications } } => Cmd.none => NoOp

        UpdatePublicationCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        RemovePublication pub_id ->
            let
                removePublication =
                    Request.Publication.remove session.token pub_id |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemovePublicationCompleted removePublication => NoOp

        RemovePublicationCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | project = { newProject | publications = project.publications } } => Cmd.none => NoOp

        RemovePublicationCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small [] [ text model.project.project_name ]
                    , viewShareButton model
                    ]
                ]
            , viewProject model.project model.isEditable
            , viewPublications model.project.publications model.isEditable
            , viewSamples model.cart model.project.samples model.isEditable
            , if not model.isEditable then
                viewAssemblies model
              else
                text ""
            , if not model.isEditable then
                viewCombinedAssemblies model
              else
                text ""
            ]
        , Dialog.view
            (if model.showNewSampleDialog then
                Just (newSampleDialogConfig model)
             else if model.showShareDialog then
                case model.currentUserId of
                    Nothing ->
                        Nothing

                    Just id ->
                        Just (shareDialogConfig id model)
             else if (model.confirmationDialog /= Nothing) then
                model.confirmationDialog
             else if model.showEditInfoDialog then
                Just (editInfoDialogConfig model)
             else if model.showAddOrEditPublicationDialog then
                Just (addOrEditPublicationDialogConfig model)
             else
                Nothing
            )
        ]


viewShareButton : Model -> Html Msg
viewShareButton model =
    if model.project.private == 1 then
        let
            buttonLabel =
                if List.length model.project.users <= 1 then
                    "Project is Private"
                else
                    "Project is Shared"

            numGroups =
                List.length model.project.project_groups

            numUsers =
                List.length model.project.users - 1

            groupsStr =
                List.map .group_name model.project.project_groups |> String.join ", "

            usersStr =
                (toString numUsers) ++ " " ++ (pluralize "user" numUsers)

            shareStr =
                (if numGroups > 0 || numUsers > 0 then
                    "with " ++
                    (if numGroups > 0 then
                        groupsStr
                    else
                        ""
                    ) ++
                    (if numGroups > 0 && numUsers > 0 then
                        " and "
                    else
                        ""
                    ) ++
                    (if numUsers > 0 then
                        usersStr
                    else
                        ""
                    )
                else
                    ""
                )
        in
        button [ class "btn btn-default pull-right", onClick OpenShareDialog ]
            [ div []
                [ span [ class "glyphicon glyphicon-lock" ] []
                , text " "
                , text buttonLabel
                ]
            , div [ class "small-text pull-right" ]
                [ text shareStr ]
            ]

    else
        text ""


viewProject : Project -> Bool -> Html Msg
viewProject project isEditable =
    let
        numDomains =
            List.length project.domains

        domainText =
            case numDomains of
                1 ->
                    "Domain"

                _ ->
                    "Domains"

        editButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-xs", onClick OpenEditInfoDialog ] [ span [ class "glyphicon glyphicon-cog" ] [], text " Edit" ]

                False ->
                    text ""
    in
    table [ class "table" ]
        [ colgroup []
            [ col [ class "col-md-2" ] [] ]
        , tr []
            [ th [] [ text "Name" ]
            , td [] [ text project.project_name ]
            ]
        , tr []
            [ th [] [ text "Code" ]
            , td [] [ text project.project_code ]
            ]
        , tr []
            [ th [] [ text "Project Type" ]
            , td [] [ text project.project_type ]
            ]
        , tr []
            [ th [] [ text domainText ]
            , td [] (viewDomains project.domains)
            ]
        , tr []
            [ th [] [ text "Investigators" ]
            , td [] (viewInvestigators project.investigators)
            ]
--        , tr []
--            [ th [] [ text "Groups" ]
--            , td [] (viewProjectGroups project.project_groups)
--            ]
        , tr []
            [ th [] [ text "URL" ]
            , td [] [ a [ href project.url, target "_blank" ] [ text project.url ] ]
            ]
--        , tr []
--            [ th [] [ text "Description" ]
--            , td [] [ text project.description ]
--            ]
        , tr []
            [ td [] [ editButton ]
            ]
        ]


viewInvestigators : List Investigator -> List (Html msg)
viewInvestigators investigators =
    case List.length investigators of
        0 ->
            [ text "None" ]

        _ ->
            List.sortBy .investigator_name investigators |> List.map viewInvestigator |> List.intersperse (text ", ")


viewInvestigator : Investigator -> Html msg
viewInvestigator investigator =
    a [ Route.href (Route.Investigator investigator.investigator_id) ]
        [ text investigator.investigator_name ]


viewDomains : List Domain -> List (Html msg)
viewDomains domains =
    case List.length domains of
        0 ->
            [ text "None" ]

        _ ->
            List.sortBy .domain_name domains |> List.map viewDomain |> List.intersperse (text ", ")


viewDomain : Domain -> Html msg
viewDomain domain =
    text domain.domain_name


viewProjectGroups : List ProjectGroup -> List (Html msg)
viewProjectGroups groups =
    case List.length groups of
        0 ->
            [ text "None" ]

        _ ->
            List.sortBy .group_name groups |> List.map viewProjectGroup |> List.intersperse (text ", ")


viewProjectGroup : ProjectGroup -> Html msg
viewProjectGroup group =
    a [ Route.href (Route.ProjectGroup group.project_group_id) ]
        [ text group.group_name ]


viewPublications : List Publication -> Bool -> Html Msg
viewPublications pubs isEditable =
    let
        numPubs =
            List.length pubs

        label =
            if numPubs == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text (toString numPubs)
                    ]

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm pull-right", onClick (OpenAddOrEditPublicationDialog Nothing) ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Publication" ]

                False ->
                    text ""
    in
    div []
        [ h2 []
            [ text "Publications "
            , label
            , addButton
            ]
        , if numPubs == 0 then
            text "None"
          else
            table [ class "table table-condensed" ]
                [ tbody [] (List.map (viewPublication isEditable) pubs) ]
        ]


viewPublication : Bool -> Publication -> Html Msg
viewPublication isEditable pub =
    let
        editButtons =
            if isEditable then
                [ button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Are you sure you want to remove this publication?" (RemovePublication pub.publication_id)) ] [ text "Remove" ]
                , button [ class "btn btn-default btn-xs pull-right margin-right", onClick (OpenAddOrEditPublicationDialog (Just pub)) ] [ text "Edit" ]
                ]
            else
                [ text "" ]
    in
    tr []
        [ td []
            [ span [ class "glyphicon glyphicon-file glyphicon-inverse" ] []
            , text " "
            , a [ Route.href (Route.Publication pub.publication_id) ]
                [ text pub.title ]
            ]
        , td []
            [ text pub.author ]
        , td []
            editButtons
        ]


viewSamples : Cart.Model -> List Sample -> Bool -> Html Msg
viewSamples cart samples isEditable =
    let
        numSamples =
            List.length samples

        label =
            case numSamples of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text (toString numSamples)
                        ]

        cartTh =
            th [ class "nowrap" ]
                (text "Cart"
                    ::
                    (if numSamples > 1 then
                        [ br [] []
                        , Cart.addAllToCartButton cart (List.map .sample_id samples) |> Html.map CartMsg
                        ]
                    else
                        []
                    )
                )

        cols =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                , cartTh
                , th [ class "nowrap" ] []
                ]

        rows =
            List.map (viewSample cart isEditable) samples

        body =
            let
                tbl =
                    table [ class "table table-condensed" ] [ tbody [] (cols :: rows) ]
            in
            if numSamples == 0 then
                text "None"

            else if numSamples < 50 then
                tbl

            else
                div [ class "scrollable" ] [ tbl ]

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm pull-right", onClick OpenNewSampleDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Sample" ]

                False ->
                    text ""
    in
    div []
        [ h2 []
            [ text "Samples "
            , label
            , addButton
            ]
        , body
        ]


viewSample : Cart.Model -> Bool -> Sample -> Html Msg
viewSample cart isEditable sample =
    let
        removeButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-xs", onClick (OpenConfirmationDialog "Remove the sample (this cannot be undone)?" (RemoveSample sample.sample_id)) ] [ span [ class "glyphicon glyphicon-trash" ] [] ]

                False ->
                    text ""
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        , td [ class "col-md-1" ] [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg ]
        , td [ class "col-md-1" ] [ removeButton ]
        ]


toTableAttrs : List (Html.Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed" ]


assemblyTableConfig : Table.Config Assembly Msg
assemblyTableConfig =
    Table.customConfig
        { toId = toString << .assembly_id
        , toMsg = SetAssemblyTableState
        , columns =
            [ assemblyNameColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


assemblyNameColumn : Table.Column Assembly Msg
assemblyNameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = assemblyLink
        , sorter = Table.increasingOrDecreasingBy .assembly_name
        }


assemblyLink : Assembly -> Table.HtmlDetails Msg
assemblyLink assembly =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Assembly assembly.assembly_id) ]
            [ text assembly.assembly_name ]
        ]


viewAssemblies : Model -> Html Msg
viewAssemblies model =
    let
        lowerQuery =
            String.toLower model.assemblyQuery

        acceptableResults =
            List.filter (\item -> String.contains lowerQuery (String.toLower item.assembly_name)) model.assemblies

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    case acceptableResults of
                        [] ->
                            case model.assemblyQuery of
                                 "" ->
                                    model.project.assembly_count

                                 _ ->
                                    0

                        _ ->
                            List.length acceptableResults

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        searchBar =
            case model.assemblies of
                [] ->
                    text ""

                _ ->
                    small [ class "right" ]
                        [ input [ placeholder "Search", onInput SetAssemblyQuery ] [] ]

        body =
            case model.project.assembly_count of
                0 ->
                    text "None"

                _ ->
                    case model.loadedAssemblies of
                        True ->
                            case acceptableResults of
                                [] ->
                                    text "None"

                                _ ->
                                    Table.view assemblyTableConfig model.assemblyTableState acceptableResults

                        False ->
                            case model.loadingAssemblies of
                                True ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ spinner ] ] ] ]

                                False ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetAssemblies ] [ text "Show Results" ] ] ] ] ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Assemblies "
                , numShowing
                , searchBar
                ]
            , div [ class "scrollable" ] [ body ]
            ]
        ]


combinedAssemblyTableConfig : Table.Config CombinedAssembly Msg
combinedAssemblyTableConfig =
    Table.customConfig
        { toId = toString << .combined_assembly_id
        , toMsg = SetCombinedAssemblyTableState
        , columns =
            [ combinedAssemblyNameColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


combinedAssemblyNameColumn : Table.Column CombinedAssembly Msg
combinedAssemblyNameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = combinedAssemblyLink
        , sorter = Table.increasingOrDecreasingBy .assembly_name
        }


combinedAssemblyLink : CombinedAssembly -> Table.HtmlDetails Msg
combinedAssemblyLink combined_assembly =
    Table.HtmlDetails []
        [ a [ Route.href (Route.CombinedAssembly combined_assembly.combined_assembly_id) ]
            [ text combined_assembly.assembly_name ]
        ]


viewCombinedAssemblies : Model -> Html Msg
viewCombinedAssemblies model =
    let
        lowerQuery =
            String.toLower model.combinedAssemblyQuery

        acceptableResults =
            List.filter (\item -> String.contains lowerQuery (String.toLower item.assembly_name)) model.combined_assemblies

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    case acceptableResults of
                        [] ->
                            case model.combinedAssemblyQuery of
                                 "" ->
                                    model.project.combined_assembly_count

                                 _ ->
                                    0

                        _ ->
                            List.length acceptableResults

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        searchBar =
            case model.combined_assemblies of
                [] ->
                    text ""

                _ ->
                    small [ class "right" ]
                        [ input [ placeholder "Search", onInput SetCombinedAssemblyQuery ] [] ]

        body =
            case model.project.combined_assembly_count of
                0 ->
                    text "None"

                _ ->
                    case model.loadedCombinedAssemblies of
                        True ->
                            case acceptableResults of
                                [] ->
                                    text "None"

                                _ ->
                                    Table.view combinedAssemblyTableConfig model.combinedAssemblyTableState acceptableResults

                        False ->
                            case model.loadingCombinedAssemblies of
                                True ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ spinner ] ] ] ]

                                False ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetCombinedAssemblies ] [ text "Show Results" ] ] ] ] ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Combined Assemblies "
                , numShowing
                , searchBar
                ]
            , div [ class "scrollable" ] [ body ]
            ]
        ]


shareDialogConfig : Int -> Model -> Dialog.Config Msg
shareDialogConfig currentUserId model =
    let
        content =
            div []
                [ div []
                    [ br [] []
                    , if model.showShareDialogBusy then
                        spinner
                      else
                        div [ class "scrollable" ]
                            [ div [] [ text "Who has access" ]
                            , (viewUsersAndGroups currentUserId model.isEditable model.project.users model.project.project_groups)
                            , br [] [] -- kludge for dropdown not on top
                            , br [] []
                            , br [] []
                            , br [] []
                            ]
                    ]
                , if model.isEditable then
                    addPanel
                  else
                    text ""
                ]

        addPanel =
            div []
                [ hr [] []
                , div [ class "form-group" ]
                    [ div [] [ text "Add a person or group:" ]
                    , div []
                        [ View.SearchableDropdown.view shareDropdownConfig model.shareDropdownState
                        ]
                    ]
                , br [] []
                ]
    in
    { closeMessage = Just CloseShareDialog
    , containerClass = Just "narrow-modal-container"
    , header = Just (h3 [] [ text "Share Project" ])
    , body = Just content
    , footer = Nothing
    }


shareDropdownConfig : View.SearchableDropdown.Config Msg Msg
shareDropdownConfig =
    { placeholder = "Enter the name of the person or group to add "
    , autofocus = False
    , inputMsg = SetShareUserName
    , selectMsg = (ShareWithUser "read-only")
    }


viewUsersAndGroups : Int -> Bool -> List User -> List ProjectGroup -> Html Msg
viewUsersAndGroups currentUserId isEditable users groups =
    if users == [] && groups == [] then
        div [] [ text "Only you can see this project." ]
    else
        table [ class "table" ]
            [ tbody []
                ((List.map (\u -> viewUser (u.user_id == currentUserId) isEditable u) users) ++
                    (List.map (\g -> viewGroup isEditable g) groups))
            ]


viewUser : Bool -> Bool -> User -> Html Msg
viewUser isMe isEditable user =
    let
        displayName =
            user.first_name ++ " " ++ user.last_name
    in
    tr []
        [ td []
            [ i [ class "fas fa-user" ] []
            , text " "
            , text displayName
            , if isMe then
                text " (you)"
              else
                text ""
            , if user.permission /= "owner" && isEditable then
                viewPermissionDropdown user
              else
                span [ class "pull-right" ] [ text (capitalize user.permission) ]
            ]
        ]


viewGroup : Bool -> ProjectGroup -> Html Msg
viewGroup isEditable group =
    tr []
        [ td []
            [ i [ class "fas fa-user-friends" ] []
            , text " "
            , a [ Route.href (Route.ProjectGroup group.project_group_id), target "_blank" ] [ text group.group_name ]
            , text " ("
            , text (toString group.user_count)
            , text " "
            , text (Util.pluralize "user" group.user_count)
            , text ")"
            , if isEditable then
                button [ class "btn btn-default btn-xs pull-right", onClick (RemoveFromProjectGroup group.project_group_id) ] [ text "Remove" ]
              else
                text ""
            ]
        ]


viewPermissionDropdown : User -> Html Msg
viewPermissionDropdown user =
    div [ class "pull-right dropdown" ]
        [ button [ class "btn btn-default btn-xs dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (capitalize user.permission), text " ", span [ class "caret" ] [] ]
        , ul [ class "dropdown-menu nowrap" ]
            [ li [] [ a [ onClick (ShareWithUser "read-only" user.user_id user.user_name) ] [ text "Read-only: can view but not modify" ] ]
            , li [] [ a [ onClick (ShareWithUser "read-write" user.user_id user.user_name) ] [ text "Read-write: can view/edit but not delete" ] ]
            , li [] [ a [ onClick (UnshareWithUser user.user_id) ] [ text "Remove access" ] ]
            ]
        ]


editInfoDialogConfig : Model -> Dialog.Config Msg
editInfoDialogConfig model =
    let
        content =
            Html.form []
                [ div [ class "form-group" ]
                    [ label [] [ text "Name" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the name (required)", value model.newProjectName, onInput SetNewProjectName ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Accession" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the accession (required)", value model.newProjectCode, onInput SetNewProjectCode ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Type" ]
                    , div [ class "input-group" ]
                        [ input [ class "form-control", type_ "text", value model.newProjectType ] []
                        , div [ class "input-group-btn" ]
                            [ div [ class "dropdown" ]
                                [ button [ class "btn btn-default dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Select ", span [ class "caret" ] [] ]
                                , ul [ class "dropdown-menu dropdown-menu-right" ]
                                    (List.map (\s -> li [ onClick (SetNewProjectType s) ] [ a [] [ text s ]]) model.project.available_types)
                                ]
                            ]
                        ]
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Domains" ]
                    , View.TagsDropdown.view (domainDropdownConfig (List.map (\d -> (d.domain_id, d.domain_name)) model.project.available_domains)) model.domainDropdownState
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Investigators" ]
                    , div []
                        [ View.Tags.view (View.Tags.Config RemoveInvestigator) model.newInvestigatorTagState
                        , View.SearchableDropdown.view investigatorDropdownConfig model.investigatorDropdownState
                        ]
                    ]
--                , div [ class "form-group" ]
--                    [ label [] [ text "Groups" ]
--                    , View.TagsDropdown.view (groupDropdownConfig (List.map (\g -> (g.project_group_id, g.group_name)) model.project.available_groups)) model.groupDropdownState
--                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "URL" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the URL (optional)", value model.newProjectURL, onInput SetNewProjectURL ] []
                    ]
                ]

        footer =
            div []
                [ button [ class "btn btn-default pull-left", onClick CloseEditInfoDialog ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick UpdateProjectInfo ] [ text "Update" ]
                ]
    in
    { closeMessage = Just CloseEditInfoDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Modify Project Info" ])
    , body = Just content
    , footer = Just footer
    }


domainDropdownConfig : List (Int, String) -> View.TagsDropdown.Config Msg Msg
domainDropdownConfig domains =
    { options = domains
    , addMsg = AddNewProjectDomain
    , removeMsg = RemoveNewProjectDomain
    }


investigatorDropdownConfig : View.SearchableDropdown.Config Msg Msg
investigatorDropdownConfig =
    { placeholder = "Enter the name of the investigator to add "
    , autofocus = False
    , inputMsg = SetInvestigatorName
    , selectMsg = SelectInvestigatorToAdd
    }


--groupDropdownConfig : List (Int, String) -> View.TagsDropdown.Config Msg Msg
--groupDropdownConfig groups =
--    { options = groups
--    , addMsg = AddNewProjectGroup
--    , removeMsg = RemoveNewProjectGroup
--    }


newSampleDialogConfig : Model -> Dialog.Config Msg
newSampleDialogConfig model =
    let
        content =
            if model.showNewSampleBusy then
                spinner
            else
                input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the name of the new sample", onInput SetNewSampleName ] []

        footer =
            let
                disable =
                    disabled model.showNewSampleBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseNewSampleDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick CreateNewSample, disable ] [ text "OK" ]
                    ]
    in
    { closeMessage = Just CloseNewSampleDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "New Sample" ])
    , body = Just content
    , footer = Just footer
    }


addOrEditPublicationDialogConfig : Model -> Dialog.Config Msg
addOrEditPublicationDialogConfig model =
    let
        content =
            if model.showAddOrEditPublicationBusy then
                spinner
            else
                Html.form []
                    [ div [ class "form-group" ]
                        [ label [] [ text "Title" ]
                        , input [ class "form-control", type_ "text", size 20, value model.newPublicationTitle, placeholder "Enter the title (required)", onInput SetPublicationTitle ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Authors" ]
                        , input [ class "form-control", type_ "text", size 20, value model.newPublicationAuthors, placeholder "Enter the author names separated by commas (required)", onInput SetPublicationAuthors ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Publication Date" ]
                        , input [ class "form-control", type_ "text", size 20, value model.newPublicationDate, placeholder "Enter the publication date as MM/DD/YYYY (required)", onInput SetPublicationDate ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "PubMed ID" ]
                        , input [ class "form-control", type_ "text", size 20, value model.newPublicationPubMedID, placeholder "Enter the PubMed ID or leave blank (optional)", onInput SetPublicationPubMedID ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "DOI" ]
                        , input [ class "form-control", type_ "text", size 20, value model.newPublicationDOI, placeholder "Enter the DOI or leave blank (optional)", onInput SetPublicationDOI ] []
                        ]
                    ]

        footer =
            div [ disabled model.showAddOrEditPublicationBusy ]
                [ button [ class "btn btn-default pull-left", onClick CloseAddOrEditPublicationDialog ] [ text "Cancel" ]
                , case model.publicationIdToEdit of
                    Nothing ->
                        button [ class "btn btn-primary", onClick AddPublication ] [ text "Add" ]

                    Just id ->
                        button [ class "btn btn-primary", onClick (UpdatePublication id) ] [ text "Update" ]
                ]
    in
    { closeMessage = Just CloseAddOrEditPublicationDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add Publication" ])
    , body = Just content
    , footer = Just footer
    }
