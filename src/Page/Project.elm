module Page.Project exposing (Model, Msg(..), PublishMsg(..), ExternalMsg(..), init, update, view)

import Data.Project exposing (Project, ProjectFile, Investigator, Domain, Assembly, CombinedAssembly, Sample, Publication, ProjectGroup, User)
import Data.ProjectGroup
import Data.Sample
import Data.Investigator
import Data.User
import Data.Session exposing (Session, isLoggedIn)
import Data.Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Json.Encode
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Project
import Request.ProjectGroup
import Request.Sample
import Request.Investigator
import Request.Publication
import Request.User
import Route
import String.Extra
import Task exposing (Task)
import Table exposing (defaultCustomizations)
import View.Cart as Cart
import View.Spinner exposing (spinner)
import View.Dialog exposing (confirmationDialogConfig, errorDialogConfig)
import View.SearchableDropdown
import View.TagsDropdown
import View.Tags
import View.Investigator
import View.Widgets
import Util exposing ((=>), capitalize, pluralize, isUrl)
import Config exposing (dataCommonsUrl)



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
    , isBetaUser : Bool
    , currentUserId : Maybe Int
    , confirmationDialog : Maybe (Dialog.Config Msg)
    , showShareDialog : Bool
    , showShareDialogBusy : Bool
    , shareDialogError : String
    , shareDropdownState : View.SearchableDropdown.State
    , showPublishDialog : Bool
    , showPublishDialogBusy : Bool
    , publishDialogError : String
    , validForPublish : Bool
    , showEditInfoDialog : Bool
    , projectName : String
    , projectDescription : String
    , projectCode : String
    , projectType : String
    , projectInstitution : String
    , projectURL : String
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
    , publicationTitle : String
    , publicationAuthors : String
    , publicationDate : String
    , publicationPubMedID : String
    , publicationDOI : String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        loadProject =
            Request.Project.get session.token id |> Http.toTask

        userId =
            Maybe.map .user_id session.user

        allUsers project =
            project.users ++ (List.map .users project.project_groups |> List.concat)

        isEditable project =
            project.private == 1 &&
            project.ebi_status == Nothing &&
            (case userId of
                Nothing ->
                    False

                Just userId ->
                    allUsers project
                        |> List.any (\u -> u.user_id == userId && (u.permconn.permission == "owner" || u.permconn.permission == "read-write"))
            )
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
                    , isBetaUser = Data.User.isBetaUser session.user
                    , currentUserId = userId
                    , confirmationDialog = Nothing
                    , showShareDialog = False
                    , showShareDialogBusy = False
                    , shareDialogError = ""
                    , shareDropdownState = View.SearchableDropdown.init
                    , showPublishDialog = False
                    , showPublishDialogBusy = False
                    , publishDialogError = ""
                    , validForPublish = False
                    , showEditInfoDialog = False
                    , projectName = ""
                    , projectDescription = ""
                    , projectCode = ""
                    , projectType = ""
                    , projectInstitution = ""
                    , projectURL = ""
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
                    , publicationTitle = ""
                    , publicationAuthors = ""
                    , publicationDate = ""
                    , publicationPubMedID = ""
                    , publicationDOI = ""
                    }
            )
        |> Task.mapError (Error.handleLoadErrorWithLogin (isLoggedIn session))



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | InfoMsg InfoMsg
    | PublishMsg PublishMsg
    | ShareMsg ShareMsg
    | PublicationMsg PublicationMsg
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
    | OpenNewSampleDialog
    | CloseNewSampleDialog
    | SetNewSampleName String
    | CreateNewSample
    | CreateNewSampleCompleted (Result Http.Error Data.Sample.Sample)
    | RemoveSample Int
    | RemoveSampleCompleted (Result Http.Error Project)


type InfoMsg
    = OpenEditInfoDialog
    | CloseEditInfoDialog
    | SetProjectName String
    | SetProjectDescription String
    | SetProjectCode String
    | SetProjectType String
    | SetProjectInstitution String
    | SetProjectURL String
    | AddProjectDomain Int String
    | RemoveProjectDomain Int
    | SetInvestigatorName String
    | SearchInvestigatorCompleted (Result Http.Error (List Data.Investigator.Investigator))
    | SelectInvestigatorToAdd String String
    | RemoveInvestigator Int
    | UpdateProjectInfo
    | UpdateProjectInfoCompleted (Result Http.Error Project)


type PublishMsg
    = OpenPublishDialog
    | ClosePublishDialog
    | ValidateProjectCompleted (Result Http.Error String)
    | PublishProject
    | PublishProjectCompleted (Result Http.Error String)
    | RefreshStatus
    | RefreshStatusCompleted (Result Http.Error Project)


type ShareMsg
    = OpenShareDialog
    | CloseShareDialog
    | CloseShareDialogError
    | SetShareUserName String
    | SearchUsersAndGroupsCompleted (Result Http.Error (List Data.User.User, List Data.ProjectGroup.ProjectGroup))
    | ShareWithUser String String String --FIXME create new type for permission instead of using string
    | ShareWithUserCompleted (Result Http.Error Project)
    | AddToProjectGroupCompleted (Result Http.Error Data.ProjectGroup.ProjectGroup)
    | RemoveFromProjectGroup Int
    | RemoveFromProjectGroupCompleted (Result Http.Error Data.ProjectGroup.ProjectGroup)
    | UnshareWithUser Int
    | UnshareWithUserCompleted (Result Http.Error String)


type PublicationMsg
    = OpenPublicationDialog (Maybe Publication)
    | ClosePublicationDialog
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

        InfoMsg subMsg ->
            let
                (newModel, newCmd) =
                    updateInfo session subMsg model
            in
            newModel => Cmd.map InfoMsg newCmd => NoOp

        PublishMsg subMsg ->
            let
                (newModel, newCmd) =
                    updatePublish session subMsg model
            in
            newModel => Cmd.map PublishMsg newCmd => NoOp

        ShareMsg subMsg ->
            let
                (newModel, newCmd) =
                    updateShare session subMsg model
            in
            newModel => Cmd.map ShareMsg newCmd => NoOp

        PublicationMsg subMsg ->
            let
                (newModel, newCmd) =
                    updatePublication session subMsg model
            in
            newModel => Cmd.map PublicationMsg newCmd => NoOp

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
                _ = Debug.log "RemoveSampleCompleted" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp


updateInfo : Session -> InfoMsg -> Model -> ( Model, Cmd InfoMsg )
updateInfo session msg model =
    case msg of
        OpenEditInfoDialog ->
            { model
                | showEditInfoDialog = True
                , projectName = model.project.project_name
                , projectDescription = model.project.description
                , projectCode = model.project.project_code
                , projectType = model.project.project_type
                , projectInstitution = model.project.institution
                , projectURL = model.project.url
             } => Cmd.none

        CloseEditInfoDialog ->
            { model | showEditInfoDialog = False } => Cmd.none

        SetProjectName name ->
            { model | projectName = name } => Cmd.none

        SetProjectDescription desc ->
            { model | projectDescription = desc } => Cmd.none

        SetProjectCode code ->
            { model | projectCode = code } => Cmd.none

        SetProjectType type_ ->
            { model | projectType = type_ } => Cmd.none

        SetProjectInstitution institution ->
            { model | projectInstitution = institution } => Cmd.none

        SetProjectURL url ->
            { model | projectURL = url } => Cmd.none

        AddProjectDomain id name ->
            let
                newDropdownState =
                    View.TagsDropdown.add id name model.domainDropdownState
            in
            { model | domainDropdownState = newDropdownState } => Cmd.none

        RemoveProjectDomain id ->
            let
                newDropdownState =
                    View.TagsDropdown.remove id model.domainDropdownState
            in
            { model | domainDropdownState = newDropdownState } => Cmd.none

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
            { model | investigatorDropdownState = { dropdownState | value = name } } => cmd

        SearchInvestigatorCompleted (Ok investigators) ->
            let
                results =
                    List.map (\i -> (toString i.investigator_id, i.investigator_name)) investigators

                dropdownState =
                    model.investigatorDropdownState
            in
            { model | investigatorDropdownState = { dropdownState | results = results } } => Cmd.none

        SearchInvestigatorCompleted (Err error) -> -- TODO finish this
            model => Cmd.none

        SelectInvestigatorToAdd id name ->
            let
                dropdownState =
                    model.investigatorDropdownState

                tagState =
                    View.Tags.add (String.toInt id |> Result.withDefault 0) name model.newInvestigatorTagState
            in
            { model
                | investigatorDropdownState = { dropdownState | value = "", results = [], selectedId = Just id }
                , newInvestigatorTagState = tagState
            } => Cmd.none

        RemoveInvestigator id ->
            let
                tagState =
                    View.Tags.remove id model.newInvestigatorTagState
            in
            { model | newInvestigatorTagState = tagState } => Cmd.none

        UpdateProjectInfo ->
            let
                domains =
                    View.TagsDropdown.selected model.domainDropdownState |> List.map (\t -> Domain (Tuple.first t) (Tuple.second t))

                investigators =
                    View.Tags.selected model.newInvestigatorTagState |> List.map (\t -> Investigator (Tuple.first t) (Tuple.second t) "")

                updateInfo =
                    Request.Project.update session.token model.project_id model.projectName model.projectDescription model.projectCode model.projectType model.projectInstitution model.projectURL domains investigators |> Http.toTask
            in
            { model | showEditInfoDialog = False } => Task.attempt UpdateProjectInfoCompleted updateInfo

        UpdateProjectInfoCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model
                | project =
                    { newProject
                        | project_name = project.project_name
                        , description = project.description
                        , project_code = project.project_code
                        , project_type = project.project_type
                        , institution = project.institution
                        , url = project.url
                        , domains = project.domains
                        , investigators = project.investigators
                        , project_groups = project.project_groups
                    }
            } => Cmd.none

        UpdateProjectInfoCompleted (Err error) ->
            let
                _ = Debug.log "UpdateProjectInfoCompleted" (toString error) -- TODO show to user
            in
            model => Cmd.none


updatePublish : Session -> PublishMsg -> Model -> ( Model, Cmd PublishMsg )
updatePublish session msg model =
    let
        getStatus =
            Request.Project.get session.token model.project_id |> Http.toTask

        submissionInProgress =
            case model.project.ebi_status of
                Nothing ->
                    False

                Just status ->
                    status /= "FINISHED" && status /= "FAILED" && status /= "STOPPED"

        errorMsg error =
            case error of
                Http.BadStatus response ->
                    case String.length response.body of
                        0 ->
                            "Bad status - please contact support"

                        _ ->
                            response.body
                _ ->
                    toString error
    in
    case msg of
        OpenPublishDialog ->
            case model.project.ebi_status of
                Nothing ->
                    let
                        validateProject =
                            Request.Project.publish session.token model.project_id True |> Http.toTask
                    in
                    { model | showPublishDialog = True, showPublishDialogBusy = True, publishDialogError = "" } => Task.attempt ValidateProjectCompleted validateProject

                Just status ->
                    if status == "FINISHED" || status == "FAILED" then
                        { model | showPublishDialog = True, showPublishDialogBusy = False, publishDialogError = "" } => Cmd.none
                    else
                        { model | showPublishDialog = True, showPublishDialogBusy = True, publishDialogError = "" } => Task.attempt RefreshStatusCompleted getStatus

        ClosePublishDialog ->
            { model | showPublishDialog = False } => Cmd.none

        ValidateProjectCompleted (Ok status) ->
            { model | showPublishDialogBusy = False, validForPublish = True } => Cmd.none

        ValidateProjectCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "ValidateProjectCompleted" (toString error)
            in
            { model | showPublishDialogBusy = False, validForPublish = False, publishDialogError = errorMsg error } => Cmd.none

        PublishProject ->
            let
                publishProject =
                    Request.Project.publish session.token model.project_id False |> Http.toTask
            in
            { model | showPublishDialog = True, showPublishDialogBusy = True, publishDialogError = "", isEditable = False } => Task.attempt PublishProjectCompleted publishProject

        PublishProjectCompleted (Ok status) ->
            let
                newProject =
                    model.project
            in
            { model | showPublishDialogBusy = False, project = { newProject | ebi_status = Just status } } => Cmd.none

        PublishProjectCompleted (Err error) ->
            { model | showPublishDialogBusy = False, publishDialogError = errorMsg error } => Cmd.none

        RefreshStatus ->
            if submissionInProgress then
                model => Task.attempt RefreshStatusCompleted getStatus
            else
                model => Cmd.none

        RefreshStatusCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showPublishDialogBusy = False, project = { newProject | ebi_status = project.ebi_status, ebi_accn = project.ebi_accn } } => Cmd.none

        RefreshStatusCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "RefreshStatusCompleted" (toString error)
            in
            model => Cmd.none


updateShare : Session -> ShareMsg -> Model -> ( Model, Cmd ShareMsg )
updateShare session msg model =
    case msg of
        OpenShareDialog ->
            { model | showShareDialog = True } => Cmd.none

        CloseShareDialog ->
            { model | showShareDialog = False } => Cmd.none

        CloseShareDialogError ->
            { model | shareDialogError = "" } => Cmd.none

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
                            (Request.ProjectGroup.searchByName session.token name |> Http.toTask)
                in
                { model | shareDropdownState = { dropdownState | value = name } } => Task.attempt SearchUsersAndGroupsCompleted searchByName
            else
                { model | shareDropdownState = { dropdownState | value = name, results = [] } } => Cmd.none

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
                        (List.map (\u -> (toString u.user_id, userDisplayName u)) users)
                        (List.map (\g -> (toString g.project_group_id, groupDisplayName g)) groups)

                dropdownState =
                    model.shareDropdownState
            in
            { model | shareDropdownState = { dropdownState | results = results } } => Cmd.none

        SearchUsersAndGroupsCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "SearchUsersAndGroupsCompleted" (toString error)
            in
            model => Cmd.none

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
                        List.any (\g -> toString g.project_group_id == id) model.project.project_groups

                    addProjectToProjectGroup =
                        Request.ProjectGroup.addProject session.token (String.toInt id |> Result.withDefault 0) model.project_id True |> Http.toTask
                in
                if noChange then
                    model => Cmd.none
                else
                    newModel => Task.attempt AddToProjectGroupCompleted addProjectToProjectGroup
            else
                let
                    noChange =
                        List.any (\u -> toString u.user_id == id && u.permconn.permission == permission) model.project.users

                    isOwner =
                        List.any (\u -> toString u.user_id == id && u.permconn.permission == "owner") model.project.users

                    addUserToProject =
                        Request.Project.addUserToProject session.token model.project_id (String.toInt id |> Result.withDefault 0) permission |> Http.toTask
                in
                if noChange || isOwner then
                    model => Cmd.none
                else
                    newModel => Task.attempt ShareWithUserCompleted addUserToProject

        ShareWithUserCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showShareDialogBusy = False, project = { newProject | users = project.users } } => Cmd.none

        ShareWithUserCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "ShareWithUserCompleted" (toString error)
            in
            model => Cmd.none

        AddToProjectGroupCompleted (Ok group) ->
            let
                newProject =
                    model.project

                newGroups =
                    (ProjectGroup group.project_group_id group.group_name group.users) :: model.project.project_groups -- FIXME kludge due to multiple type defs
            in
            { model | showShareDialogBusy = False, project = { newProject | project_groups = newGroups } } => Cmd.none

        AddToProjectGroupCompleted (Err error) ->
            let
                error =
                    "You don't have permission to share with that group."
            in
            { model | showShareDialogBusy = False, shareDialogError = error } => Cmd.none

        RemoveFromProjectGroup groupId ->
            let
                removeFromProjectGroup =
                    Request.ProjectGroup.removeProject session.token groupId model.project_id |> Http.toTask

                newProject =
                    model.project

                newGroups =
                    List.filter (\g -> g.project_group_id /= groupId) model.project.project_groups
            in
            { model | project = { newProject | project_groups = newGroups } } => Task.attempt RemoveFromProjectGroupCompleted removeFromProjectGroup

        RemoveFromProjectGroupCompleted (Ok _) ->
            model => Cmd.none

        RemoveFromProjectGroupCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "RemoveFromProjectGroupCompleted" (toString error)
            in
            model => Cmd.none

        UnshareWithUser id ->
            let
                removeUser =
                    Request.Project.removeUserFromProject session.token model.project_id id |> Http.toTask

                newProject =
                    model.project

                newUsers =
                    List.filter (\u -> u.user_id /= id) model.project.users
            in
            { model | project = { newProject | users = newUsers } } => Task.attempt UnshareWithUserCompleted removeUser

        UnshareWithUserCompleted (Ok _) ->
            model => Cmd.none

        UnshareWithUserCompleted (Err error) -> -- TODO finish this
            let
                _ = Debug.log "UnshareWithUserCompleted" (toString error)
            in
            model => Cmd.none


updatePublication : Session -> PublicationMsg -> Model -> ( Model, Cmd PublicationMsg )
updatePublication session msg model =
    let
        loadProject _ =
            Request.Project.get session.token model.project_id |> Http.toTask
    in
    case msg of
        OpenPublicationDialog publication ->
            { model
                | showAddOrEditPublicationDialog = True
                , showAddOrEditPublicationBusy = False
                , publicationIdToEdit = Maybe.map .publication_id publication
                , publicationTitle = Maybe.withDefault "" (Maybe.map .title publication)
                , publicationAuthors = Maybe.withDefault "" (Maybe.map .author publication)
                , publicationDate = Maybe.withDefault "" (Maybe.map .pub_date publication)
                , publicationPubMedID = Maybe.withDefault "" (Maybe.map (.pubmed_id >> toString) publication)
                , publicationDOI = Maybe.withDefault "" (Maybe.map .doi publication)
            } => Cmd.none

        ClosePublicationDialog ->
            { model | showAddOrEditPublicationDialog = False } => Cmd.none

        SetPublicationTitle title ->
            { model | publicationTitle = title } => Cmd.none

        SetPublicationAuthors authors ->
            { model | publicationAuthors = authors } => Cmd.none

        SetPublicationDate date ->
            { model | publicationDate = date } => Cmd.none

        SetPublicationPubMedID id ->
            { model | publicationPubMedID = id } => Cmd.none

        SetPublicationDOI doi ->
            { model | publicationDOI = doi } => Cmd.none

        AddPublication ->
            let
                pubmedId =
                    String.toInt model.publicationPubMedID |> Result.toMaybe

                createPublication =
                    Request.Publication.create session.token model.project_id model.publicationTitle model.publicationAuthors model.publicationDate pubmedId model.publicationDOI |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | showAddOrEditPublicationBusy = True } => Task.attempt AddPublicationCompleted createPublication

        AddPublicationCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showAddOrEditPublicationDialog = False, project = { newProject | publications = project.publications } } => Cmd.none

        AddPublicationCompleted (Err error) ->
            let
                _ = Debug.log "AddPublicationCompleted" (toString error) -- TODO show to user
            in
            { model | showAddOrEditPublicationDialog = False } => Cmd.none

        UpdatePublication pub_id ->
            let
                pubmedId =
                    String.toInt model.publicationPubMedID |> Result.toMaybe

                updatePublication =
                    Request.Publication.update session.token pub_id model.publicationTitle model.publicationAuthors model.publicationDate pubmedId model.publicationDOI |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | confirmationDialog = Nothing, showAddOrEditPublicationBusy = True } => Task.attempt UpdatePublicationCompleted updatePublication

        UpdatePublicationCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showAddOrEditPublicationDialog = False, project = { newProject | publications = project.publications } } => Cmd.none

        UpdatePublicationCompleted (Err error) ->
            let
                _ = Debug.log "UpdatePublicationCompleted" (toString error) -- TODO show to user
            in
            model => Cmd.none

        RemovePublication pub_id ->
            let
                removePublication =
                    Request.Publication.remove session.token pub_id |> Http.toTask
                        |> Task.andThen loadProject
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemovePublicationCompleted removePublication

        RemovePublicationCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | project = { newProject | publications = project.publications } } => Cmd.none

        RemovePublicationCompleted (Err error) ->
            let
                _ = Debug.log "RemovePublicationCompleted" (toString error) -- TODO show to user
            in
            model => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small [] [ text model.project.project_name ]
                    , if model.isEditable && model.isBetaUser then
                        viewPublishButton model.project
                      else
                        text ""
                    , viewShareButton model.project
                    ]
                ]
            , viewProject model.project model.isEditable
            , br [] []
            , viewFiles model.project.project_files
            , viewPublications model.project.publications model.isEditable
            , br [] []
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
              else if model.showPublishDialog then
                case model.currentUserId of
                    Nothing ->
                        Nothing

                    Just id ->
                        Just (publishDialogConfig id model)
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


viewShareButton : Project -> Html Msg
viewShareButton project =
    if project.private == 1 then
        let
            buttonLabel =
                if List.length project.users <= 1 && List.length project.project_groups == 0 then
                    "Project is Private"
                else
                    "Project is Shared"

            numGroups =
                List.length project.project_groups

            numUsers =
                List.length project.users - 1

            groupsStr =
                List.map .group_name project.project_groups |> String.join ", "

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
        button [ class "btn btn-default pull-right", onClick (ShareMsg OpenShareDialog) ]
            [ div []
                [ span [ class "glyphicon glyphicon-lock" ] []
                , text " "
                , text buttonLabel
                ]
            , div [ class "small-text pull-right" ]
                [ text shareStr ]
            ]
    else if project.project_groups /= [] then
        let
            mkLabel group =
                a [ class "label label-primary tiny-text", title (group.group_name ++ " Group"), Route.href (Route.ProjectGroup group.project_group_id) ] [ text group.group_name ]
        in
        span [ class "pull-right" ] (List.map mkLabel project.project_groups |> List.intersperse (text " "))
    else
        text ""


viewPublishButton : Project -> Html Msg
viewPublishButton project =
    let
        viewBtn btnClass inner =
            button [ class ("pull-right margin-left btn " ++ btnClass), onClick OpenPublishDialog ] inner |> Html.map PublishMsg
    in
    case project.ebi_status of
        Nothing ->
            viewBtn "btn-default"
                [ text "Publish Project"
                , br [] []
                , span [ class "label label-info pull-right" ] [ text "BETA" ]
                ]

        Just "FINISHED" ->
            viewBtn "btn-default" [ text "Source: EBI" ]

        Just "FAILED" ->
            viewBtn "btn-danger"  [ text "Publication Failed" ]

        _ ->
            if project.private == 1 then
                viewBtn "btn-default" [ text "Publication in Progress ..." ]
            else
                text ""


viewProject : Project -> Bool -> Html Msg
viewProject project isEditable =
    let
        domainText =
            pluralize "Domain" (List.length project.domains)

        editButton =
            if isEditable then
                button [ class "btn btn-default btn-xs", onClick (InfoMsg OpenEditInfoDialog) ] [ span [ class "glyphicon glyphicon-cog" ] [], text " Edit" ]
            else
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
            [ th [] [ text "Description" ]
            , td [] [ div [ style [("overflow-y", "auto"), ("max-height", "6.75em")] ] [ text project.description ] ]
            ]
        , tr []
            [ th [] [ text "Accession" ]
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
            [ th [] [ text "Institution" ]
            , td [] [ text project.institution ]
            ]
        , tr []
            [ th [] [ text "Investigators" ]
            , td [] (View.Investigator.viewList project.investigators)
            ]
--        , tr []
--            [ th [] [ text "Groups" ]
--            , td [] (viewProjectGroups project.project_groups)
--            ]
        , tr []
            [ th [] [ text "URL" ]
            , td [] [ a [ href project.url, target "_blank" ] [ text project.url ] ]
            ]
        , tr []
            [ td [] [ editButton ]
            ]
        ]


viewDomains : List Domain -> List (Html msg)
viewDomains domains =
    if List.length domains == 0 then
        [ text "None" ]
    else
        List.sortBy .domain_name domains |> List.map viewDomain |> List.intersperse (text ", ")


viewDomain : Domain -> Html msg
viewDomain domain =
    text domain.domain_name


viewProjectGroups : List ProjectGroup -> List (Html msg)
viewProjectGroups groups =
    if List.length groups == 0 then
        [ text "None" ]
    else
        List.sortBy .group_name groups |> List.map viewProjectGroup |> List.intersperse (text ", ")


viewProjectGroup : ProjectGroup -> Html msg
viewProjectGroup group =
    a [ Route.href (Route.ProjectGroup group.project_group_id) ]
        [ text group.group_name ]


viewFiles : List ProjectFile -> Html Msg
viewFiles files =
    let
        numFiles =
            List.length files

        label =
            if numFiles == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text (toString numFiles)
                    ]

        cols =
            tr []
                [ th [] [ text "Path" ]
                , th [] [ text "Type" ]
                , th [] [ text "Description" ]
                ]
    in
    div []
        [ h2 []
            [ text "Files "
            , label
            ]
        , div [ class "scrollable" ]
            [ if numFiles == 0 then
                text "None"
              else
                table [ class "table table-condensed" ]
                    [ tbody [] (cols :: (List.sortBy .file files |> List.map viewFile)) ]
            ]
        ]


viewFile : ProjectFile -> Html Msg
viewFile file =
    let
        link =
            if isUrl file.file then
                file.file
            else
                dataCommonsUrl ++ file.file
    in
    tr []
        [ td []
            [ a [ href link, target "_blank" ] [ text file.file ] ]
        , td []
            [ text file.project_file_type.type_ ]
        , td []
            [ text file.description ]
        ]


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
            if isEditable then
                button [ class "btn btn-default btn-sm pull-right", onClick (OpenPublicationDialog Nothing |> PublicationMsg) ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Publication" ]
            else
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
                [ button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Are you sure you want to remove this publication?" (RemovePublication pub.publication_id |> PublicationMsg)) ] [ text "Remove" ]
                , button [ class "btn btn-default btn-xs pull-right margin-right", onClick (OpenPublicationDialog (Just pub) |> PublicationMsg) ] [ text "Edit" ]
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
            if numSamples == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text (toString numSamples)
                    ]

        cartTh =
            th [ class "nowrap" ]
                (text "Cart"
                    ::
                    (if numSamples > 1 then
                        [ br [] []
                        , Cart.addAllToCartButton cart Nothing (List.map .sample_id samples) |> Html.map CartMsg
                        ]
                    else
                        []
                    )
                )

        cols =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                , th [] [ text "Investigators" ]
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
            if isEditable then
                button [ class "btn btn-default btn-sm pull-right", onClick OpenNewSampleDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Sample" ]
            else
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
            if isEditable then
                button [ class "btn btn-default btn-xs", onClick (OpenConfirmationDialog "Remove the sample (this cannot be undone)?" (RemoveSample sample.sample_id)) ] [ span [ class "glyphicon glyphicon-trash" ] [] ]
            else
                text ""
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        , td [] (View.Investigator.viewList sample.investigators)
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

        count =
            if acceptableResults == [] then
                if model.assemblyQuery == "" then
                    model.project.assembly_count
                else
                    0
            else
                List.length acceptableResults

        searchBar =
            if model.assemblies == [] then
                text ""
            else
                small [ class "right" ]
                    [ input [ placeholder "Search", onInput SetAssemblyQuery ] [] ]

        body =
            if model.project.assembly_count == 0 then
                text "None"
            else
                if model.loadedAssemblies then
                    if acceptableResults == [] then
                        text "None"
                    else
                        Table.view assemblyTableConfig model.assemblyTableState acceptableResults
                else
                    if model.loadingAssemblies then
                        table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td []
                                        [ spinner ]
                                    ]
                                ]
                            ]
                    else
                        table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td []
                                        [ button [ class "btn btn-default", onClick GetAssemblies ]
                                            [ text "Show Results" ]
                                        ]
                                    ]
                                ]
                            ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Assemblies "
                , View.Widgets.counter count
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

        count =
            if acceptableResults == [] then
                if model.combinedAssemblyQuery == "" then
                    model.project.combined_assembly_count
                else
                    0
            else
                List.length acceptableResults

        searchBar =
            if model.combined_assemblies == [] then
                text ""
            else
                small [ class "right" ]
                    [ input [ placeholder "Search", onInput SetCombinedAssemblyQuery ] [] ]

        body =
            if model.project.combined_assembly_count == 0 then
                text "None"
            else
                if model.loadedCombinedAssemblies then
                    if acceptableResults == [] then
                        text "None"
                    else
                        Table.view combinedAssemblyTableConfig model.combinedAssemblyTableState acceptableResults
                else
                    if model.loadingCombinedAssemblies then
                        table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td []
                                        [ spinner ]
                                    ]
                                ]
                            ]
                    else
                        table [ class "table" ]
                            [ tbody []
                                [ tr []
                                    [ td []
                                        [ button [ class "btn btn-default", onClick GetCombinedAssemblies ] [ text "Show Results" ] ]
                                    ]
                                ]
                            ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Combined Assemblies "
                , View.Widgets.counter count
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
                [ div [ style [ ("min-height","30vh"),("max-height","30vh") ] ]
                    [ br [] []
                    , if model.shareDialogError /= "" then
                        div [ class "alert alert-danger alert-dismissible" ]
                            [ button [ type_ "button", class "close" ]
                                [ span [ property "innerHTML" (Json.Encode.string "&times;"), onClick (ShareMsg CloseShareDialogError) ] [] ]
                            , text model.shareDialogError
                            ]
                      else if model.showShareDialogBusy then
                        spinner
                      else
                        div []
                            [ div [] [ text "Who has access" ]
                            , div [ class "scrollable", style [ ("max-height","30vh") ] ]
                                [ (viewUsersAndGroups currentUserId model.isEditable model.project.users model.project.project_groups)
                                , br [] [] -- kludge for dropdown not on top
                                , br [] []
                                , br [] []
                                ]
                            ]
                    ]
                , if model.isEditable then
                    addPanel
                  else
                    text ""
                ]

        addPanel =
            div []
                [ br [] []
                , hr [] []
                , br [] []
                , div [ class "form-group" ]
                    [ div [] [ text "Add a person or group:" ]
                    , div []
                        [ View.SearchableDropdown.view shareDropdownConfig model.shareDropdownState |> Html.map ShareMsg
                        ]
                    ]
                , br [] []
                ]
    in
    { closeMessage = Just (ShareMsg CloseShareDialog)
    , containerClass = Just "narrow-modal-container"
    , header = Just (h3 [] [ text "Share Project" ])
    , body = Just content
    , footer = Nothing
    }


shareDropdownConfig : View.SearchableDropdown.Config ShareMsg ShareMsg
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
        let
            sortByNameAndPerm a b =
                if a.permconn.permission == "owner" then
                    LT
                else if b.permconn.permission == "owner" then
                    GT
                else
                    compare (userDisplayName a) (userDisplayName b)
        in
        table [ class "table" ]
            [ tbody []
                ( (List.sortWith sortByNameAndPerm users |> List.map (\u -> viewUser (u.user_id == currentUserId) isEditable u)) ++
                    (List.sortBy .group_name groups |> List.map (\g -> viewGroup isEditable g))
                )
            ]


viewUser : Bool -> Bool -> User -> Html Msg
viewUser isMe isEditable user =
    tr []
        [ td []
            [ i [ class "fas fa-user" ] []
            , text " "
            , text (userDisplayName user)
            , if isMe then
                text " (you)"
              else
                text ""
            , if user.permconn.permission /= "owner" && isEditable then
                viewPermissionDropdown user
              else
                span [ class "pull-right" ] [ text (capitalize user.permconn.permission) ]
            ]
        ]


userDisplayName : User -> String
userDisplayName user =
    user.first_name ++ " " ++ user.last_name


viewGroup : Bool -> ProjectGroup -> Html Msg
viewGroup isEditable group =
    let
        numUsers =
            List.length group.users
    in
    tr []
        [ td []
            [ i [ class "fas fa-user-friends" ] []
            , text " "
            , a [ Route.href (Route.ProjectGroup group.project_group_id), target "_blank" ] [ text group.group_name ]
            , text " ("
            , text (toString numUsers)
            , text " "
            , text (Util.pluralize "user" numUsers)
            , text ")"
            , if isEditable then
                button [ class "btn btn-default btn-xs pull-right", onClick (RemoveFromProjectGroup group.project_group_id |> ShareMsg) ] [ text "Remove" ]
              else
                text ""
            ]
        ]


viewPermissionDropdown : User -> Html Msg
viewPermissionDropdown user =
    div [ class "pull-right dropdown" ]
        [ button [ class "btn btn-default btn-xs dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text (capitalize user.permconn.permission), text " ", span [ class "caret" ] [] ]
        , ul [ class "dropdown-menu nowrap" ]
            [ li [] [ a [ onClick (ShareWithUser "read-only" (toString user.user_id) user.user_name |> ShareMsg) ] [ text "Read-only: can view but not modify" ] ]
            , li [] [ a [ onClick (ShareWithUser "read-write" (toString user.user_id) user.user_name |> ShareMsg) ] [ text "Read-write: can view/edit but not delete" ] ]
            , li [] [ a [ onClick (UnshareWithUser user.user_id |> ShareMsg) ] [ text "Remove access" ] ]
            ]
        ]


publishDialogConfig : Int -> Model -> Dialog.Config Msg
publishDialogConfig currentUserId model =
    let
        description =
            div []
                [ p []
                    [ text "Projects and associated samples can be submitted to EBI's nucleotide archive (ENA). For further information please refer to the "
                    , a [] [ text "documentation" ]
                    , text "."
                    ]
                , p [] [ strong [] [ text "Important:" ], text " When submitted the project and it's associated samples can no longer be edited.  This is a final step in the project/sample creation process." ]
                ]

        content =
            if model.showPublishDialogBusy then
                spinner
            else if model.publishDialogError /= "" then
                let
                    errorList =
                        String.split "," model.publishDialogError

                    viewError msg =
                        li [] [ text msg ]
                in
                div []
                    [ description
                    , br [] []
                    , div [ class "alert alert-danger", style [("max-height","50vh"), ("overflow-y","auto")] ]
                        [ p [] [ text "The project cannot be submitted until these issues are corrected:" ]
                        , div []
                            [ ul [] (List.map viewError errorList) ]
                        ]
                    ]
            else
                case model.project.ebi_status of
                    Nothing ->
                        if model.validForPublish then
                            div []
                                [ description
                                , br [] []
                                , div [ class "alert alert-info" ] [ text "This project meets the requirements and is ready to be submitted.  Click the 'Publish' button to submit." ]
                                ]
                        else
                            text "Oops, an error occurred"

                    Just "FINISHED" ->
                        let
                            accn =
                                model.project.ebi_accn |> Maybe.withDefault "ERROR"

                            url =
                                "https://www.ebi.ac.uk/ena/data/view/" ++ accn -- FIXME hardcoded base url
                        in
                        div []
                            [ text "This project is published in EBI-ENA under accession "
                            , a [ href url, target "_blank" ] [ text accn ]
                            , text "."
                            ]

                    Just "FAILED" ->
                        div [ class "alert alert-danger", style [("max-height","10em"), ("overflow-y","auto")] ]
                            [ text "An error occurred while submitting the project.  Please contact ", a [ Route.href Route.Contact ] [ text "support" ], text " for assistance." ]

                    Just status ->
                        div []
                            [ p [] [ text "Submitting to EBI-ENA ..." ]
                            , viewStatus status
                            ]

        viewStatus status =
            let
                progressBar pct =
                    let
                        label = String.Extra.replace "_" " " status -- replace _ with space
                    in
                    div [ class "progress", style [("width","20em")] ]
                        [ div [ class "progress-bar progress-bar-striped active", style [("width", ((toString pct) ++ "%"))],
                                attribute "role" "progressbar", attribute "aria-valuenow" (toString pct), attribute "aria-valuemin" "0", attribute "aria-valuemax" "100" ]
                            [ text label ]
                        ]
            in
            case String.toUpper status of
                "PENDING" -> progressBar 10
                "CREATED" -> progressBar 20
                "INITIALIZING" -> progressBar 30
                "QUEUED" -> progressBar 40
                "STAGING_INPUTS" -> progressBar 50
                "SUBMITTING" -> progressBar 60
                "SUBMITTED" -> progressBar 70
                "FINISHED" -> progressBar 100
                _ -> progressBar 0

        footer =
            div []
                [ button [ class "btn btn-default pull-left", onClick (PublishMsg ClosePublishDialog) ] [ text "Close" ]
                , if model.project.ebi_status == Nothing && model.validForPublish then
                    button [ class "btn btn-primary", onClick (PublishMsg PublishProject) ] [ text "Publish" ]
                  else
                    text ""
                ]
    in
    { closeMessage = Just (PublishMsg ClosePublishDialog)
    , containerClass = Just "narrow-modal-container"
    , header = Just (h3 [] [ text "Publish Project" ])
    , body = Just content
    , footer = Just footer
    }


editInfoDialogConfig : Model -> Dialog.Config Msg
editInfoDialogConfig model =
    let
        content =
            Html.form [ style [("max-height","60vh"), ("overflow-y", "auto")] ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Name" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the name (required)", value model.projectName, onInput SetProjectName ] [] |> Html.map InfoMsg
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Description" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the description (required)", value model.projectDescription, onInput SetProjectDescription ] [] |> Html.map InfoMsg
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Accession" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the accession (required)", value model.projectCode, onInput SetProjectCode ] [] |> Html.map InfoMsg
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Type" ]
                    , div [ class "input-group" ]
                        [ input [ class "form-control", type_ "text", value model.projectType ] []
                        , div [ class "input-group-btn" ]
                            [ div [ class "dropdown" ]
                                [ button [ class "btn btn-default dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Select ", span [ class "caret" ] [] ]
                                , ul [ class "dropdown-menu dropdown-menu-right" ]
                                    (List.map (\s -> li [ onClick (SetProjectType s |> InfoMsg) ] [ a [] [ text s ]]) model.project.available_types)
                                ]
                            ]
                        ]
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Domains" ]
                    , View.TagsDropdown.view (domainDropdownConfig (List.map (\d -> (d.domain_id, d.domain_name)) model.project.available_domains)) model.domainDropdownState |> Html.map InfoMsg
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Institution" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the institution (required)", value model.projectInstitution, onInput SetProjectInstitution ] [] |> Html.map InfoMsg
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Investigators" ]
                    , div []
                        [ View.Tags.view (View.Tags.Config RemoveInvestigator) model.newInvestigatorTagState |> Html.map InfoMsg
                        , View.SearchableDropdown.view investigatorDropdownConfig model.investigatorDropdownState |> Html.map InfoMsg
                        ]
                    ]
--                , div [ class "form-group" ]
--                    [ label [] [ text "Groups" ]
--                    , View.TagsDropdown.view (groupDropdownConfig (List.map (\g -> (g.project_group_id, g.group_name)) model.project.available_groups)) model.groupDropdownState
--                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "URL" ]
                    , input [ class "form-control", type_ "text", placeholder "Enter the URL (optional)", value model.projectURL, onInput SetProjectURL ] [] |> Html.map InfoMsg
                    ]
                ]

        footer =
            div []
                [ button [ class "btn btn-default pull-left", onClick (InfoMsg CloseEditInfoDialog) ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick (InfoMsg UpdateProjectInfo) ] [ text "Update" ]
                ]
    in
    { closeMessage = Just (InfoMsg CloseEditInfoDialog)
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Modify Project Info" ])
    , body = Just content
    , footer = Just footer
    }


domainDropdownConfig : List (Int, String) -> View.TagsDropdown.Config InfoMsg InfoMsg
domainDropdownConfig domains =
    { options = domains
    , addMsg = AddProjectDomain
    , removeMsg = RemoveProjectDomain
    }


investigatorDropdownConfig : View.SearchableDropdown.Config InfoMsg InfoMsg
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
                        , input [ class "form-control", type_ "text", size 20, value model.publicationTitle, placeholder "Enter the title (required)", onInput SetPublicationTitle ] [] |> Html.map PublicationMsg
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Authors" ]
                        , input [ class "form-control", type_ "text", size 20, value model.publicationAuthors, placeholder "Enter the author names separated by commas (required)", onInput SetPublicationAuthors ] [] |> Html.map PublicationMsg
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Publication Date" ]
                        , input [ class "form-control", type_ "text", size 20, value model.publicationDate, placeholder "Enter the publication date as MM/DD/YYYY (required)", onInput SetPublicationDate ] [] |> Html.map PublicationMsg
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "PubMed ID" ]
                        , input [ class "form-control", type_ "text", size 20, value model.publicationPubMedID, placeholder "Enter the PubMed ID or leave blank (optional)", onInput SetPublicationPubMedID ] [] |> Html.map PublicationMsg
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "DOI" ]
                        , input [ class "form-control", type_ "text", size 20, value model.publicationDOI, placeholder "Enter the DOI or leave blank (optional)", onInput SetPublicationDOI ] [] |> Html.map PublicationMsg
                        ]
                    ]

        footer =
            div [ disabled model.showAddOrEditPublicationBusy ]
                [ button [ class "btn btn-default pull-left", onClick (PublicationMsg ClosePublicationDialog) ] [ text "Cancel" ]
                , case model.publicationIdToEdit of
                    Nothing ->
                        button [ class "btn btn-primary", onClick (PublicationMsg AddPublication) ] [ text "Add" ]

                    Just id ->
                        button [ class "btn btn-primary", onClick (UpdatePublication id |> PublicationMsg) ] [ text "Update" ]
                ]
    in
    { closeMessage = Just (PublicationMsg ClosePublicationDialog)
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add Publication" ])
    , body = Just content
    , footer = Just footer
    }
