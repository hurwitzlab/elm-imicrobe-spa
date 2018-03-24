module Page.Project exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Project exposing (Project, Investigator, Domain, Assembly, CombinedAssembly, Sample, Publication, ProjectGroup)
import Data.Sample
import Data.Investigator
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
import Request.Sample
import Request.Investigator
import Request.Publication
import Route
import Task exposing (Task)
import Table exposing (defaultCustomizations)
import View.Cart as Cart
import View.Spinner exposing (spinner)
import View.Dialog exposing (confirmationDialogConfig, infoDialogConfig, errorDialogConfig)
import Dict
import Util exposing ((=>))



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
    , infoDialog : Maybe (Dialog.Config Msg)
    , confirmationDialog : Maybe (Dialog.Config Msg)
    , showEditInfoDialog : Bool
    , newProjectName : String
    , newProjectCode : String
    , newProjectType : String
    , newProjectURL : String
    , showNewSampleDialog : Bool
    , showNewSampleBusy : Bool
    , newSampleName : String
    , showAddInvestigatorDialog : Bool
    , showAddInvestigatorBusy : Bool
    , newInvestigatorId : Maybe Int
    , newInvestigatorName : String
    , investigatorSearchResults : List (Int, String)
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
            Request.Project.get id |> Http.toTask

        isEditable project =
            case session.user of
                Nothing ->
                    False

                Just user ->
                    List.map .user_name project.users |> List.member user.user_name
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
                    , infoDialog = Nothing
                    , confirmationDialog = Nothing
                    , showEditInfoDialog = False
                    , newProjectName = ""
                    , newProjectCode = ""
                    , newProjectType = ""
                    , newProjectURL = ""
                    , showNewSampleDialog = False
                    , showNewSampleBusy = False
                    , newSampleName = ""
                    , showAddInvestigatorDialog = False
                    , showAddInvestigatorBusy = False
                    , newInvestigatorId = Nothing
                    , newInvestigatorName = ""
                    , investigatorSearchResults = []
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
    | OpenInfoDialog String
    | CloseInfoDialog
    | OpenEditInfoDialog
    | CloseEditInfoDialog
    | SetNewProjectName String
    | SetNewProjectCode String
    | SetNewProjectType String
    | SetNewProjectURL String
    | UpdateProjectInfo
    | UpdateProjectInfoCompleted (Result Http.Error Project)
    | OpenNewSampleDialog
    | CloseNewSampleDialog
    | SetNewSampleName String
    | CreateNewSample
    | CreateNewSampleCompleted (Result Http.Error Data.Sample.Sample)
    | RemoveSample Int
    | RemoveSampleCompleted (Result Http.Error Project)
    | OpenAddInvestigatorDialog
    | CloseAddInvestigatorDialog
    | SetInvestigatorName String
    | SearchInvestigatorCompleted (Result Http.Error (List Data.Investigator.Investigator))
    | SelectInvestigatorToAdd Int String
    | AddInvestigator
    | AddInvestigatorCompleted (Result Http.Error Project)
    | RemoveInvestigator Int
    | RemoveInvestigatorCompleted (Result Http.Error Project)
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

        OpenInfoDialog infoText ->
            let
                dialog =
                    infoDialogConfig infoText CloseInfoDialog
            in
            { model | infoDialog = Just dialog } => Cmd.none => NoOp

        CloseInfoDialog ->
            { model | infoDialog = Nothing } => Cmd.none => NoOp

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

        UpdateProjectInfo ->
            let
                updateInfo =
                    Request.Project.update session.token model.project_id model.newProjectName model.newProjectCode model.newProjectType model.newProjectURL |> Http.toTask
            in
            { model | showEditInfoDialog = False } => Task.attempt UpdateProjectInfoCompleted updateInfo => NoOp

        UpdateProjectInfoCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | project = { newProject | project_name = project.project_name, project_code = project.project_code, project_type = project.project_type, url = project.url } } => Cmd.none => NoOp

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
                        |> Task.andThen
                            (\_ -> Request.Project.get model.project_id |> Http.toTask)
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

        OpenAddInvestigatorDialog ->
            { model | showAddInvestigatorDialog = True, showAddInvestigatorBusy = False, newInvestigatorId = Nothing, newInvestigatorName = "" } => Cmd.none => NoOp

        CloseAddInvestigatorDialog ->
            { model | showAddInvestigatorDialog = False } => Cmd.none => NoOp

        SetInvestigatorName name ->
            let
                searchByName =
                    Request.Investigator.searchByName name |> Http.toTask
                        |> Task.attempt SearchInvestigatorCompleted

                cmd =
                    if String.length name > 2 then
                        searchByName
                    else
                        Cmd.none
            in
            { model | newInvestigatorName = name } => cmd => NoOp

        SearchInvestigatorCompleted (Ok investigators) ->
            let
                results = List.map (\i -> (i.investigator_id, i.investigator_name)) investigators
            in
            { model | investigatorSearchResults = results } => Cmd.none => NoOp

        SearchInvestigatorCompleted (Err error) -> -- TODO finish this
            model => Cmd.none => NoOp

        SelectInvestigatorToAdd id name ->
            { model | newInvestigatorId = Just id, newInvestigatorName = name, investigatorSearchResults = [] } => Cmd.none => NoOp

        AddInvestigator ->
            case model.newInvestigatorId of
                Nothing ->
                    model => Cmd.none => NoOp

                Just id ->
                    let
                        addInvestigator =
                            Request.Project.addInvestigatorToProject session.token model.project_id id |> Http.toTask
                    in
                    { model | showAddInvestigatorBusy = True } => Task.attempt AddInvestigatorCompleted addInvestigator => NoOp

        AddInvestigatorCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | showAddInvestigatorDialog = False, project = { newProject | investigators = project.investigators } } => Cmd.none => NoOp

        AddInvestigatorCompleted (Err error) -> -- TODO finish this
            model => Cmd.none => NoOp

        RemoveInvestigator id ->
            let
                removeInvestigator =
                    Request.Project.removeInvestigatorFromProject session.token model.project_id id |> Http.toTask
                        |> Task.andThen
                            (\_ -> Request.Project.get model.project_id |> Http.toTask)
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemoveInvestigatorCompleted removeInvestigator => NoOp

        RemoveInvestigatorCompleted (Ok project) ->
            let
                newProject =
                    model.project
            in
            { model | project = { newProject | investigators = project.investigators } } => Cmd.none => NoOp

        RemoveInvestigatorCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

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
                        |> Task.andThen
                            (\_ -> Request.Project.get model.project_id |> Http.toTask)
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
                        |> Task.andThen
                            (\_ -> Request.Project.get model.project_id |> Http.toTask)
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
                        |> Task.andThen
                            (\_ -> Request.Project.get model.project_id |> Http.toTask)
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
    let
        privateButton =
            if model.project.private == 1 then
                let
                    infoText =
                        "This feature is currently under development.  Soon you will be able to 'publish' your project (making it publicly accessible) or share with collaborators."
                in
                button [ class "btn btn-default pull-right", onClick (OpenInfoDialog infoText) ]
                    [ span [ class "glyphicon glyphicon-lock" ] [], text " Project is Private" ]

            else
                text ""
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small [] [ text model.project.project_name ]
                    , privateButton
                    ]
                ]
            , viewProject model.project model.isEditable
            , viewInvestigators model.project.investigators model.isEditable
            , viewPublications model.project.publications model.isEditable
            , viewSamples model.cart model.project.samples model.isEditable
            , viewAssemblies model
            , viewCombinedAssemblies model
            ]
        , Dialog.view
            (if model.showNewSampleDialog then
                Just (newSampleDialogConfig model)
             else if model.infoDialog /= Nothing then
                model.infoDialog
             else if (model.confirmationDialog /= Nothing) then
                model.confirmationDialog
             else if model.showEditInfoDialog then
                Just (editInfoDialogConfig model)
             else if model.showAddInvestigatorDialog then
                Just (addInvestigatorDialogConfig model)
             else if model.showAddOrEditPublicationDialog then
                Just (addOrEditPublicationDialogConfig model)
             else
                Nothing
            )
        ]


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
            [ th [] [ text "Groups" ]
            , td [] (viewProjectGroups project.project_groups)
            ]
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


viewInvestigators : List Investigator -> Bool -> Html Msg
viewInvestigators investigators isEditable =
    let
        numInvs =
            List.length investigators

        label =
            case numInvs of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text (toString numInvs)
                        ]

        body =
            case numInvs of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (List.map (viewInvestigator isEditable) investigators) ]

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm pull-right", onClick OpenAddInvestigatorDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Investigator" ]

                False ->
                    text ""
    in
    div []
        [ h2 []
            [ text "Investigators "
            , label
            , addButton
            ]
        , body
        ]


viewInvestigator : Bool -> Investigator -> Html Msg
viewInvestigator isEditable investigator =
    let
        removeButton =
            if isEditable then
                [ button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Are you sure you want to remove this investigator?" (RemoveInvestigator investigator.investigator_id)) ] [ text "Remove" ]
                ]
            else
                [ text "" ]
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Investigator investigator.investigator_id) ]
                [ text investigator.investigator_name ]
            ]
        , td []
            removeButton
        ]


viewDomain : Domain -> Html msg
viewDomain domain =
    text domain.domain_name


viewDomains : List Domain -> List (Html msg)
viewDomains domains =
    case List.length domains of
        0 ->
            [ text "None" ]

        _ ->
            List.intersperse (text ", ") (List.map viewDomain domains)


viewProjectGroup : ProjectGroup -> Html msg
viewProjectGroup group =
    a [ Route.href (Route.ProjectGroup group.project_group_id) ]
        [ text group.group_name ]


viewProjectGroups : List ProjectGroup -> List (Html msg)
viewProjectGroups groups =
    case List.length groups of
        0 ->
            [ text "None" ]

        _ ->
            List.intersperse (text ", ") (List.map viewProjectGroup groups)


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
                , button [ class "btn btn-default btn-xs pull-right", onClick (OpenAddOrEditPublicationDialog (Just pub)) ] [ text "Edit" ]
                ]
            else
                [ text "" ]
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Publication pub.publication_id) ]
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
        cols =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                , th [ class "col-md-1 nowrap" ] []
                , th [ class "col-md-1 nowrap" ] [ Cart.addAllToCartButton cart (List.map .sample_id samples) |> Html.map CartMsg ]
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
                    button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Remove the sample (this cannot be undone)?" (RemoveSample sample.sample_id)) ] [ text " Remove from Project" ]

                False ->
                    text ""
    in
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        , td [] [ removeButton ]
        , td [] [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg ]
        ]


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


toTableAttrs : List (Html.Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed" ]


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


editInfoDialogConfig : Model -> Dialog.Config Msg
editInfoDialogConfig model =
    let
        content =
            Html.form []
                [ div [ class "form-group" ]
                    [ label [] [ text "Name" ]
                    , input [ class "form-control", type_ "text", size 20, placeholder "Enter the name (required)", value model.newProjectName, onInput SetNewProjectName ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Accession" ]
                    , input [ class "form-control", type_ "text", size 20, placeholder "Enter the accession (required)", value model.newProjectCode, onInput SetNewProjectCode ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Type" ]
                    , input [ class "form-control", type_ "text", size 20, placeholder "Enter the type (required)", value model.newProjectType, onInput SetNewProjectType ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Domains" ]
                    , input [ class "form-control", type_ "text", size 20, placeholder "Enter the domains (optional)" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Groups" ]
                    , input [ class "form-control", type_ "text", size 20, placeholder "Enter the groups (optional)" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "URL" ]
                    , input [ class "form-control", type_ "text", size 20, placeholder "Enter the URL (optional)", value model.newProjectURL, onInput SetNewProjectURL ] []
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


newSampleDialogConfig : Model -> Dialog.Config Msg
newSampleDialogConfig model =
    let
        content =
            if model.showNewSampleBusy then
                spinner
            else
                input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new sample", onInput SetNewSampleName ] []

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


addInvestigatorDialogConfig : Model -> Dialog.Config Msg
addInvestigatorDialogConfig model =
    let
        content =
            if model.showAddInvestigatorBusy then
                spinner
            else
                let
                    invOption (id, name) =
                        tr [ onClick (SelectInvestigatorToAdd id name) ] [ td [] [ text name ] ]

                    resultTable =
                        div [ style [("overflow-y","auto"),("max-height","10em")] ]
                            [ table [ class "table-condensed table-hover", style [("width","100%")] ]
                                [ tbody [] (List.map invOption model.investigatorSearchResults) ]
                            ]
                in
                div []
                    [ input [ class "form-control", type_ "text", size 20, value model.newInvestigatorName, placeholder "Enter the name of the investigator to add", onInput SetInvestigatorName ] []
                    , if model.investigatorSearchResults /= [] then
                        resultTable
                      else
                        text ""
                    ]

        footer =
            div [ disabled model.showAddInvestigatorBusy ]
                [ button [ class "btn btn-default pull-left", onClick CloseAddInvestigatorDialog ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick AddInvestigator ] [ text "Add" ]
                ]
    in
    { closeMessage = Just CloseAddInvestigatorDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add Investigator" ])
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
