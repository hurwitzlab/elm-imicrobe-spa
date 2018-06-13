module Page.Sample exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (..)
import Data.Session as Session exposing (Session)
import Data.Cart
import Json.Encode as Encode exposing (Value)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Dialog
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Sample
import Route
import Time exposing (Time)
import Task exposing (Task)
import Config exposing (dataCommonsUrl)
import Table exposing (defaultCustomizations)
import List.Extra
import Maybe.Extra
import Util exposing ((=>))
import View.Cart as Cart
import View.Dialog exposing (confirmationDialogConfig, infoDialogConfig, errorDialogConfig)
import View.Spinner exposing (spinner)
import View.GMap as GMap exposing (LatLng, MapState, gmap, loadMap, setCenter)
import View.FileBrowser as FileBrowser



---- MODEL ----


type alias Model =
    { pageTitle : String
    , sample_id : Int
    , sample : Sample
    , showMap : Bool
    , mapState : MapState
    , cart : Cart.Model
    , loadingProteins : Bool
    , loadedProteins : Bool
    , proteins : Proteins
    , loadingCentrifugeResults : Bool
    , loadedCentrifugeResults : Bool
    , centrifugeResults : List SampleToCentrifuge
    , attrTableState : Table.State
    , pfamTableState : Table.State
    , keggTableState : Table.State
    , centrifugeTableState : Table.State
    , attrQuery : String
    , proteinQuery : String
    , centrifugeQuery : String
    , proteinFilterType : String
    , isEditable : Bool
    , currentUser : Maybe User
    , showNewAttributeDialog : Bool
    , showNewAttributeBusy : Bool
    , attributeType : String
    , attributeAliases : String
    , attributeValue : String
    , showModifyAttributeDialog : Bool
    , showModifyAttributeBusy : Bool
    , attributeToModify : Maybe Sample.Attribute
    , confirmationDialog : Maybe (Dialog.Config Msg)
    , infoDialog : Maybe (Dialog.Config Msg)
    , showAddFilesDialog : Bool
    , showEditInfoDialog : Bool
    , sampleName : String
    , sampleCode : String
    , sampleType : String
    , dialogError : Maybe String
    , fileBrowser : FileBrowser.Model
    }


type PFAMorKEGG
    = PFAM UProC_PFAM
    | KEGG UProC_KEGG


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        loadSample =
            Request.Sample.get session.token id |> Http.toTask

        userId =
            Maybe.map .user_id session.user

        allUsers sample =
            sample.project.users ++ (List.map .users sample.project.project_groups |> List.concat)

        isEditable sample =
            case userId of
                Nothing ->
                    False

                Just userId ->
                    List.any (\u -> u.user_id == userId && (u.permconn.permission == "owner" || u.permconn.permission == "read-write")) (allUsers sample)

        currentUser sample =
            case userId of
                Nothing ->
                    Nothing

                Just userId ->
                    List.filter (\u -> u.user_id == userId) (allUsers sample) |> List.head
    in
    loadSample
        |> Task.andThen
            (\sample ->
                let
                    getAttrValue name =
                        case List.Extra.find (\attr -> attr.sample_attr_type.type_ == name) sample.sample_attrs of
                            Nothing ->
                                0

                            Just attr ->
                                Result.withDefault 0 (String.toFloat attr.attr_value)

                    lat =
                        getAttrValue "latitude"

                    lng =
                        getAttrValue "longitude"
                in
                Task.succeed
                    { pageTitle = "Sample"
                    , sample_id = id
                    , sample = sample
                    , showMap = False
                    , mapState = MapState (Encode.string "google map here") (LatLng lat lng)
                    , cart = Cart.init session.cart Cart.Editable
                    , loadingProteins = False
                    , loadedProteins = False
                    , proteins = Proteins [] []
                    , loadingCentrifugeResults = False
                    , loadedCentrifugeResults = False
                    , centrifugeResults = []
                    , attrTableState = Table.initialSort "Name"
                    , pfamTableState = Table.initialSort "Read Count"
                    , keggTableState = Table.initialSort "Read Count"
                    , centrifugeTableState = Table.initialSort "Abundance"
                    , attrQuery = ""
                    , proteinQuery = ""
                    , centrifugeQuery = ""
                    , proteinFilterType = "PFAM"
                    , isEditable = isEditable sample
                    , currentUser = currentUser sample
                    , showNewAttributeDialog = False
                    , showNewAttributeBusy = False
                    , attributeType = ""
                    , attributeAliases = ""
                    , attributeValue = ""
                    , showModifyAttributeDialog = False
                    , showModifyAttributeBusy = False
                    , attributeToModify = Nothing
                    , confirmationDialog = Nothing
                    , infoDialog = Nothing
                    , showAddFilesDialog = False
                    , showEditInfoDialog = False
                    , sampleName = ""
                    , sampleCode = ""
                    , sampleType = ""
                    , dialogError = Nothing
                    , fileBrowser = FileBrowser.init session (Just (FileBrowser.Config False False False))
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = MapTick Time
    | JSMap Value
    | SetAttrQuery String
    | SetProteinQuery String
    | SetCentrifugeQuery String
    | SetAttrTableState Table.State
    | SetPFAMTableState Table.State
    | SetKEGGTableState Table.State
    | SetCentrifugeTableState Table.State
    | FilterProteinType String
    | GetProteins
    | SetProteins Proteins
    | GetCentrifugeResults
    | SetCentrifugeResults (List SampleToCentrifuge)
    | OpenNewAttributeDialog
    | CloseNewAttributeDialog
    | CreateNewAttribute
    | CreateNewAttributeCompleted (Result Http.Error Sample)
    | SetAttributeType String
    | SetAttributeAliases String
    | SetAttributeValue String
    | RemoveAttribute Int
    | RemoveAttributeCompleted (Result Http.Error Sample)
    | OpenModifyAttributeDialog Sample.Attribute
    | CloseModifyAttributeDialog
    | UpdateAttribute Int
    | UpdateAttributeCompleted (Result Http.Error Sample)
    | OpenConfirmationDialog String Msg
    | OpenInfoDialog String
    | OpenAddFilesDialog
    | OpenEditInfoDialog
    | CloseConfirmationDialog
    | CloseInfoDialog
    | CloseErrorDialog
    | CloseAddFilesDialog
    | CloseEditInfoDialog
    | SetSampleName String
    | SetSampleCode String
    | SetSampleType String
    | UpdateSampleInfo
    | UpdateSampleInfoCompleted (Result Http.Error Sample)
    | AddFiles (List String)
    | AddFilesCompleted (Result Http.Error Sample)
    | RemoveFile Int
    | RemoveFileCompleted (Result Http.Error Sample)
    | SetFileType Int String
    | SetFileTypeCompleted (Result Http.Error String)
    | CartMsg Cart.Msg
    | FileBrowserMsg FileBrowser.Msg


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        JSMap gmap ->
            { model | mapState = MapState gmap model.mapState.center } => Cmd.none => NoOp

        MapTick _ ->
            { model | showMap = True } => loadMap model.mapState.center => NoOp

        SetAttrQuery newQuery ->
            { model | attrQuery = newQuery } => Cmd.none => NoOp

        SetProteinQuery newQuery ->
            { model | proteinQuery = newQuery } => Cmd.none => NoOp

        SetCentrifugeQuery newQuery ->
            { model | centrifugeQuery = newQuery } => Cmd.none => NoOp

        SetAttrTableState newState ->
            { model | attrTableState = newState } => Cmd.none => NoOp

        SetPFAMTableState newState ->
            { model | pfamTableState = newState } => Cmd.none => NoOp

        SetKEGGTableState newState ->
            { model | keggTableState = newState } => Cmd.none => NoOp

        SetCentrifugeTableState newState ->
            { model | centrifugeTableState = newState } => Cmd.none => NoOp

        FilterProteinType filterType ->
            { model | proteinFilterType = filterType } => Cmd.none => NoOp

        GetProteins ->
            let
                loadProteins =
                    Request.Sample.proteins session.token model.sample_id |> Http.toTask

                handleProteins proteins =
                    case proteins of
                        Ok proteins ->
                            SetProteins proteins

                        Err error ->
                            let
                                _ = Debug.log "Error" ("could not retrieve proteins: " ++ (toString error))
                            in
                            SetProteins (Proteins [] [])
            in
            { model | loadingProteins = True } => Task.attempt handleProteins loadProteins => NoOp

        SetProteins proteins ->
            { model | loadedProteins = True, proteins = proteins } => Cmd.none => NoOp

        GetCentrifugeResults ->
            let
                loadCentrifugeResults =
                    Request.Sample.centrifuge_results session.token model.sample_id |> Http.toTask

                handleCentrifugeResults results =
                    case results of
                        Ok results ->
                            SetCentrifugeResults results

                        Err error ->
                            let
                                _ = Debug.log "Error" ("could not retrieve centrifuge results: " ++ (toString error))
                            in
                            SetCentrifugeResults []
            in
            { model | loadingCentrifugeResults = True } => Task.attempt handleCentrifugeResults loadCentrifugeResults => NoOp

        SetCentrifugeResults results ->
            { model | loadedCentrifugeResults = True, centrifugeResults = results } => Cmd.none => NoOp

        OpenNewAttributeDialog ->
            { model | showNewAttributeDialog = True, showNewAttributeBusy = False, attributeType = "", attributeAliases = "", attributeValue = "" } => Cmd.none => NoOp

        CloseNewAttributeDialog ->
            { model | showNewAttributeDialog = False } => Cmd.none => NoOp

        CreateNewAttribute ->
            let
                createAttribute =
                    Request.Sample.addAttribute session.token model.sample_id model.attributeType model.attributeAliases model.attributeValue |> Http.toTask
            in
            { model | showNewAttributeBusy = True } => Task.attempt CreateNewAttributeCompleted createAttribute => NoOp

        CreateNewAttributeCompleted (Ok sample) ->
            let
                newSample =
                    model.sample
            in
            { model | showNewAttributeDialog = False, sample = { newSample | sample_attrs = sample.sample_attrs } } => Cmd.none => NoOp

        CreateNewAttributeCompleted (Err error) ->
            { model | showNewAttributeDialog = False, dialogError = Just (toString error) } => Cmd.none => NoOp

        SetAttributeType val ->
            { model | attributeType = val } => Cmd.none => NoOp

        SetAttributeAliases val ->
            { model | attributeAliases = val } => Cmd.none => NoOp

        SetAttributeValue val ->
            { model | attributeValue = val } => Cmd.none => NoOp

        OpenModifyAttributeDialog attr ->
            { model
                | attributeToModify = Just attr
                , showModifyAttributeBusy = False
                , attributeType = attr.sample_attr_type.type_
                , attributeAliases = aliasesToString attr.sample_attr_type.sample_attr_type_aliases
                , attributeValue = attr.attr_value
            } => Cmd.none => NoOp

        CloseModifyAttributeDialog ->
            { model | attributeToModify = Nothing } => Cmd.none => NoOp

        UpdateAttribute attr_id ->
            let
                updateAttribute =
                    Request.Sample.updateAttribute session.token model.sample_id attr_id model.attributeType model.attributeAliases model.attributeValue |> Http.toTask
            in
            { model | showModifyAttributeBusy = True } => Task.attempt UpdateAttributeCompleted updateAttribute => NoOp

        UpdateAttributeCompleted (Ok sample) ->
            let
                newSample =
                    model.sample
            in
            { model | attributeToModify = Nothing, sample = { newSample | sample_attrs = sample.sample_attrs } } => Cmd.none => NoOp

        UpdateAttributeCompleted (Err error) ->
            { model | attributeToModify = Nothing, dialogError = Just (toString error) } => Cmd.none => NoOp

        RemoveAttribute attr_id ->
            let
                removeAttribute =
                    Request.Sample.removeAttribute session.token model.sample_id attr_id |> Http.toTask
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemoveAttributeCompleted removeAttribute => NoOp

        RemoveAttributeCompleted (Ok sample) ->
            let
                newSample =
                    model.sample
            in
            { model | sample = { newSample | sample_attrs = sample.sample_attrs } } => Cmd.none => NoOp

        RemoveAttributeCompleted (Err error) ->
            model => Cmd.none => NoOp

        OpenConfirmationDialog confirmationText yesMsg ->
            let
                dialog =
                    confirmationDialogConfig confirmationText CloseConfirmationDialog yesMsg
            in
            { model | confirmationDialog = Just dialog } => Cmd.none => NoOp

        CloseConfirmationDialog ->
            { model | confirmationDialog = Nothing } => Cmd.none => NoOp

        CloseErrorDialog ->
            { model | dialogError = Nothing } => Cmd.none => NoOp

        OpenInfoDialog infoText ->
            let
                dialog =
                    infoDialogConfig infoText CloseInfoDialog
            in
            { model | infoDialog = Just dialog } => Cmd.none => NoOp

        CloseInfoDialog ->
            { model | infoDialog = Nothing } => Cmd.none => NoOp

        OpenAddFilesDialog ->
            let
                (subModel, subCmd) =
                    FileBrowser.update session FileBrowser.RefreshPath model.fileBrowser
            in
            { model | showAddFilesDialog = True, fileBrowser = subModel  } => Cmd.map FileBrowserMsg subCmd => NoOp

        CloseAddFilesDialog ->
            { model | showAddFilesDialog = False } => Cmd.none => NoOp

        AddFiles files ->
            let
                addFiles =
                    Request.Sample.addFiles session.token model.sample_id files |> Http.toTask
            in
            { model | showAddFilesDialog = False } => Task.attempt AddFilesCompleted addFiles => NoOp

        AddFilesCompleted (Ok sample) ->
            let
                newSample =
                    model.sample
            in
            { model | sample = { newSample | sample_files = sample.sample_files } } => Cmd.none => NoOp

        AddFilesCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        RemoveFile file_id ->
            let
                removeFile =
                    Request.Sample.removeFile session.token model.sample_id file_id |> Http.toTask
            in
            { model | confirmationDialog = Nothing } => Task.attempt RemoveFileCompleted removeFile => NoOp

        RemoveFileCompleted (Ok sample) ->
            let
                newSample =
                    model.sample
            in
            { model | sample = { newSample | sample_files = sample.sample_files } } => Cmd.none => NoOp

        RemoveFileCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        SetFileType sampleFileId fileType ->
            let
                fileTypeId =
                    case String.toInt fileType of
                        Ok id -> id
                        Err error -> 0 -- FIXME

                updateFile =
                    Request.Sample.updateFile session.token model.sample_id sampleFileId fileTypeId |> Http.toTask
            in
            { model | confirmationDialog = Nothing } => Task.attempt SetFileTypeCompleted updateFile => NoOp

        SetFileTypeCompleted (Ok _) ->
            model => Cmd.none => NoOp

        SetFileTypeCompleted (Err error) ->
            let
                _ = Debug.log "Error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        OpenEditInfoDialog ->
            { model
                | showEditInfoDialog = True
                , sampleName = model.sample.sample_name
                , sampleCode = model.sample.sample_acc
                , sampleType = model.sample.sample_type
             } => Cmd.none => NoOp

        CloseEditInfoDialog ->
            { model | showEditInfoDialog = False } => Cmd.none => NoOp

        SetSampleName val ->
            { model | sampleName = val } => Cmd.none => NoOp

        SetSampleCode val ->
            { model | sampleCode = val } => Cmd.none => NoOp

        SetSampleType val ->
            { model | sampleType = val } => Cmd.none => NoOp

        UpdateSampleInfo ->
            let
                updateInfo =
                    Request.Sample.update session.token model.sample_id model.sampleName model.sampleCode model.sampleType |> Http.toTask
            in
            { model | showEditInfoDialog = False } => Task.attempt UpdateSampleInfoCompleted updateInfo => NoOp

        UpdateSampleInfoCompleted (Ok sample) ->
            let
                newSample =
                    model.sample
            in
            { model | sample = { newSample | sample_name = sample.sample_name, sample_acc = sample.sample_acc, sample_type = sample.sample_type } } => Cmd.none => NoOp

        UpdateSampleInfoCompleted (Err error) ->
            let
                _ = Debug.log "error" (toString error) -- TODO show to user
            in
            model => Cmd.none => NoOp

        CartMsg subMsg ->
            let
                _ = Debug.log "Sample.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart

        FileBrowserMsg subMsg ->
            let
                ( newFileBrowser, subCmd ) =
                    FileBrowser.update session subMsg model.fileBrowser
            in
            { model | fileBrowser = newFileBrowser } => Cmd.map FileBrowserMsg subCmd => NoOp


-- VIEW --


view : Model -> Html Msg
view model =
    let
        showMapButton =
            let
                attrExists name =
                    List.Extra.find (\attr -> attr.sample_attr_type.type_ == name) model.sample.sample_attrs |> Maybe.Extra.isJust
            in
            attrExists "latitude" && attrExists "longitude"

        user =
            List.filter
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.sample.sample_name ]
                    , div [ class "pull-right" ]
                        [ viewShareButton model
                        , text " "
                        , Cart.addToCartButton2 model.cart model.sample.sample_id |> Html.map CartMsg
                        ]
                    ]
                ]
            , viewSample model.sample model.isEditable
            , if showMapButton then
                viewMap model.showMap
              else
                text ""
            , viewFiles model.sample.sample_files model.isEditable
            , viewAssemblies model.sample.assemblies
            , viewCombinedAssemblies model.sample.combined_assemblies
            , viewAttributes model model.isEditable
            , if not model.isEditable then
                viewProteins model
              else
                text ""
            , if not model.isEditable then
                viewCentrifugeResults model
              else
                text ""
            ]
        , Dialog.view
            (if (model.dialogError /= Nothing) then
                Just (errorDialogConfig (Maybe.withDefault "Unknown error" model.dialogError) CloseErrorDialog)
             else if (model.infoDialog /= Nothing) then
                model.infoDialog
             else if (model.confirmationDialog /= Nothing) then
                model.confirmationDialog
             else if model.showNewAttributeDialog then
                Just (newAttributeDialogConfig model.showNewAttributeBusy)
             else if model.showAddFilesDialog then
                Just (addFilesDialogConfig model False)
             else if model.showEditInfoDialog then
                Just (editInfoDialogConfig model False)
             else
                case model.attributeToModify of
                    Nothing ->
                        Nothing

                    Just attr ->
                        Just (editAttributeDialogConfig model attr.sample_attr_id model.showModifyAttributeBusy)
            )
        ]


--FIXME messy, clean up
viewShareButton : Model -> Html Msg
viewShareButton model =
    if model.sample.project.private == 1 then
        let
            (buttonLabel, permissionText, sharingText) =
                if List.length model.sample.project.users <= 1 && model.sample.project.project_groups == [] then -- users will always have the owner
                    ("Sample is Private", "You are the owner of this sample.", "  This sample is only visible to you.  To share, open the parent project and click the sharing button.")
                else
                    let
                        permText =
                            case model.currentUser of
                                Nothing -> ""

                                Just user ->
                                    if (user.permconn.permission == "owner") then
                                        "You are the owner of this sample."
                                    else if (model.isEditable) then
                                        "You have read-write access to this sample."
                                    else
                                        "You have read-only access to this sample."
                    in
                    ("Sample is Shared", permText, "  This sample is shared with other users and/or groups.  To view or modify the sharing settings, open the parent project and click the sharing button.")
        in
        button [ class "btn btn-default", onClick (OpenInfoDialog (permissionText ++ sharingText)) ]
            [ span [ class "glyphicon glyphicon-lock" ] [], text " ", text buttonLabel ]

    else
        text ""


viewSample : Sample -> Bool -> Html Msg
viewSample sample isEditable =
    let
        numFiles =
            List.length sample.sample_files

        ontologies =
            if sample.ontologies == [] then
                "none"
            else
                List.map (\o -> o.ontology_acc ++ o.label) sample.ontologies |> String.join ", "

        editButton =
            if isEditable then
                button [ class "btn btn-default btn-xs", onClick OpenEditInfoDialog ] [ span [ class "glyphicon glyphicon-cog" ] [], text " Edit" ]
            else
                text ""
    in
    table [ class "table" ]
        [ colgroup []
            [ col [ class "col-md-2" ] [] ]
        , tr []
            [ th [] [ text "Project" ]
            , td []
                [ a [ Route.href (Route.Project sample.project_id) ] [ text sample.project.project_name ]
                ]
            ]
        , tr []
            [ th [] [ text "Sample" ]
            , td [] [ text sample.sample_name ]
            ]
        , tr []
            [ th [] [ text "Code" ]
            , td [] [ text sample.sample_acc ]
            ]
        , tr []
            [ th [] [ text "Sample Type" ]
            , td [] [ text sample.sample_type ]
            ]
        , tr []
            [ td [] [ editButton ]
            ]
        ]


mapStyles : Html.Attribute msg
mapStyles =
    style
        [ ( "display", "block" )
        , ( "height", "200px" )
        , ( "width", "400px" )
        ]


viewMap : Bool -> Html Msg
viewMap showMap =
    let
        hideorShow =
            if showMap then
                style [ ( "display", "block") ]
            else
                style [ ( "display", "none") ]
    in
    div []
        [ button [ class "btn btn-xs btn-default", onClick (MapTick Time.millisecond) ] [ text "View Map" ]
        , div [ hideorShow ] [ gmap [ mapStyles ] [] ] -- needs to be always rendered for port to work
        ]


editInfoDialogConfig : Model -> Bool -> Dialog.Config Msg
editInfoDialogConfig model isBusy =
    let
        content =
            if isBusy then
                spinner
            else
                Html.form []
                    [ div [ class "form-group" ]
                        [ label [] [ text "Sample Name" ]
                        , input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the name (required)", value model.sampleName, onInput SetSampleName ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Sample Code" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the code (required)", value model.sampleCode, onInput SetSampleCode ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Type" ]
                        , div [ class "input-group" ]
                            [ input [ class "form-control", type_ "text", value model.sampleType ] []
                            , div [ class "input-group-btn" ]
                                [ div [ class "dropdown" ]
                                    [ button [ class "btn btn-default dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Select ", span [ class "caret" ] [] ]
                                    , ul [ class "dropdown-menu dropdown-menu-right" ]
                                        (List.map (\s -> li [ onClick (SetSampleType s) ] [ a [] [ text s ]]) model.sample.available_types)
                                    ]
                                ]
                            ]
                        ]
                    ]

        footer =
            div [ disabled isBusy ]
                [ button [ class "btn btn-default pull-left", onClick CloseEditInfoDialog ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick UpdateSampleInfo ] [ text "Update" ]
                ]
    in
    { closeMessage = Just CloseEditInfoDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Modify Sample Info" ])
    , body = Just content
    , footer = Just footer
    }


viewFiles : List SampleFile2 -> Bool -> Html Msg
viewFiles files isEditable =
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
                , th [] []
                ]

        body =
            if numFiles == 0 then
                text "None"
            else
                table [ class "table table-condensed" ]
                    [ tbody [] (cols :: (List.map (viewFile isEditable) files)) ]

        addButton =
            if isEditable then
                button [ class "btn btn-default btn-sm pull-right", onClick OpenAddFilesDialog ]
                    [ span [ class "glyphicon glyphicon-plus" ] [], text " Add File(s)"
                    ]
            else
                text ""
    in
    div []
        [ h2 []
            [ text "Files "
            , label
            , addButton
            ]
        , body
        ]


viewFile : Bool -> SampleFile2 -> Html Msg
viewFile isEditable file =
    let
        availableTypes =
            [ (1, "Reads"), (2, "Contigs"), (7, "Assembly"), (51, "Annotation"), (36, "Meta"), (35, "Unknown") ]

        makeOption (id, name) =
            option [ value (toString id), selected (name == file.sample_file_type.file_type) ] [ text name ]
    in
    tr []
        [ td []
            [ a [ href (dataCommonsUrl ++ file.file), target "_blank" ] [ text file.file ]
            ]
        , td []
            [ if isEditable then
--                div [ class "btn-group" ]
--                    [ button [ class "btn btn-default btn-xs dropdown-toggle", type_ "button", attribute "data-toggle" "dropdown" ] [ text "Select ", span [ class "caret" ] [] ]
--                    , ul [ class "dropdown-menu dropdown-menu-right" ]
--                        (List.map (\s -> li [ onClick (SetFileType file.sample_file_id s) ] [ a [] [ text s ]]) availableTypes)
--                    ]
                select [ onInput (SetFileType file.sample_file_id) ]
                    (List.map makeOption availableTypes)
              else
                text file.sample_file_type.file_type
            ]
        , td [ class "col-md-2" ]
            [ if isEditable then
                button [ class "btn btn-default btn-xs pull-right", onClick (OpenConfirmationDialog "Are you sure you want to remove this file from the sample?" (RemoveFile file.sample_file_id)) ] [ text "Remove" ]
              else
                text ""
            ]
        ]


addFilesDialogConfig : Model -> Bool -> Dialog.Config Msg
addFilesDialogConfig model isBusy =
    let
        content =
            if isBusy then
                spinner
            else
                div [ class "scrollable-half" ]
                    [ FileBrowser.view model.fileBrowser |> Html.map FileBrowserMsg ]

        footer =
            let
                selectedFilepaths =
                    FileBrowser.getSelected model.fileBrowser |> List.map .path
            in
            div []
                [ button [ class "btn btn-default pull-left", onClick CloseAddFilesDialog ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick (AddFiles selectedFilepaths) ] [ text "Select" ]
                ]
    in
    { closeMessage = Just CloseAddFilesDialog
    , containerClass = Just "wide-modal-container"
    , header = Just (h3 [] [ text "Add Files" ])
    , body = Just content
    , footer = Just footer
    }


viewAssemblies : List Assembly -> Html msg
viewAssemblies assemblies =
    let
        count =
            List.length assemblies

        label =
            if count == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text (toString count)
                    ]

        body =
            if count == 0 then
                text "None"
            else
                table [ class "table" ]
                    [ tbody [] (List.map viewAssembly assemblies) ]
    in
    if count == 0 then
        text ""
    else
        div []
            [ h2 []
                [ text "Assemblies "
                , label
                ]
            , body
            ]


viewAssembly : Assembly -> Html msg
viewAssembly assembly =
    tr []
        [ td []
            [ a [ Route.href (Route.Assembly assembly.assembly_id) ] [ text assembly.assembly_name ]
            ]
        ]


viewCombinedAssemblies : List CombinedAssembly -> Html msg
viewCombinedAssemblies assemblies =
    let
        count =
            List.length assemblies

        label =
            if count == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text (toString count)
                    ]

        body =
            if count == 0 then
                text "None"
            else
                table [ class "table" ]
                    [ tbody [] (List.map viewCombinedAssembly assemblies) ]
    in
    if count == 0 then
        text ""
    else
        div []
            [ h2 []
                [ text "Combined Assemblies "
                , label
                ]
            , body
            ]


viewCombinedAssembly : CombinedAssembly -> Html msg
viewCombinedAssembly assembly =
    tr []
        [ td []
            [ a [ Route.href (Route.CombinedAssembly assembly.combined_assembly_id) ] [ text assembly.assembly_name ]
            ]
        ]


attrTableConfig : Bool -> Table.Config Sample.Attribute Msg
attrTableConfig isEditable =
    let
        defaultColumns =
            [ attrTypeColumn
            , attrAliasColumn
            , attrValueColumn
            ]

        columns =
            if isEditable then
                defaultColumns ++ [ attrEditColumn ]
            else
                defaultColumns
    in
    Table.customConfig
        { toId = toString << .sample_attr_id
        , toMsg = SetAttrTableState
        , columns = columns
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Html.Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed" ]


attrTypeColumn : Table.Column Sample.Attribute Msg
attrTypeColumn =
    Table.customColumn
        { name = "Type"
        , viewData = .type_ << .sample_attr_type
        , sorter = Table.increasingOrDecreasingBy (.type_ << .sample_attr_type)
        }


attrAliasColumn : Table.Column Sample.Attribute Msg
attrAliasColumn =
    Table.customColumn
        { name = "Aliases"
        , viewData = aliasesToString << .sample_attr_type_aliases << .sample_attr_type
        , sorter = Table.increasingOrDecreasingBy (.type_ << .sample_attr_type)
        }


aliasesToString : List Sample.AttributeTypeAlias -> String
aliasesToString aliases =
    String.join ", " (List.map .alias_ aliases)


attrValueColumn : Table.Column Sample.Attribute Msg
attrValueColumn =
    Table.customColumn
        { name = "Value"
        , viewData = .attr_value
        , sorter = Table.increasingOrDecreasingBy .attr_value
        }


attrEditColumn : Table.Column Sample.Attribute Msg
attrEditColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = attrEditView
        , sorter = Table.unsortable
        }


attrEditView : Sample.Attribute -> Table.HtmlDetails Msg
attrEditView attr =
    Table.HtmlDetails [ class "col-md-2", style [("text-align","right")] ]
        [ button [ class "btn btn-default btn-xs margin-right", onClick (OpenModifyAttributeDialog attr) ] [ text "Modify" ]
        , button [ class "btn btn-default btn-xs", onClick (OpenConfirmationDialog "Are you sure you to remove this attribute?" (RemoveAttribute attr.sample_attr_id)) ] [ text "Remove" ]
        ]


viewAttributes : Model -> Bool -> Html Msg
viewAttributes model isEditable =
    let
        lowerQuery =
            String.toLower model.attrQuery

        attrFilter attr =
            ( (String.contains lowerQuery (String.toLower attr.attr_value))
                || (String.contains lowerQuery (aliasesToString attr.sample_attr_type.sample_attr_type_aliases |> String.toLower))
                || (String.contains lowerQuery (String.toLower attr.sample_attr_type.type_)) )

        acceptableAttributes =
            List.filter attrFilter model.sample.sample_attrs

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableAttributes

                numStr =
                    count |> toFloat |> format myLocale
            in
            if count == 0 then
                text ""
            else
                span [ class "badge" ] [ text numStr ]

        searchBar =
            if acceptableAttributes == [] then
                text ""
            else
                small [] [ input [ placeholder "Search", onInput SetAttrQuery ] [] ]

        display =
            if acceptableAttributes == [] then
                text "None"
            else
                Table.view (attrTableConfig isEditable) model.attrTableState acceptableAttributes

        addButton =
            if isEditable then
                button [ class "btn btn-default btn-sm", onClick OpenNewAttributeDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Attribute" ]
            else
                text ""
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Attributes "
                , numShowing
                , div [ class "pull-right" ]
                    [ searchBar, text " ", addButton ]
                ]
            , div [ class "scrollable" ] [ display ]
            ]
        ]


newAttributeDialogConfig : Bool -> Dialog.Config Msg
newAttributeDialogConfig isBusy =
    let
        content =
            if isBusy then
                spinner
            else
                Html.form []
                    [ div [ class "form-group" ]
                        [ label [] [ text "Name" ]
                        , input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the type (required)", onInput SetAttributeType ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Aliases" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter aliases as a comma-separated list (optional)", onInput SetAttributeAliases ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Value" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the value (required)", onInput SetAttributeValue ] []
                        ]
                    ]

        footer =
            let
                disable =
                    disabled isBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseNewAttributeDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick CreateNewAttribute, disable ] [ text "Add" ]
                    ]
    in
    { closeMessage = Just CloseNewAttributeDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Add Attribute" ])
    , body = Just content
    , footer = Just footer
    }


editAttributeDialogConfig : Model -> Int -> Bool -> Dialog.Config Msg
editAttributeDialogConfig model attr_id isBusy =
    let
        content =
            if isBusy then
                spinner
            else
                Html.form []
                    [ div [ class "form-group" ]
                        [ label [] [ text "Name" ]
                        , input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the type (required)", value model.attributeType, onInput SetAttributeType ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Aliases" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter aliases as a comma-separated list (optional)", value model.attributeAliases, onInput SetAttributeAliases ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Value" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the value (required)", value model.attributeValue, onInput SetAttributeValue ] []
                        ]
                    ]

        footer =
            let
                disable =
                    disabled isBusy
            in
                div []
                    [ button [ class "btn btn-default pull-left", onClick CloseModifyAttributeDialog, disable ] [ text "Cancel" ]
                    , button [ class "btn btn-primary", onClick (UpdateAttribute attr_id), disable ] [ text "Update" ]
                    ]
    in
    { closeMessage = Just CloseModifyAttributeDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Modify Attribute" ])
    , body = Just content
    , footer = Just footer
    }


pfamTableConfig : Table.Config UProC_PFAM Msg
pfamTableConfig =
    Table.customConfig
        { toId = toString << .sample_to_uproc_id
        , toMsg = SetPFAMTableState
        , columns =
            [ pfamIdColumn
            , pfamCountColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


pfamIdColumn : Table.Column UProC_PFAM Msg
pfamIdColumn =
    Table.veryCustomColumn
        { name = "ID"
        , viewData = pfamIdLink
        , sorter = Table.increasingOrDecreasingBy (.annotation >> .identifier)
        }


pfamIdLink : UProC_PFAM -> Table.HtmlDetails Msg
pfamIdLink protein =
    let
        url =
            "http://pfam.xfam.org/family/" ++ protein.annotation.accession
    in
    Table.HtmlDetails []
        [ a [ href url, target "_blank" ] [ text protein.annotation.identifier ]
        ]


pfamCountColumn : Table.Column UProC_PFAM Msg
pfamCountColumn =
    Table.customColumn
        { name = "Read Count"
        , viewData = toString << .read_count
        , sorter = Table.decreasingOrIncreasingBy .read_count
        }


keggTableConfig : Table.Config UProC_KEGG Msg
keggTableConfig =
    Table.customConfig
        { toId = toString << .uproc_kegg_result_id
        , toMsg = SetKEGGTableState
        , columns =
            [ keggIdColumn
            , keggCountColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


keggIdColumn : Table.Column UProC_KEGG Msg
keggIdColumn =
    Table.veryCustomColumn
        { name = "ID"
        , viewData = keggIdLink
        , sorter = Table.increasingOrDecreasingBy (.annotation >> .name)
        }


keggIdLink : UProC_KEGG -> Table.HtmlDetails Msg
keggIdLink protein =
    let
        url =
            "http://www.genome.jp/dbget-bin/www_bget?ko:" ++ protein.kegg_annotation_id
    in
    Table.HtmlDetails []
        [ a [ href url, target "_blank" ] [ text protein.annotation.name ]
        ]


keggCountColumn : Table.Column UProC_KEGG Msg
keggCountColumn =
    Table.customColumn
        { name = "Read Count"
        , viewData = toString << .read_count
        , sorter = Table.decreasingOrIncreasingBy .read_count
        }


viewProteins : Model -> Html Msg
viewProteins model =
    let
        lowerQuery =
            String.toLower model.proteinQuery

--        proteinFilter protein =
--            ( (String.contains lowerQuery (String.toLower (toString protein.uproc_id)))
--                || (String.contains lowerQuery (String.toLower (toString protein.read_count))) )
--
--        acceptableProteins =
--            case model.proteinFilterType of
--                "PFAM" -> model.proteins.pfam--List.filter proteinFilter model.proteins.pfam
--
--                "KEGG" -> model.proteins.kegg--List.filter proteinFilter model.proteins.kegg

        searchBar =
            if model.proteins.pfam == [] then
                text ""
            else
                small [ class "pull-right" ]
                    [ input [ placeholder "Search", onInput SetProteinQuery ] [] ]

        filterButton label =
            let
                classes =
                    if label == model.proteinFilterType then
                        "btn btn-default active"
                    else
                        "btn btn-default"
            in
            button [ class classes, onClick (FilterProteinType label) ] [ text label ]

        filterBar =
            div [ class "btn-group margin-top-bottom", attribute "role" "group", attribute "aria-label" "..."]
                [ filterButton "PFAM"
                , filterButton "KEGG"
                ]

        (proteinTable, proteinCount) =
            case model.proteinFilterType of
                "PFAM" ->
                    let
                        filter protein =
                            ( (String.contains lowerQuery (String.toLower (toString protein.annotation.identifier)))
                                || (String.contains lowerQuery (String.toLower (toString protein.read_count))) )

                        acceptableProteins =
                            List.filter filter model.proteins.pfam
                    in
                    ( Table.view pfamTableConfig model.pfamTableState acceptableProteins, List.length acceptableProteins )

                "KEGG" ->
                    let
                        filter protein =
                            ( (String.contains lowerQuery (String.toLower (toString protein.annotation.name)))
                                || (String.contains lowerQuery (String.toLower (toString protein.read_count))) )

                        acceptableProteins =
                            List.filter filter model.proteins.kegg
                    in
                ( Table.view keggTableConfig model.keggTableState acceptableProteins, List.length acceptableProteins )

                _ -> (text "", 0)

        body =
            if model.sample.protein_count == 0 then
                text "None"
            else
                if model.loadedProteins then
                    if proteinCount == 0 then
                        text "None"
                    else
                        div []
                            [ filterBar
                            , div [ class "scrollable" ] [ proteinTable ]
                            ]
                else
                    if model.loadingProteins then
                        table [ class "table" ] [ tbody [] [ tr [] [ td [] [ spinner ] ] ] ]
                    else
                        table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetProteins ] [ text "Show Proteins" ] ] ] ] ]

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    if proteinCount == 0 then
                        if model.proteinQuery == "" then
                            model.sample.protein_count
                        else
                            0
                    else
                        proteinCount

                numStr =
                    count |> toFloat |> format myLocale
            in
            if count == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text numStr ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Proteins "
                , numShowing
                , searchBar
                ]
            , body
            ]
        ]


centrifugeTableConfig : Table.Config SampleToCentrifuge Msg
centrifugeTableConfig =
    Table.customConfig
        { toId = toString << .sample_to_centrifuge_id
        , toMsg = SetCentrifugeTableState
        , columns =
            [ nameColumn
            , taxIdColumn
            , numReadsColumn
            , numUniqueReadsColumn
            , abundanceColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


nameColumn : Table.Column SampleToCentrifuge Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy (.name << .centrifuge)
        }


nameLink : SampleToCentrifuge -> Table.HtmlDetails Msg
nameLink result =
    let
        url =
            "https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?mode=Info&id=" ++ (toString result.centrifuge.tax_id)
    in
    Table.HtmlDetails []
        [ a [ href url, target "_blank" ] [ text result.centrifuge.name ]
        ]


taxIdColumn : Table.Column SampleToCentrifuge Msg
taxIdColumn =
    Table.veryCustomColumn
        { name = "NCBI Tax ID"
        , viewData = taxIdLink
        , sorter = Table.increasingOrDecreasingBy (String.toLower << toString << .tax_id << .centrifuge)
        }


taxIdLink : SampleToCentrifuge -> Table.HtmlDetails Msg
taxIdLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.TaxonomySearch (toString result.centrifuge.tax_id)) ]
            [ text (toString result.centrifuge.tax_id) ]
        ]


numReadsColumn : Table.Column SampleToCentrifuge Msg
numReadsColumn =
    Table.customColumn
        { name = "Reads"
        , viewData = toString << .num_reads
        , sorter = Table.increasingOrDecreasingBy (toString << .num_reads)
        }


numUniqueReadsColumn : Table.Column SampleToCentrifuge Msg
numUniqueReadsColumn =
    Table.customColumn
        { name = "Unique Reads"
        , viewData = toString << .num_unique_reads
        , sorter = Table.increasingOrDecreasingBy (toString << .num_unique_reads)
        }


abundanceColumn : Table.Column SampleToCentrifuge Msg
abundanceColumn =
    Table.customColumn
        { name = "Abundance"
        , viewData = toString << .abundance
        , sorter = Table.decreasingOrIncreasingBy (toString << .abundance)
        }


viewCentrifugeResults : Model -> Html Msg
viewCentrifugeResults model =
    let
        lowerQuery =
            String.toLower model.centrifugeQuery

        centrifugeFilter item =
            ( (String.contains lowerQuery (String.toLower item.centrifuge.name))
                || (String.contains lowerQuery (String.toLower (toString item.centrifuge.tax_id)))
                || (String.contains lowerQuery (String.toLower (toString item.num_reads)))
                || (String.contains lowerQuery (String.toLower (toString item.num_unique_reads)))
                || (String.contains lowerQuery (String.toLower (toString item.abundance))) )

        acceptableResults =
            List.filter centrifugeFilter model.centrifugeResults

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    if acceptableResults == [] then
                        if model.centrifugeQuery == "" then
                            model.sample.centrifuge_count
                        else
                            0
                    else
                        List.length acceptableResults

                numStr =
                    count |> toFloat |> format myLocale
            in
            if count == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text numStr ]

        searchBar =
            if model.centrifugeResults == [] then
                text ""
            else
                small [ class "pull-right" ]
                    [ input [ placeholder "Search", onInput SetCentrifugeQuery ] [] ]

        body =
            if model.sample.centrifuge_count == 0 then
                text "None"
            else
                if model.loadedCentrifugeResults then
                    if acceptableResults == [] then
                        text "None"
                    else
                        Table.view centrifugeTableConfig model.centrifugeTableState acceptableResults
                else
                    if model.loadingCentrifugeResults then
                        table [ class "table" ] [ tbody [] [ tr [] [ td [] [ spinner ] ] ] ]
                    else
                        table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetCentrifugeResults ] [ text "Show Results" ] ] ] ] ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Taxonomic Classification "
                , numShowing
                , searchBar
                ]
            , div [ style [("padding-bottom","0.5em")] ] [ text "As determined by ", a [ href "https://ccb.jhu.edu/software/centrifuge/manual.shtml", target "_blank" ] [ text "Centrifuge"] ]
            , div [ class "scrollable" ] [ body ]
            ]
        ]
