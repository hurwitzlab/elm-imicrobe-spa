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
    , showNewAttributeDialog : Bool
    , showNewAttributeBusy : Bool
    , newAttributeType : String
    , newAttributeAliases : String
    , newAttributeValue : String
    , showModifyAttributeDialog : Bool
    , showModifyAttributeBusy : Bool
    , attributeToModify : Maybe Sample.Attribute
    , confirmationDialog : Maybe (Dialog.Config Msg)
    , infoDialog : Maybe (Dialog.Config Msg)
    , showAddFilesDialog : Bool
    , showEditInfoDialog : Bool
    , newSampleName : String
    , newSampleCode : String
    , newSampleType : String
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

        isEditable sample =
            case session.user of
                Nothing ->
                    False

                Just user ->
                    List.map .user_name sample.users |> List.member user.user_name
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
                    , showNewAttributeDialog = False
                    , showNewAttributeBusy = False
                    , newAttributeType = ""
                    , newAttributeAliases = ""
                    , newAttributeValue = ""
                    , showModifyAttributeDialog = False
                    , showModifyAttributeBusy = False
                    , attributeToModify = Nothing
                    , confirmationDialog = Nothing
                    , infoDialog = Nothing
                    , showAddFilesDialog = False
                    , showEditInfoDialog = False
                    , newSampleName = ""
                    , newSampleCode = ""
                    , newSampleType = ""
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
    | SetNewAttributeType String
    | SetNewAttributeAliases String
    | SetNewAttributeValue String
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
    | SetNewSampleName String
    | SetNewSampleCode String
    | SetNewSampleType String
    | UpdateSampleInfo
    | UpdateSampleInfoCompleted (Result Http.Error Sample)
    | AddFiles (List String)
    | AddFilesCompleted (Result Http.Error Sample)
    | RemoveFile Int
    | RemoveFileCompleted (Result Http.Error Sample)
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
            { model | showNewAttributeDialog = True, showNewAttributeBusy = False } => Cmd.none => NoOp

        CloseNewAttributeDialog ->
            { model | showNewAttributeDialog = False } => Cmd.none => NoOp

        CreateNewAttribute ->
            let
                createAttribute =
                    Request.Sample.addAttribute session.token model.sample_id model.newAttributeType model.newAttributeAliases model.newAttributeValue |> Http.toTask
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

        SetNewAttributeType val ->
            { model | newAttributeType = val } => Cmd.none => NoOp

        SetNewAttributeAliases val ->
            { model | newAttributeAliases = val } => Cmd.none => NoOp

        SetNewAttributeValue val ->
            { model | newAttributeValue = val } => Cmd.none => NoOp

        OpenModifyAttributeDialog attr ->
            { model
                | attributeToModify = Just attr
                , showModifyAttributeBusy = False
                , newAttributeType = attr.sample_attr_type.type_
                , newAttributeAliases = aliasesToString attr.sample_attr_type.sample_attr_type_aliases
                , newAttributeValue = attr.attr_value
            } => Cmd.none => NoOp

        CloseModifyAttributeDialog ->
            { model | attributeToModify = Nothing } => Cmd.none => NoOp

        UpdateAttribute attr_id ->
            let
                updateAttribute =
                    Request.Sample.updateAttribute session.token model.sample_id attr_id model.newAttributeType model.newAttributeAliases model.newAttributeValue |> Http.toTask
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
                _ = Debug.log "error" (toString error) -- TODO show to user
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
            model => Cmd.none => NoOp

        OpenEditInfoDialog ->
            { model
                | showEditInfoDialog = True
                , newSampleName = model.sample.sample_name
                , newSampleCode = model.sample.sample_acc
                , newSampleType = model.sample.sample_type
             } => Cmd.none => NoOp

        CloseEditInfoDialog ->
            { model | showEditInfoDialog = False } => Cmd.none => NoOp

        SetNewSampleName val ->
            { model | newSampleName = val } => Cmd.none => NoOp

        SetNewSampleCode val ->
            { model | newSampleCode = val } => Cmd.none => NoOp

        SetNewSampleType val ->
            { model | newSampleType = val } => Cmd.none => NoOp

        UpdateSampleInfo ->
            let
                updateInfo =
                    Request.Sample.update session.token model.sample_id model.newSampleName model.newSampleCode model.newSampleType |> Http.toTask
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
        privateButton =
            if model.sample.private == 1 then
                let
                    infoText =
                        "This feature is currently under development.  Soon you will be able to 'publish' your sample (making it publicly accessible) or share with collaborators."
                in
                button [ class "btn btn-default", onClick (OpenInfoDialog infoText) ]
                    [ span [ class "glyphicon glyphicon-lock" ] [], text " Sample is Private" ]
            else
                text ""

        showMapButton =
            let
                attrExists name =
                    List.Extra.find (\attr -> attr.sample_attr_type.type_ == name) model.sample.sample_attrs |> Maybe.Extra.isJust
            in
            attrExists "latitude" && attrExists "longitude"
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.sample.sample_name ]
                    , div [ class "pull-right" ]
                        [ privateButton
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


viewSample : Sample -> Bool -> Html Msg
viewSample sample isEditable =
    let
        numFiles =
            List.length sample.sample_files

        ontologies =
            case sample.ontologies of
                [] ->
                    "none"

                _ ->
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
--        , tr []
--            [ th [] [ text "Ontologies" ]
--            , td [] [ text ontologies ]
--            ]
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
            case showMap of
                True ->
                    style [ ( "display", "block") ]

                False ->
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
                        , input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the name (required)", value model.newSampleName, onInput SetNewSampleName ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Sample Code" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the code (required)", value model.newSampleCode, onInput SetNewSampleCode ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Sample Type" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the type (required)", value model.newSampleType, onInput SetNewSampleType ] []
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
            case numFiles of
                0 ->
                    text ""

                _ ->
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
            case numFiles of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (cols :: (List.map (viewFile isEditable) files)) ]

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm pull-right", onClick OpenAddFilesDialog ]
                        [ span [ class "glyphicon glyphicon-plus" ] [], text " Add File(s)"
                        ]

                False ->
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
    tr []
        [ td []
            [ a [ href (dataCommonsUrl ++ file.file), target "_blank" ] [ text file.file ]
            ]
        , td []
            [ text file.sample_file_type.file_type
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
                 FileBrowser.view model.fileBrowser |> Html.map FileBrowserMsg

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
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text (toString count)
                        ]

        body =
            case count of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        [ tbody [] (List.map viewAssembly assemblies) ]
    in
    case count of
        0 -> text ""

        _ ->
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
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text (toString count)
                        ]

        body =
            case count of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        [ tbody [] (List.map viewCombinedAssembly assemblies) ]
    in
    case count of
        0 -> text ""

        _ ->
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
        columns =
            case isEditable of
                False ->
                    [ attrTypeColumn
                    , attrAliasColumn
                    , attrValueColumn
                    ]

                True ->
                    [ attrTypeColumn
                    , attrAliasColumn
                    , attrValueColumn
                    , attrEditColumn
                    ]
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
        [ button [ class "btn btn-default btn-xs", onClick (OpenModifyAttributeDialog attr) ] [ text "Modify" ]
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
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ] [ text numStr ]

        searchBar =
            case acceptableAttributes of
                [] ->
                    text ""

                _ ->
                    small [] [ input [ placeholder "Search", onInput SetAttrQuery ] [] ]

        display =
            case acceptableAttributes of
                [] ->
                    text "None"

                _ ->
                    Table.view (attrTableConfig isEditable) model.attrTableState acceptableAttributes

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm", onClick OpenNewAttributeDialog ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Attribute" ]

                False ->
                    text ""
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Attributes "
                , numShowing
                , div [ class "pull-right" ]
                    [ addButton, searchBar ]
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
                        , input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the type (required)", onInput SetNewAttributeType ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Aliases" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter aliases as a comma-separated list (optional)", onInput SetNewAttributeAliases ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Value" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the value (required)", onInput SetNewAttributeValue ] []
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
                        , input [ class "form-control", type_ "text", size 20, autofocus True, placeholder "Enter the type (required)", value model.newAttributeType, onInput SetNewAttributeType ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Aliases" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter aliases as a comma-separated list (optional)", value model.newAttributeAliases, onInput SetNewAttributeAliases ] []
                        ]
                    , div [ class "form-group" ]
                        [ label [] [ text "Value" ]
                        , input [ class "form-control", type_ "text", size 20, placeholder "Enter the value (required)", value model.newAttributeValue, onInput SetNewAttributeValue ] []
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
            case model.proteins.pfam of
                [] ->
                    text ""

                _ ->
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
            case model.sample.protein_count of
                0 ->
                    text "None"

                _ ->
                    case model.loadedProteins of
                        True ->
                            case proteinCount of
                                0 ->
                                    text "None"

                                _ ->
                                    div []
                                        [ filterBar
                                        , div [ class "scrollable" ] [ proteinTable ]
                                        ]

                        False ->
                            case model.loadingProteins of
                                True ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ spinner ] ] ] ]

                                False ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetProteins ] [ text "Show Proteins" ] ] ] ] ]

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    case proteinCount of
                        0 ->
                            case model.proteinQuery of
                                 "" ->
                                    model.sample.protein_count

                                 _ ->
                                    0

                        _ ->
                            proteinCount

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    text ""

                _ ->
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
                    case acceptableResults of
                        [] ->
                            case model.centrifugeQuery of
                                 "" ->
                                    model.sample.centrifuge_count

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
            case model.centrifugeResults of
                [] ->
                    text ""

                _ ->
                    small [ class "pull-right" ]
                        [ input [ placeholder "Search", onInput SetCentrifugeQuery ] [] ]

        body =
            case model.sample.centrifuge_count of
                0 ->
                    text "None"

                _ ->
                    case model.loadedCentrifugeResults of
                        True ->
                            case acceptableResults of
                                [] ->
                                    text "None"

                                _ ->
                                    Table.view centrifugeTableConfig model.centrifugeTableState acceptableResults

                        False ->
                            case model.loadingCentrifugeResults of
                                True ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ spinner ] ] ] ]

                                False ->
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
