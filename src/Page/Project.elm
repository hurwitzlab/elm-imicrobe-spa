module Page.Project exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Project exposing (Project, Investigator, Domain, Assembly, CombinedAssembly, Sample, Publication, ProjectGroup)
import Data.Session as Session exposing (Session)
import Data.Cart
import Data.Sample
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
import Route
import Task exposing (Task)
import Table exposing (defaultCustomizations)
import View.Cart as Cart
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
    , showNewSampleDialog : Bool
    , showNewSampleBusy : Bool
    , newSampleName : String
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
                    , showNewSampleDialog = False
                    , showNewSampleBusy = False
                    , newSampleName = ""
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
    | OpenNewSampleDialog
    | CloseNewSampleDialog
    | SetNewSampleName String
    | CreateNewSample
    | CreateNewSampleCompleted (Result Http.Error Data.Sample.Sample)


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

        OpenNewSampleDialog ->
            { model | showNewSampleDialog = True } => Cmd.none => NoOp

        CloseNewSampleDialog ->
            { model | showNewSampleDialog = False } => Cmd.none => NoOp

        SetNewSampleName name ->
            { model | newSampleName = name } => Cmd.none => NoOp

        CreateNewSample ->
            let
                createSample =
                    Request.Sample.create session.token model.newSampleName model.project_id |> Http.toTask
            in
            { model | showNewSampleBusy = True } => Task.attempt CreateNewSampleCompleted createSample => NoOp

        CreateNewSampleCompleted (Ok sample) ->
            model => Route.modifyUrl (Route.Sample sample.sample_id) => NoOp

        CreateNewSampleCompleted (Err error) ->
            model => Cmd.none => NoOp



-- VIEW --


view : Model -> Html Msg
view model =
    let
        privateButton =
            case model.project.private of
                1 ->
                    button [ class "btn btn-default pull-right" ]
                        [ span [ class "glyphicon glyphicon-lock" ] [], text " Project is Private" ]

                _ ->
                    span [] []
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
             else
                Nothing
            )
        ]


viewProject : Project -> Bool -> Html msg
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
                    button [ class "btn btn-default btn-xs" ] [ span [ class "glyphicon glyphicon-cog" ] [], text " Edit" ]

                False ->
                    span [] []
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


viewInvestigators : List Investigator -> Bool -> Html msg
viewInvestigators investigators isEditable =
    let
        numInvs =
            List.length investigators

        label =
            case numInvs of
                0 ->
                    span [] []

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
                        [ tbody [] (List.map viewInvestigator investigators) ]

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm pull-right" ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Investigator" ]

                False ->
                    span [] []
    in
    div []
        [ h2 []
            [ text "Investigators "
            , label
            , addButton
            ]
        , body
        ]


viewInvestigator : Investigator -> Html msg
viewInvestigator investigator =
    tr []
        [ td []
            [ a [ Route.href (Route.Investigator investigator.investigator_id) ]
                [ text investigator.investigator_name ]
            ]
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


viewPublications : List Publication -> Bool -> Html msg
viewPublications pubs isEditable =
    let
        numPubs =
            List.length pubs

        label =
            case numPubs of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numPubs)
                        ]

        body =
            case numPubs of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ] [ tbody [] (List.map viewPublication pubs) ]

        addButton =
            case isEditable of
                True ->
                    button [ class "btn btn-default btn-sm pull-right" ] [ span [ class "glyphicon glyphicon-plus" ] [], text " Add Publication" ]

                False ->
                    span [] []
    in
    div []
        [ h2 []
            [ text "Publications "
            , label
            , addButton
            ]
        , body
        ]


viewPublication : Publication -> Html msg
viewPublication pub =
    tr []
        [ td []
            [ a [ Route.href (Route.Publication pub.publication_id) ]
                [ text pub.title ]
            ]
        ]


viewSamples : Cart.Model -> List Sample -> Bool -> Html Msg
viewSamples cart samples isEditable =
    let
        numSamples =
            List.length samples

        label =
            case numSamples of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numSamples)
                        ]
        cols =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Type" ]
                , th [ class "nowrap" ] [ Cart.addAllToCartButton cart (List.map .sample_id samples) |> Html.map CartMsg ]
                ]

        rows =
            List.map (viewSample cart) samples

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
                    span [] []
    in
    div []
        [ h2 []
            [ text "Samples "
            , label
            , addButton
            ]
        , body
        ]


viewSample : Cart.Model -> Sample -> Html Msg
viewSample cart sample =
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        , td [ class "col-md-1" ] [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg ]
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
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        searchBar =
            case model.assemblies of
                [] ->
                    span [] []

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
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ] ] ] ] ]

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
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        searchBar =
            case model.combined_assemblies of
                [] ->
                    span [] []

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
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ] ] ] ] ]

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


newSampleDialogConfig : Model -> Dialog.Config Msg
newSampleDialogConfig model =
    let
        content =
            case model.showNewSampleBusy of
                False ->
                    input [ class "form-control", type_ "text", size 20, placeholder "Enter the name of the new sample", onInput SetNewSampleName ] []

                True ->
                    div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ]

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