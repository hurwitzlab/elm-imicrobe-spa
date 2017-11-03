module Page.Sample exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (..)
import Data.Session as Session exposing (Session)
import Data.Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Sample
import Route
import Task exposing (Task)
import Config exposing (dataCommonsUrl)
import Table exposing (defaultCustomizations)
import View.Cart as Cart
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , sample_id : Int
    , sample : Sample
    , cart : Cart.Model
    , loadingProteins : Bool
    , loadedProteins : Bool
    , proteins : List SampleUProC
    , loadingCentrifugeResults : Bool
    , loadedCentrifugeResults : Bool
    , centrifugeResults : List SampleToCentrifuge
    , attrTableState : Table.State
    , proteinTableState : Table.State
    , centrifugeTableState : Table.State
    , attrQuery : String
    , proteinQuery : String
    , centrifugeQuery : String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        loadSample =
            Request.Sample.get id |> Http.toTask
    in
    loadSample
        |> Task.andThen
            (\sample ->
                Task.succeed
                    { pageTitle = "Sample"
                    , sample_id = id
                    , sample = sample
                    , cart = Cart.init session.cart Cart.Editable
                    , loadingProteins = False
                    , loadedProteins = False
                    , proteins = []
                    , loadingCentrifugeResults = False
                    , loadedCentrifugeResults = False
                    , centrifugeResults = []
                    , attrTableState = Table.initialSort "Name"
                    , proteinTableState = Table.initialSort "Read Count"
                    , centrifugeTableState = Table.initialSort "Abundance"
                    , attrQuery = ""
                    , proteinQuery = ""
                    , centrifugeQuery = ""
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetAttrQuery String
    | SetProteinQuery String
    | SetCentrifugeQuery String
    | SetAttrTableState Table.State
    | SetProteinTableState Table.State
    | SetCentrifugeTableState Table.State
    | GetProteins
    | SetProteins (List SampleUProC)
    | GetCentrifugeResults
    | SetCentrifugeResults (List SampleToCentrifuge)
    | CartMsg Cart.Msg


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        SetAttrQuery newQuery ->
            { model | attrQuery = newQuery } => Cmd.none => NoOp

        SetProteinQuery newQuery ->
            { model | proteinQuery = newQuery } => Cmd.none => NoOp

        SetCentrifugeQuery newQuery ->
            { model | centrifugeQuery = newQuery } => Cmd.none => NoOp

        SetAttrTableState newState ->
            { model | attrTableState = newState } => Cmd.none => NoOp

        SetProteinTableState newState ->
            { model | proteinTableState = newState } => Cmd.none => NoOp

        SetCentrifugeTableState newState ->
            { model | centrifugeTableState = newState } => Cmd.none => NoOp

        GetProteins ->
            let
                loadProteins =
                    Request.Sample.proteins model.sample_id |> Http.toTask

                handleProteins proteins =
                    case proteins of
                        Ok proteins ->
                            SetProteins proteins

                        Err error ->
                            let
                                _ = Debug.log "Error" ("could not retrieve proteins: " ++ (toString error))
                            in
                            SetProteins []
            in
            { model | loadingProteins = True } => Task.attempt handleProteins loadProteins => NoOp

        SetProteins proteins ->
            { model | loadedProteins = True, proteins = proteins } => Cmd.none => NoOp

        GetCentrifugeResults ->
            let
                loadCentrifugeResults =
                    Request.Sample.centrifuge_results model.sample_id |> Http.toTask

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

        CartMsg subMsg ->
            let
                _ = Debug.log "Sample.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.sample.sample_name ]
                    , div [ class "pull-right" ]
                        [ Cart.addToCartButton2 model.cart model.sample.sample_id |> Html.map CartMsg ]
                    ]
                ]
            , viewSample model.sample
            , viewFiles model.sample.sample_files
            , viewAssemblies model.sample.assemblies
            , viewCombinedAssemblies model.sample.combined_assemblies
            , viewOntologies model.sample.ontologies
            , viewAttributes model
--            , viewProteins model
            , viewCentrifugeResults model
            ]
        ]


viewSample : Sample -> Html msg
viewSample sample =
    let
        numFiles =
            List.length sample.sample_files

        numOntologies =
            List.length sample.ontologies
    in
    table [ class "table" ]
        [ tr []
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
        ]


viewFiles : List SampleFile2 -> Html msg
viewFiles files =
    let
        numFiles =
            List.length files

        label =
            case numFiles of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numFiles)
                        ]

        cols =
            tr []
                [ th [] [ text "Path" ]
                , th [] [ text "Type" ]
                ]

        body =
            case numFiles of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (cols :: (List.map viewFile files)) ]
    in
    div []
        [ h2 []
            [ text "Files "
            , label
            ]
        , body
        ]


viewFile : SampleFile2 -> Html msg
viewFile file =
    tr []
        [ td []
            [ a [ href (dataCommonsUrl ++ file.file) ] [ text file.file ]
            ]
        , td []
            [ text file.sample_file_type.file_type
            ]
        ]


viewAssemblies : List Assembly -> Html msg
viewAssemblies assemblies =
    let
        count =
            List.length assemblies

        label =
            case count of
                0 ->
                    span [] []

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
                        (List.map viewAssembly assemblies)
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
                    span [] []

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
                        (List.map viewCombinedAssembly assemblies)
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


viewOntologies : List Ontology -> Html msg
viewOntologies ontologies =
    let
        numOntologies =
            List.length ontologies

        label =
            case numOntologies of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numOntologies)
                        ]

        body =
            case numOntologies of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (List.map viewOntology ontologies) ]
    in
    div []
        [ h2 []
            [ text "Ontologies "
            , label
            ]
        , body
        ]


viewOntology : Ontology -> Html msg
viewOntology ont =
    let
        display =
            ont.ontology_acc
                ++ (case ont.label of
                        "" ->
                            ""

                        _ ->
                            " (" ++ ont.label ++ ")"
                   )
    in
    tr []
        [ td []
            [ text display
            ]
        ]


attrTableConfig : Table.Config Sample.Attribute Msg
attrTableConfig =
    Table.customConfig
        { toId = toString << .sample_attr_id
        , toMsg = SetAttrTableState
        , columns =
            [ typeColumn
            , aliasColumn
            , valueColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Html.Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed"
    ]


typeColumn : Table.Column Sample.Attribute Msg
typeColumn =
    Table.customColumn
        { name = "Type"
        , viewData = .type_ << .sample_attr_type
        , sorter = Table.increasingOrDecreasingBy (.type_ << .sample_attr_type)
        }


aliasColumn : Table.Column Sample.Attribute Msg
aliasColumn =
    Table.customColumn
        { name = "Aliases"
        , viewData = aliasesToString << .sample_attr_type_aliases << .sample_attr_type
        , sorter = Table.increasingOrDecreasingBy (.type_ << .sample_attr_type)
        }


aliasesToString : List Sample.AttributeTypeAlias -> String
aliasesToString aliases =
    String.join ", " (List.map .alias_ aliases)


valueColumn : Table.Column Sample.Attribute Msg
valueColumn =
    Table.customColumn
        { name = "Value"
        , viewData = .attr_value
        , sorter = Table.increasingOrDecreasingBy .attr_value
        }


viewAttributes : Model -> Html Msg
viewAttributes model =
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
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]
        display =
            case acceptableAttributes of
                [] ->
                    text "None"

                _ ->
                    Table.view attrTableConfig model.attrTableState acceptableAttributes

    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Attributes "
                , numShowing
                , small [ class "right" ]
                    [ input [ placeholder "Search", onInput SetAttrQuery ] [] ]
                ]
            , display
            ]
        ]


proteinTableConfig : Table.Config SampleUProC Msg
proteinTableConfig =
    Table.customConfig
        { toId = toString << .sample_uproc_id
        , toMsg = SetProteinTableState
        , columns =
            [ uprocIdColumn
            , countColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


uprocIdColumn : Table.Column SampleUProC Msg
uprocIdColumn =
    Table.veryCustomColumn
        { name = "ID"
        , viewData = uprocIdLink
        , sorter = Table.increasingOrDecreasingBy (String.toLower << .uproc_id)
        }


uprocIdLink : SampleUProC -> Table.HtmlDetails Msg
uprocIdLink protein =
    let
        url =
            "http://pfam.xfam.org/family/" ++ protein.uproc_id
    in
    Table.HtmlDetails []
        [ a [ href url ] [ text protein.uproc_id ]
        ]


countColumn : Table.Column SampleUProC Msg
countColumn =
    Table.veryCustomColumn
        { name = "Read Count"
        , viewData = viewReadCount
        , sorter = Table.decreasingOrIncreasingBy .count
        }


viewReadCount : SampleUProC -> Table.HtmlDetails Msg
viewReadCount protein =
    Table.HtmlDetails []
        [ text (toString protein.count) ]


viewProteins : Model -> Html Msg
viewProteins model =
    let
        lowerQuery =
            String.toLower model.proteinQuery

        proteinFilter protein =
            ( (String.contains lowerQuery (String.toLower protein.uproc_id))
                || (String.contains lowerQuery (String.toLower (toString protein.count))) )

        acceptableProteins =
            List.filter proteinFilter model.proteins

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    case acceptableProteins of
                        [] ->
                            case model.proteinQuery of
                                 "" ->
                                    model.sample.protein_count

                                 _ ->
                                    0

                        _ ->
                            List.length acceptableProteins

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
            case model.proteins of
                [] ->
                    span [] []

                _ ->
                    small [ class "right" ]
                        [ input [ placeholder "Search", onInput SetProteinQuery ] [] ]

        body =
            case model.sample.protein_count of
                0 ->
                    text "None"

                _ ->
                    case model.loadedProteins of
                        True ->
                            case acceptableProteins of
                                [] ->
                                    text "None"

                                _ ->
                                    Table.view proteinTableConfig model.proteinTableState acceptableProteins

                        False ->
                            case model.loadingProteins of
                                True ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ] ] ] ] ]

                                False ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetProteins ] [ text "Show Proteins" ] ] ] ] ]
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
        [ a [ href url ] [ text (toString result.centrifuge.name) ]
        ]


taxIdColumn : Table.Column SampleToCentrifuge Msg
taxIdColumn =
    Table.veryCustomColumn
        { name = "Tax ID"
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
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        searchBar =
            case model.centrifugeResults of
                [] ->
                    span [] []

                _ ->
                    small [ class "right" ]
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
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ] ] ] ] ]

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
            , div [ style [("padding-bottom","0.5em")] ] [ text "As determined by ", a [ href "https://ccb.jhu.edu/software/centrifuge/manual.shtml" ] [ text "Centrifuge"] ]
            , body
            ]
        ]
