module Page.ProteinSearch exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (Sample, Project, PFAMProtein, PFAMResult, KEGGProtein, KEGGResult)
import Data.Session as Session exposing (Session)
import Data.Cart
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Page.Error as Error exposing (PageLoadError)
import Route
import Request.Sample
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing ((=>))
import View.Cart as Cart
import View.Spinner exposing (spinner)
import View.Widgets
import Events exposing (onKeyDown)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , pageLoaded : Bool
    , resultsLoaded : Bool
    , searchTerm : String
    , query : String
    , accession : String
    , minReadCount : Int
    , proteinFilterType : String
    , tableState : Table.State
    , cart : Cart.Model
    , pfamResults : List PFAMProtein
    , keggResults : List KEGGProtein
    }


init : Session -> String -> Task PageLoadError Model
init session searchTerm =
    let
        noSearch =
            searchTerm == ""
    in
    Task.succeed
        { pageTitle = "Protein Search"
        , pageLoaded = noSearch
        , resultsLoaded = noSearch
        , searchTerm = searchTerm
        , query = ""
        , accession = searchTerm
        , minReadCount = 0
        , proteinFilterType = ""
        , tableState = Table.initialSort "Reads"
        , cart = (Cart.init session.cart Cart.Editable)
        , pfamResults = []
        , keggResults = []
        }



-- UPDATE --


type Msg
    = SetQuery String
    | SetAccession String
    | DelayedInit
    | InitCompleted (Result PageLoadError ((List PFAMProtein), (List KEGGProtein)))
    | Search
    | SearchKeyDown Int
    | SetReadCountThreshold String
    | FilterProteinType String
    | SetTableState Table.State
    | SetSession Session
    | CartMsg Cart.Msg


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        DelayedInit ->
            if model.pageLoaded then
                model => Cmd.none => NoOp
            else
                let
                    search =
                        Task.map2 (\pfamResults keggResults -> (pfamResults, keggResults))
                            (Request.Sample.protein_pfam_search session.token model.searchTerm |> Http.toTask)
                            (Request.Sample.protein_kegg_search session.token model.searchTerm |> Http.toTask)
                                |> Task.mapError Error.handleLoadError
                in
                { model | pageLoaded = True } => Task.attempt InitCompleted search => NoOp

        InitCompleted (Ok (pfamResults, keggResults)) ->
            { model | resultsLoaded = True, pfamResults = pfamResults, keggResults = keggResults, proteinFilterType = autoFilterType pfamResults keggResults } => Cmd.none => NoOp

        InitCompleted (Err error) ->
            model => Cmd.none => NoOp -- TODO

        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none => NoOp

        SetAccession strValue ->
            { model | accession = strValue } => Cmd.none => NoOp

        Search ->
            model => Route.modifyUrl (Route.ProteinSearch model.accession) => NoOp

        SearchKeyDown key ->
            if key == 13 then -- enter key
                update session Search model
            else
                model => Cmd.none => NoOp

        SetReadCountThreshold strValue ->
            let
                threshold =
                    case String.toInt strValue of
                        Ok value -> value

                        Err _ -> 0
            in
            { model | minReadCount = threshold } => Cmd.none => NoOp

        FilterProteinType filterType ->
            { model | proteinFilterType = filterType } => Cmd.none => NoOp

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none => NoOp

        SetSession newSession ->
            let
                _ = Debug.log "Page.TaxonomySearch.SetSession" (toString newSession)

                newCart =
                    Cart.init newSession.cart Cart.Editable

                (subModel, cmd) =
                    Cart.update newSession (Cart.SetSession newSession) model.cart
            in
            { model | cart = newCart } => Cmd.none => NoOp

        CartMsg subMsg ->
            let
                _ = Debug.log "Samples.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart


autoFilterType : List PFAMProtein -> List KEGGProtein -> String
autoFilterType pfamResults keggResults =
    if pfamResults /= [] || keggResults == [] then
        "PFAM"
    else
        "KEGG"



-- VIEW --


view : Model -> Html Msg
view model =
    let
        lowerQuery =
            String.toLower model.query

        (resultTable, resultCount) =
            case model.proteinFilterType of
                "PFAM" ->
                    let
                        filter result =
                            ( (String.contains lowerQuery (String.toLower (toString result.sample.sample_name)))
                                || (String.contains lowerQuery (String.toLower (toString result.sample.project.project_name)))
                                || (String.contains lowerQuery (String.toLower (toString result.read_count))) )
                                && result.read_count >= model.minReadCount

                        results =
                            case List.head model.pfamResults of
                                Nothing -> []

                                Just result -> result.uproc_pfam_results

                        acceptableResults =
                            List.filter filter results
                    in
                    ( Table.view (pfamTableConfig model.cart) model.tableState acceptableResults, List.length acceptableResults )

                "KEGG" ->
                    let
                        filter result =
                            ( (String.contains lowerQuery (String.toLower (toString result.sample.sample_name)))
                                || (String.contains lowerQuery (String.toLower (toString result.sample.project.project_name)))
                                || (String.contains lowerQuery (String.toLower (toString result.read_count))) )
                                && result.read_count >= model.minReadCount

                        results =
                            case List.head model.keggResults of
                                Nothing -> []

                                Just result -> result.uproc_kegg_results

                        acceptableResults =
                            List.filter filter results
                    in
                    ( Table.view (keggTableConfig model.cart) model.tableState acceptableResults, List.length acceptableResults )

                _ -> (text "", 0)

        combinedResultCount =
            (List.length model.pfamResults) + (List.length model.keggResults)

        searchBar =
            if model.resultsLoaded && combinedResultCount > 0 then
                small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]
            else
                text ""

        filters =
            if model.resultsLoaded && combinedResultCount > 0 then
                div [ style [("padding-bottom", "0.5em")] ]
                    [ text "Filter: Read Count >= "
                    , input [ size 8, onInput SetReadCountThreshold ] []
                    ]
            else
                text ""

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
            if model.resultsLoaded && combinedResultCount > 0 then
                div [ class "btn-group margin-top-bottom", attribute "role" "group", attribute "aria-label" "..."]
                    [ filterButton "PFAM"
                    , filterButton "KEGG"
                    ]
            else
                text ""

        body =
            if model.resultsLoaded then
                if model.searchTerm == "" then
                    text ""
                else
                    if resultCount == 0 then
                        div [] [ text "No results" ]
                    else
                        resultTable
            else
                spinner
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text model.pageTitle
                , text " "
                , View.Widgets.counter resultCount
                , searchBar
                ]
            , div [ style [("padding-bottom", "0.5em")] ]
                [ text "Protein Name or Accession: "
                , input [ value model.accession, size 20, onInput SetAccession, onKeyDown SearchKeyDown ] []
                , text " "
                , button [ class "btn btn-default btn-xs", onClick Search ] [ text "Search" ]
                , text " "
                , span [ class "gray", style [("padding-left", "1.5em")] ]
                    [ text " Examples: "
                    , a [ href "#/protein_search/nifz" ] [ text "nfiz" ]
                    , text ", "
                    , a [ href "#/protein_search/K02584" ] [ text "K02584" ]
                    ]
                ]
            , filters
            , filterBar
            , body
            ]
        ]


pfamTableConfig : Cart.Model -> Table.Config PFAMResult Msg
pfamTableConfig cart =
    Table.customConfig
        { toId = toString << .sample_to_uproc_id
        , toMsg = SetTableState
        , columns =
            [ pfam_projectColumn
            , pfam_nameColumn
            , pfam_numReadsColumn
            , pfam_addToCartColumn cart
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


keggTableConfig : Cart.Model -> Table.Config KEGGResult Msg
keggTableConfig cart =
    Table.customConfig
        { toId = toString << .uproc_kegg_result_id
        , toMsg = SetTableState
        , columns =
            [ kegg_projectColumn
            , kegg_nameColumn
            , kegg_numReadsColumn
            , kegg_addToCartColumn cart
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed"
    ]


pfam_nameColumn : Table.Column PFAMResult Msg
pfam_nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = pfam_nameLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .sample_name)
        }


pfam_nameLink : PFAMResult -> Table.HtmlDetails Msg
pfam_nameLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample result.sample.sample_id) ] [ text result.sample.sample_name ]
        ]


pfam_projectColumn : Table.Column PFAMResult Msg
pfam_projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = pfam_projectLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .project >> .project_name)
        }


pfam_projectLink : PFAMResult -> Table.HtmlDetails Msg
pfam_projectLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project result.sample.project_id) ] [ text result.sample.project.project_name ]
        ]


pfam_numReadsColumn : Table.Column PFAMResult Msg
pfam_numReadsColumn =
    Table.veryCustomColumn
        { name = "Reads"
        , viewData = nowrapColumn 4 << toString << .read_count
        , sorter = Table.decreasingOrIncreasingBy .read_count
        }


nowrapColumn : Int -> String -> Table.HtmlDetails Msg
nowrapColumn width value =
    let
        widthStr =
            (toString width) ++ "em"
    in
    Table.HtmlDetails [ style [("min-width",widthStr), ("text-align","right")] ] -- min-width is to prevent column header from wrapping
        [ text value ]


pfam_addToCartColumn : Cart.Model -> Table.Column PFAMResult Msg
pfam_addToCartColumn cart =
    Table.veryCustomColumn
        { name = "Cart"
        , viewData = (\result -> addToCartButton cart result.sample)
        , sorter = Table.unsortable
        }


kegg_nameColumn : Table.Column KEGGResult Msg
kegg_nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = kegg_nameLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .sample_name)
        }


kegg_nameLink : KEGGResult -> Table.HtmlDetails Msg
kegg_nameLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample result.sample.sample_id) ] [ text result.sample.sample_name ]
        ]


kegg_projectColumn : Table.Column KEGGResult Msg
kegg_projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = kegg_projectLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .project >> .project_name)
        }


kegg_projectLink : KEGGResult -> Table.HtmlDetails Msg
kegg_projectLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project result.sample.project_id) ] [ text result.sample.project.project_name ]
        ]


kegg_numReadsColumn : Table.Column KEGGResult Msg
kegg_numReadsColumn =
    Table.veryCustomColumn
        { name = "Reads"
        , viewData = nowrapColumn 4 << toString << .read_count
        , sorter = Table.decreasingOrIncreasingBy .read_count
        }


kegg_addToCartColumn : Cart.Model -> Table.Column KEGGResult Msg
kegg_addToCartColumn cart =
    Table.veryCustomColumn
        { name = "Cart"
        , viewData = (\result -> addToCartButton cart result.sample)
        , sorter = Table.unsortable
        }

addToCartButton : Cart.Model -> Sample -> Table.HtmlDetails Msg
addToCartButton cart sample =
    Table.HtmlDetails []
        [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg
        ]
