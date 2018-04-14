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
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing ((=>))
import View.Cart as Cart
import View.Spinner exposing (spinner)
import Events exposing (onKeyDown)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , searchTerm : String
    , query : String
    , accession : String
    , isSearching : Bool
    , minReadCount : Int
    , proteinFilterType : String
    , tableState : Table.State
    , cart : Cart.Model
    , pfamResults : List PFAMProtein
    , keggResults : List KEGGProtein
    }


init : Session -> String -> Task PageLoadError Model
init session searchTerm =
    doSearch session.token searchTerm
        |> Task.andThen
            (\(pfamResults, keggResults) ->
                Task.succeed
                    { pageTitle = "Protein Search"
                    , searchTerm = searchTerm
                    , query = ""
                    , accession = searchTerm
                    , isSearching = False
                    , minReadCount = 0
                    , proteinFilterType = autoFilterType pfamResults keggResults
                    , tableState = Table.initialSort "Reads"
                    , cart = (Cart.init session.cart Cart.Editable)
                    , pfamResults = pfamResults
                    , keggResults = keggResults
                    }
            )
        |> Task.mapError Error.handleLoadError


doSearch : String -> String -> Task Http.Error ((List PFAMProtein), (List KEGGProtein))
doSearch token searchTerm =
    case searchTerm of
        "" ->
            Task.succeed ([], [])

        _ ->
            Task.map2 (\pfamResults keggResults -> (pfamResults, keggResults))
                (Request.Sample.protein_pfam_search token searchTerm |> Http.toTask)
                (Request.Sample.protein_kegg_search token searchTerm |> Http.toTask)



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | SetQuery String
    | SetAccession String
    | Search
    | SearchKeyDown Int
    | SetReadCountThreshold String
    | FilterProteinType String
    | SetTableState Table.State
    | SetSession Session


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

        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none => NoOp

        SetAccession strValue ->
            { model | accession = strValue } => Cmd.none => NoOp

        Search ->
            { model | isSearching = True } => Route.modifyUrl (Route.ProteinSearch model.accession) => NoOp

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

        searchBar =
            case model.searchTerm of
                "" -> text ""

                _ ->
                    small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]

        filters =
            case model.searchTerm of
                "" -> text ""

                _ ->
                    div [ style [("padding-bottom", "0.5em")] ]
                        [ text "Filter: Read Count >= "
                        , input [ size 8, onInput SetReadCountThreshold ] []
                        ]

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

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                numStr =
                    resultCount |> toFloat |> format myLocale
            in
            case resultCount of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        body =
            case model.isSearching of
                True ->
                    spinner

                False ->
                    case resultCount of
                        0 -> div [] [ text "No results" ]

                        _ -> resultTable
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text model.pageTitle
                , text " "
                , numShowing
                , searchBar
                ]
            , div [ style [("padding-bottom", "0.5em")] ]
                [ text "Protein Name or Accession: "
                , input [ value model.accession, size 10, onInput SetAccession, onKeyDown SearchKeyDown ] []
                , text " "
                , button [ class "btn btn-default btn-xs", onClick Search ] [ text "Search" ]
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
