module Page.ProteinSearch exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (Sample, PFAMProtein, PFAMResult)
import Data.Session as Session exposing (Session)
import Data.Cart
import Http
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Page.Error as Error exposing (PageLoadError)
import Route
import Request.Sample
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing ((=>))
import View.Cart as Cart



---- MODEL ----


type alias Model =
    { pageTitle : String
    , searchTerm : String
    , query : String
    , accession : String
    , minReadCount : Float
    , tableState : Table.State
    , cart : Cart.Model
    , results : List PFAMProtein
    }


init : Session -> String -> Task PageLoadError Model
init session searchTerm =
    doSearch searchTerm
        |> Task.andThen
            (\results ->
                Task.succeed
                    { pageTitle = "Protein Search"
                    , searchTerm = searchTerm
                    , query = ""
                    , accession = searchTerm
                    , minReadCount = 0
                    , tableState = Table.initialSort "Reads"
                    , cart = (Cart.init session.cart Cart.Editable)
                    , results = results
                    }
            )
        |> Task.mapError Error.handleLoadError


doSearch : String -> Task Http.Error (List PFAMProtein)
doSearch searchTerm =
    case searchTerm of
        "" ->
            Task.succeed []

        _ ->
            Request.Sample.protein_search searchTerm |> Http.toTask



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | SetQuery String
    | SetAccession String
    | Search
    | SetResults String (List PFAMProtein)
--    | SetAbundanceThreshold String
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
           let
                handleResults searchTerm results =
                    case results of
                        Ok results ->
                            SetResults searchTerm results

                        Err _ ->
                            let
                                _ = Debug.log "Error" "could not retrieve search results"
                            in
                            SetResults searchTerm []
            in
            model => Task.attempt (handleResults model.accession) (doSearch model.accession) => NoOp

        SetResults searchTerm results ->
                { model | searchTerm = searchTerm, results = results } => Route.modifyUrl (Route.ProteinSearch searchTerm) => NoOp

--        SetAbundanceThreshold strValue ->
--            let
--                threshold =
--                    case String.toFloat strValue of
--                        Ok value -> value
--
--                        Err _ -> 0
--            in
--            { model | minAbundance = threshold } => Cmd.none => NoOp

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



-- VIEW --


view : Model -> Html Msg
view model =
    let
        results =
            case List.head model.results of
                Nothing -> []

                Just result -> result.uproc_pfam_results
--
--        lowerQuery =
--            String.toLower model.query
--
--        filter sample =
--            ( String.contains lowerQuery (String.toLower sample.sample_name)
--                || String.contains lowerQuery (String.toLower sample.project.project_name)
--                || String.contains lowerQuery (toString sample.sample_to_centrifuge.abundance) )
--              && sample.sample_to_centrifuge.abundance >= model.minAbundance
--
--        acceptableSamples =
--            List.filter filter samples
--
        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length results

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
            case model.searchTerm of
                "" -> text ""

                _ ->
                    small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]

--        filters =
--            case model.searchTerm of
--                "" -> text ""
--
--                _ ->
--                    div [ style [("padding-bottom", "0.5em")] ]
--                        [ text "Filter: Abundance >= "
--                        , input [ placeholder "0", size 4, onInput SetAbundanceThreshold ] []
--                        ]
--

        display =
            case model.searchTerm of
                "" -> text ""

                _ ->
                    case results of
                        [] ->
                            text "No results"

                        _ ->
                            Table.view (tableConfig model.cart) model.tableState results
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
                [ text "PFAM/KEGG Accession "
                , input [ value model.accession, size 10, onInput SetAccession ] []
                , text " "
                , button [ class "btn btn-default btn-xs", onClick Search ] [ text "Search" ]
                ]
--            , filters
            , display
            ]
        ]


tableConfig : Cart.Model -> Table.Config PFAMResult Msg
tableConfig cart =
    Table.customConfig
        { toId = toString << .sample_to_uproc_id
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , numReadsColumn
            , addToCartColumn cart
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed"
    ]


nameColumn : Table.Column PFAMResult Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .sample_name)
        }


nameLink : PFAMResult -> Table.HtmlDetails Msg
nameLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample result.sample.sample_id) ] [ text result.sample.sample_name ]
        ]


projectColumn : Table.Column PFAMResult Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .project >> .project_name)
        }


projectLink : PFAMResult -> Table.HtmlDetails Msg
projectLink result =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project result.sample.project_id) ] [ text result.sample.project.project_name ]
        ]


numReadsColumn : Table.Column PFAMResult Msg
numReadsColumn =
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


addToCartColumn : Cart.Model -> Table.Column PFAMResult Msg
addToCartColumn cart =
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
