module Page.TaxonomySearch exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (Sample, Centrifuge2, CentrifugeSample)
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
    , taxId : String
    , minAbundance : Float
    , tableState : Table.State
    , cart : Cart.Model
    , results : List Centrifuge2
    }


init : Session -> String -> Task PageLoadError Model
init session searchTerm =
    let
        noSearch =
            searchTerm == ""
    in
    Task.succeed
        { pageTitle = "Taxonomy Search"
        , pageLoaded = noSearch
        , resultsLoaded = noSearch
        , searchTerm = searchTerm
        , query = ""
        , taxId = searchTerm
        , minAbundance = 0
        , tableState = Table.initialSort "Abundance"
        , cart = (Cart.init session.cart Cart.Editable)
        , results = []
        }



-- UPDATE --


type Msg
    = SetQuery String
    | SetTaxId String
    | Search
    | DelayedInit
    | InitCompleted (Result PageLoadError (List Centrifuge2))
    | SearchKeyDown Int
    | SetAbundanceThreshold String
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
                        Request.Sample.taxonomy_search session.token model.searchTerm |> Http.toTask |> Task.mapError Error.handleLoadError
                in
                { model | pageLoaded = True } => Task.attempt InitCompleted search => NoOp

        InitCompleted (Ok results) ->
            { model | resultsLoaded = True, results = results } => Cmd.none => NoOp

        InitCompleted (Err error) ->
            model => Cmd.none => NoOp -- TODO

        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none => NoOp

        SetTaxId strValue ->
            { model | taxId = strValue } => Cmd.none => NoOp

        Search ->
            model => Route.modifyUrl (Route.TaxonomySearch model.taxId) => NoOp

        SearchKeyDown key ->
            if key == 13 then -- enter key
                update session Search model
            else
                model => Cmd.none => NoOp

        SetAbundanceThreshold strValue ->
            let
                threshold =
                    case String.toFloat strValue of
                        Ok value -> value

                        Err _ -> 0
            in
            { model | minAbundance = threshold } => Cmd.none => NoOp

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



-- VIEW --


view : Model -> Html Msg
view model =
    let
        samples =
            case List.head model.results of
                Nothing -> []

                Just result -> result.samples

        resultCount =
            List.length model.results

        lowerQuery =
            String.toLower model.query

        filter sample =
            ( String.contains lowerQuery (String.toLower sample.sample_name)
                || String.contains lowerQuery (String.toLower sample.project.project_name)
                || String.contains lowerQuery (toString sample.sample_to_centrifuge.abundance) )
              && sample.sample_to_centrifuge.abundance >= model.minAbundance

        acceptableSamples =
            List.filter filter samples

        searchBar =
            if model.resultsLoaded && resultCount > 0 then
                small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]
            else
                text ""

        filters =
            if model.resultsLoaded && resultCount > 0 then
                div [ style [("padding-bottom", "0.5em")] ]
                    [ text "Filter: Abundance >= "
                    , input [ placeholder "0", size 4, onInput SetAbundanceThreshold ] []
                    , text " (value between 0 and 1)"
                    ]
            else
                text ""

        display =
            if model.resultsLoaded then
                if model.searchTerm == "" then
                    text ""
                else
                    if acceptableSamples == [] then
                        text "No results"
                    else
                        Table.view (tableConfig model.cart) model.tableState acceptableSamples
            else
                spinner
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text model.pageTitle
                , text " "
                , View.Widgets.counter (List.length acceptableSamples)
                , searchBar
                ]
            , div [ style [("padding-bottom", "0.5em")] ]
                [ text "Species Name or NCBI Tax ID: "
                , input [ value model.taxId, size 20, onInput SetTaxId, onKeyDown SearchKeyDown ] []
                , text " "
                , button [ class "btn btn-default btn-xs", onClick Search ] [ text "Search" ]
                , text " "
                , span [ class "gray", style [("padding-left", "1.5em")] ]
                    [ text " Example: "
                    , a [ href "#/taxonomy_search/Prochlorococcus" ] [ text "Prochlorococcus" ]
                    , text ", "
                    , a [ href "#/taxonomy_search/Staphylococcus" ] [ text "Staphylococcus" ]
                    ]
                ]
            , filters
            , display
            ]
        ]


tableConfig : Cart.Model -> Table.Config CentrifugeSample Msg
tableConfig cart =
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , numReadsColumn
            , numUniqueReadsColumn
            , abundanceColumn
            , addToCartColumn cart
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed"
    ]


nameColumn : Table.Column CentrifugeSample Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .sample_name
        }


nameLink : CentrifugeSample -> Table.HtmlDetails Msg
nameLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample sample.sample_id) ] [ text sample.sample_name ]
        ]


projectColumn : Table.Column CentrifugeSample Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy (.project_name << .project)
        }


projectLink : CentrifugeSample -> Table.HtmlDetails Msg
projectLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project sample.project_id) ] [ text sample.project.project_name ]
        ]


abundanceColumn : Table.Column CentrifugeSample Msg
abundanceColumn =
    Table.veryCustomColumn
        { name = "Abundance"
        , viewData = nowrapColumn 7 << toString << .abundance << .sample_to_centrifuge
        , sorter = Table.decreasingOrIncreasingBy (.abundance << .sample_to_centrifuge)
        }


numReadsColumn : Table.Column CentrifugeSample Msg
numReadsColumn =
    Table.veryCustomColumn
        { name = "Reads"
        , viewData = nowrapColumn 4 << toString << .num_reads << .sample_to_centrifuge
        , sorter = Table.decreasingOrIncreasingBy (.num_reads << .sample_to_centrifuge)
        }


numUniqueReadsColumn : Table.Column CentrifugeSample Msg
numUniqueReadsColumn =
    Table.veryCustomColumn
        { name = "Unique Reads"
        , viewData = nowrapColumn 8 << toString << .num_unique_reads << .sample_to_centrifuge
        , sorter = Table.decreasingOrIncreasingBy (.num_unique_reads << .sample_to_centrifuge)
        }


nowrapColumn : Int -> String -> Table.HtmlDetails Msg
nowrapColumn width value =
    let
        widthStr =
            (toString width) ++ "em"
    in
    Table.HtmlDetails [ style [("min-width",widthStr), ("text-align","right")] ] -- min-width is to prevent column header from wrapping
        [ text value ]


addToCartColumn : Cart.Model -> Table.Column CentrifugeSample Msg
addToCartColumn cart =
    Table.veryCustomColumn
        { name = "Cart"
        , viewData = (\sample -> addToCartButton cart sample)
        , sorter = Table.unsortable
        }


addToCartButton : Cart.Model -> CentrifugeSample -> Table.HtmlDetails Msg
addToCartButton cart sample =
    Table.HtmlDetails []
        [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg
        ]
