module Page.TaxonomySearch exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (Sample, Centrifuge2, CentrifugeSample)
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
    , taxId : String
    , minAbundance : Float
    , tableState : Table.State
    , cart : Cart.Model
    , results : List Centrifuge2
    }


init : Session -> String -> Task PageLoadError Model
init session searchTerm =
    doSearch searchTerm
        |> Task.andThen
            (\results ->
                Task.succeed
                    { pageTitle = "Taxonomy Search"
                    , searchTerm = searchTerm
                    , query = ""
                    , taxId = searchTerm
                    , minAbundance = 0
                    , tableState = Table.initialSort "Abundance"
                    , cart = (Cart.init session.cart Cart.Editable)
                    , results = results
                    }
            )
        |> Task.mapError Error.handleLoadError


doSearch : String -> Task Http.Error (List Centrifuge2)
doSearch searchTerm =
    case searchTerm of
        "" ->
            Task.succeed []

        _ ->
            Request.Sample.taxonomy_search searchTerm |> Http.toTask



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | SetQuery String
    | SetTaxId String
    | Search
    | SetResults String (List Centrifuge2)
    | SetAbundanceThreshold String
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

        SetTaxId strValue ->
            { model | taxId = strValue } => Cmd.none => NoOp

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
            model => Task.attempt (handleResults model.taxId) (doSearch model.taxId) => NoOp

        SetResults searchTerm results ->
            { model | searchTerm = searchTerm, results = results } => Route.modifyUrl (Route.TaxonomySearch searchTerm) => NoOp

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


-- VIEW --


view : Model -> Html Msg
view model =
    let
        samples =
            case List.head model.results of
                Nothing -> []

                Just result -> result.samples

        lowerQuery =
            String.toLower model.query

        filter sample =
            ( String.contains lowerQuery (String.toLower sample.sample_name)
                || String.contains lowerQuery (String.toLower sample.project.project_name)
                || String.contains lowerQuery (toString sample.sample_to_centrifuge.abundance) )
              && sample.sample_to_centrifuge.abundance >= model.minAbundance

        acceptableSamples =
            List.filter filter samples

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableSamples

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        filters =
            case model.searchTerm of
                "" -> text ""

                _ ->
                    div [ style [("padding-bottom", "0.5em")] ]
                        [ text "Filter: Abundance >= "
                        , input [ placeholder "0", size 4, onInput SetAbundanceThreshold ] []
                        ]

        display =
            case model.searchTerm of
                "" -> text ""

                _ ->
                    case acceptableSamples of
                        [] ->
                            text "No results"

                        _ ->
                            Table.view (tableConfig model.cart) model.tableState acceptableSamples
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Taxonomy Search "
                , numShowing
                , small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]
                ]
            , div [ style [("padding-bottom", "0.5em")] ]
                [ text "NCBI Tax ID "
                , input [ value model.taxId, size 10, onInput SetTaxId ] []
                , text " "
                , button [ class "btn btn-default btn-xs", onClick Search ] [ text "Search" ]
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
        , viewData = nowrapColum << toString << .abundance << .sample_to_centrifuge
        , sorter = Table.decreasingOrIncreasingBy (toString << .abundance << .sample_to_centrifuge)
        }


numReadsColumn : Table.Column CentrifugeSample Msg
numReadsColumn =
    Table.veryCustomColumn
        { name = "Reads"
        , viewData = nowrapColum << toString << .num_reads << .sample_to_centrifuge
        , sorter = Table.decreasingOrIncreasingBy (toString << .num_reads << .sample_to_centrifuge)
        }


numUniqueReadsColumn : Table.Column CentrifugeSample Msg
numUniqueReadsColumn =
    Table.veryCustomColumn
        { name = "Unique Reads"
        , viewData = nowrapColum << toString << .num_unique_reads << .sample_to_centrifuge
        , sorter = Table.decreasingOrIncreasingBy (toString << .num_unique_reads << .sample_to_centrifuge)
        }


nowrapColum : String -> Table.HtmlDetails Msg
nowrapColum value =
    Table.HtmlDetails [ style [("min-width","8em")] ] -- min-width is to prevent column header from wrapping
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
