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
import Time exposing (Time)
import Util exposing ((=>))
import View.Cart as Cart
import View.Spinner exposing (spinner)
import View.Widgets
import Events exposing (onKeyDown)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , doSearch : Bool
    , isSearching : Bool
    , searchStartTime : Time
    , searchTerm : String
    , query : String
    , taxId : String
    , minAbundance : Float
    , tableState : Table.State
    , pageNum : Int
    , cart : Cart.Model
    , results : Maybe Centrifuge2
    , errorMsg : Maybe String
    }


init : Session -> String -> Task PageLoadError Model
init session searchTerm =
    let
        doSearch =
            searchTerm /= ""
    in
    Task.succeed
        { pageTitle = "Taxonomy Search"
        , doSearch = doSearch
        , isSearching = doSearch
        , searchStartTime = 0
        , searchTerm = searchTerm
        , query = ""
        , taxId = searchTerm
        , minAbundance = 0
        , tableState = Table.initialSort "Abundance"
        , pageNum = 0
        , cart = (Cart.init session.cart Cart.Editable)
        , results = Nothing
        , errorMsg = Nothing
        }


pageSz : Int
pageSz = 20



-- UPDATE --


type Msg
    = SetStartTime Time
    | DelayedSearch Time
    | SearchCompleted (Result PageLoadError Centrifuge2)
    | NewSearch
    | SearchKeyDown Int
    | SetQuery String
    | SetTaxId String
    | SetAbundanceThreshold String
    | Next
    | Previous
    | SetTableState Table.State
    | SetSession Session
    | CartMsg Cart.Msg


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    let
        setStartTime =
            Task.perform SetStartTime Time.now
    in
    case msg of
        SetStartTime time ->
            { model | searchStartTime = time } => Cmd.none => NoOp

        DelayedSearch time ->
            if model.doSearch then
                if time - model.searchStartTime >= 500 * Time.millisecond then
                    let
                        search =
                            Request.Sample.taxonomy_search session.token model.searchTerm model.query (model.pageNum * pageSz) pageSz "abundance" "DESC" model.minAbundance
                                |> Http.toTask
                                |> Task.mapError Error.handleLoadError
                    in
                    { model | doSearch = False, isSearching = True, errorMsg = Nothing } => Task.attempt SearchCompleted search => NoOp
                else
                    model => Cmd.none => NoOp
            else
                model => Cmd.none => NoOp

        SearchCompleted (Ok results) ->
            { model | results = Just results, isSearching = False } => Cmd.none => NoOp

        SearchCompleted (Err error) ->
            { model | errorMsg = Just (toString error) } => Cmd.none => NoOp

        NewSearch ->
            model => Route.modifyUrl (Route.TaxonomySearch model.taxId) => NoOp

        SearchKeyDown key ->
            if key == 13 then -- enter key
                update session NewSearch model
            else
                model => Cmd.none => NoOp

        SetQuery newQuery ->
            { model | query = newQuery, doSearch = True } => setStartTime => NoOp

        SetTaxId strValue ->
            { model | taxId = strValue } => Cmd.none => NoOp

        SetAbundanceThreshold strValue ->
            let
                threshold =
                    String.toFloat strValue |> Result.withDefault 0
            in
            { model | minAbundance = threshold, doSearch = True } => setStartTime => NoOp

        Next ->
            let
                pageNum =
                    model.pageNum + 1
            in
            { model | pageNum = pageNum, doSearch = True } => Cmd.none => NoOp

        Previous ->
            let
                pageNum =
                    model.pageNum - 1 |> Basics.max 0
            in
            { model | pageNum = pageNum, doSearch = True } => Cmd.none => NoOp

        SetTableState newState ->
            { model | tableState = newState, doSearch = True, isSearching = True } => setStartTime => NoOp

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


--toSearchParams : Table.State -> (String, String)
--toSearchParams { sortName, isReversed } =
--    let
--        order =
--            if isReversed then
--                "DESC"
--            else
--                "ASC"
--
--        colName =
--            case sortName of
--                "Project" -> "project_name"
--
--                "Sample" -> "sample_name"
--
--                "Reads" -> "num_reads"
--
--                "Unique Reads" -> "num_unique_reads"
--
--                "Abundance" -> "abundance"
--
--                _ -> ""
--    in
--    (colName, order)



-- VIEW --


view : Model -> Html Msg
view model =
    let
        (count, samples) =
            case model.results of
                Nothing ->
                    (0, [])

                Just result ->
                    (result.sample_count, result.samples)

        searchBar =
            if model.searchTerm == "" then
                text ""
            else
                small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]

        filters =
            if model.searchTerm == "" then
                text ""
            else
                div [ style [("padding-bottom", "0.5em")] ]
                    [ text "Filter: Abundance > "
                    , input [ placeholder "0", size 4, onInput SetAbundanceThreshold ] []
                    , text " (value between 0 and 1)"
                    ]

        pageControls =
            if count == 0 then
                text ""
            else
                div [ class "pull-right" ]
                    [ text "Showing "
                    , pageSz |> toString |> text
                    , text " results starting at #"
                    , (model.pageNum * pageSz) |> Basics.max 1 |> toString |> text
                    , text ". "
                    , a [ onClick Previous, classList [("disabled", model.pageNum == 0)]  ] [ text "Previous" ]
                    , text " / "
                    , a [ onClick Next ] [ text "Next" ]
                    ]

        display =
            if model.errorMsg /= Nothing then
                div [ class "alert alert-danger" ] [ model.errorMsg |> Maybe.withDefault "" |> text ]
            else if model.isSearching then
                spinner
            else if model.searchTerm == "" then
                text ""
            else if count == 0 then
                p [ class "gray bold lead" ] [ text "No results" ]
            else
                Table.view (tableConfig model.cart) model.tableState samples
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text model.pageTitle
                , text " "
                , View.Widgets.counter count
                , searchBar
                ]
            , div [ style [("padding-bottom", "0.5em")] ]
                [ text "Species Name or NCBI Tax ID: "
                , input [ value model.taxId, size 20, onInput SetTaxId, onKeyDown SearchKeyDown ] []
                , text " "
                , button [ class "btn btn-default btn-xs", onClick NewSearch ] [ text "Search" ]
                , text " "
                , span [ class "gray", style [("padding-left", "1.5em")] ]
                    [ text " Examples: "
                    , a [ href "#/taxonomy_search/Prochlorococcus" ] [ text "Prochlorococcus" ]
                    , text ", "
                    , a [ href "#/taxonomy_search/Staphylococcus" ] [ text "Staphylococcus" ]
                    ]
                ]
            , filters
            , pageControls
            , br [] []
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
        , sorter = Table.unsortable --Table.increasingOrDecreasingBy .sample_name
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
        , sorter = Table.unsortable --Table.increasingOrDecreasingBy (.project_name << .project)
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
        , sorter = Table.unsortable --Table.decreasingOrIncreasingBy (.abundance << .sample_to_centrifuge)
        }


numReadsColumn : Table.Column CentrifugeSample Msg
numReadsColumn =
    Table.veryCustomColumn
        { name = "Reads"
        , viewData = nowrapColumn 4 << toString << .num_reads << .sample_to_centrifuge
        , sorter = Table.unsortable --Table.decreasingOrIncreasingBy (.num_reads << .sample_to_centrifuge)
        }


numUniqueReadsColumn : Table.Column CentrifugeSample Msg
numUniqueReadsColumn =
    Table.veryCustomColumn
        { name = "Unique Reads"
        , viewData = nowrapColumn 8 << toString << .num_unique_reads << .sample_to_centrifuge
        , sorter = Table.unsortable --Table.decreasingOrIncreasingBy (.num_unique_reads << .sample_to_centrifuge)
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
