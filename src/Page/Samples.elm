module Page.Samples exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Sample as Sample exposing (Sample)
import Data.Session as Session exposing (Session)
import Data.Cart
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onClick)
import MultiSelect as Multi
import Http
import List exposing (map)
import List.Extra
import Dict exposing (Dict)
import Exts.Dict as EDict
import Set exposing (Set)
import String.Extra as SE
import Json.Decode as Decode
import Json.Encode as Encode
import Page.Error as Error exposing (PageLoadError)
import RemoteData exposing (..)
import Request.Sample
import Request.MetaSearch
import Route
import String exposing (join)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Time exposing (Time)
import Util exposing ((=>), truncate)
import View.Cart as Cart
import Config exposing (apiBaseUrl)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , samples : List Sample
    , tableState : Table.State
    , query : String
    , sampleTypeRestriction : List String
    , cart : Cart.Model
    , params : Dict String String
    , selectedParams : List ( String, String )
    , optionValues : Dict String (List String)
    , searchResults : WebData (List (Dict String JsonType))
    , possibleOptionValues : Dict String (List JsonType)
    , restrictedParams : Dict String String
    , doSearch : Bool
    }


type JsonType
    = StrType String
    | IntType Int
    | FloatType Float
    | ValueType Decode.Value


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadSamples =
            Request.Sample.list |> Http.toTask

        loadSearchParams =
            Request.MetaSearch.getParams |> Http.toTask
    in
    -- FIXME load samples and search params in parallel
    loadSamples |> Task.andThen
        (\samples ->
            (loadSearchParams |> Task.andThen
                (\params ->
                    Task.succeed
                    { pageTitle = "Samples"
                    , samples = samples
                    , tableState = Table.initialSort "Name"
                    , query = ""
                    , sampleTypeRestriction = []
                    , cart = (Cart.init session.cart Cart.Editable)
                    , params = params
                    , selectedParams = []
                    , optionValues = Dict.empty
                    , searchResults = NotAsked
                    , possibleOptionValues = Dict.empty
                    , restrictedParams = Dict.empty
                    , doSearch = False
                    }
                )
            )
        )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | SetQuery String
    | SelectType String Bool
    | SetTableState Table.State
    | SetSession Session
    | AddParamOption String
    | RemoveOption String
    | UpdateOptionValue String String
    | UpdateMultiOptionValue String (List String)
    | UpdatePossibleOptionValues (Result Http.Error (Dict String (List JsonType)))
--    | Search
    | DelayedSearch Time
    | UpdateSearchResults (WebData (List (Dict String JsonType)))


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

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none => NoOp

        SelectType value bool ->
            let
                curOptions =
                    model.sampleTypeRestriction

                newOpts =
                    case bool of
                        True ->
                            List.sort (value :: curOptions)

                        False ->
                            List.filter ((/=) value) curOptions
            in
            { model | sampleTypeRestriction = newOpts } => Cmd.none => NoOp

        SetSession newSession ->
            let
                _ = Debug.log "Page.Samples.SetSession" (toString newSession)

                newCart =
                    Cart.init newSession.cart Cart.Editable

                (subModel, cmd) =
                    Cart.update newSession (Cart.SetSession newSession) model.cart
            in
            { model | cart = newCart } => Cmd.none => NoOp

        AddParamOption opt ->
            { model | selectedParams = addSelectedParam model opt }
            => getParamValues opt model.optionValues model.possibleOptionValues model.params
            => NoOp

        RemoveOption opt ->
            let
                newModel =
                    { model
                        | selectedParams = rmParam model.selectedParams opt
                        , optionValues = rmOptionValue model.optionValues opt
                        , doSearch = True
                    }
            in
            newModel => Cmd.none => NoOp

--        Search ->
--            { model | doSearch = False } => doSearch model => NoOp

        DelayedSearch time ->
            case model.doSearch of
                True ->
                    case model.selectedParams of
                        [] ->
                            { model | doSearch = False, searchResults = NotAsked } => Cmd.none => NoOp

                        _ ->
                            { model | doSearch = False } => doSearch model => NoOp

                False ->
                    model => Cmd.none => NoOp

        UpdateOptionValue opt val ->
            { model
                | optionValues = Dict.insert opt [ val ] model.optionValues
                , doSearch = True
            }
            => Cmd.none
            => NoOp

        UpdateMultiOptionValue opt vals ->
            { model
                | optionValues = Dict.insert opt vals model.optionValues
                , doSearch = True
            }
            => Cmd.none
            => NoOp

        UpdateSearchResults response ->
            { model
                | searchResults = response
                , restrictedParams = mkRestrictedParams model.params response
                -- , restrictedOptionValues = mkRestrictedOptionValues response
            }
            => Cmd.none
            => NoOp

        UpdatePossibleOptionValues (Err err) ->
            model => Cmd.none => NoOp

        UpdatePossibleOptionValues (Ok response) ->
            let
                opt =
                    case Dict.toList response |> List.head of
                        Nothing -> ""

                        Just opt ->
                            Tuple.first opt

                (minVal, maxVal) =
                    maxMinForOpt opt response

                dataType =
                    case Dict.get opt (Dict.fromList model.selectedParams) of
                        Nothing -> ""

                        Just dataType -> dataType

                optionValues =
                    case dataType of
                        "number" ->
                            Dict.fromList [ (("max__" ++ opt), [maxVal]), (("min__" ++ opt) , [minVal]) ]

                        _ -> Dict.fromList []

                newModel = { model
                                | possibleOptionValues = Dict.union response model.possibleOptionValues
                                , optionValues = Dict.union optionValues model.optionValues
                                , doSearch = True
                            }
            in
            newModel => Cmd.none => NoOp


config : Cart.Model -> Table.Config Sample Msg
config cart =
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , Table.stringColumn "Type" .sample_type
            , addToCartColumn cart
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]



-- VIEW --


view : Model -> Html Msg
view model =
    case model.searchResults of
        NotAsked -> showAll model

        Loading ->
            text "Loading ..."

        Failure e ->
            text (toString e)

        Success data ->
            case model.selectedParams of
                [] ->
                    showAll model

                _ ->
                    showSearchResults model data


searchView : Model -> Html Msg
searchView model =
    div []
        [ div [ class "bold" ] [ text "Attributes:" ]
        , div [] [ mkOptionTable model ]
        , div [] [ mkParamsSelect model ]
        ]


showSearchResults : Model -> List (Dict String JsonType) -> Html Msg
showSearchResults model results =
    let
        lowerQuery =
            String.toLower model.query

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length resultRows

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ] [ text numStr ]

        mkTh fld =
            th [] [ text (prettyName fld) ]

        fieldNames =
            List.map Tuple.first model.selectedParams

        sampleIdFromResult result =
            case String.toInt (getVal "specimen__sample_id" result) of
                Ok sampleId ->
                    sampleId

                Err _ ->
                    0

        sampleIds =
            List.map sampleIdFromResult results

        cartTh =
            th [ class "nowrap" ] [ text "Cart ", br [] [], Cart.addAllToCartButton model.cart sampleIds |> Html.map CartMsg ]

        headerRow =
            [ tr [] ((List.map mkTh ("specimen__project_name" :: "specimen__sample_name" :: "specimen__sample_type" :: fieldNames)) ++ [cartTh]) ]

        resultRows =
            results
                |> List.filter (\result -> String.contains (String.toLower model.query) (String.toLower (getVal "specimen__sample_name" result)))
                |> List.filter filterOnType
                |> List.map (mkResultRow model.cart model.selectedParams)

        filterOnType result =
            case List.length model.sampleTypeRestriction of
                0 ->
                    True

                _ ->
                    List.member (getVal "specimen__sample_type" result) model.sampleTypeRestriction

        body =
            case resultRows of
                [] ->
                    noResults

                _ ->
                    div [ style [("padding-top", "1em")] ]
                        [ table [ class "table" ] [ tbody [] (headerRow ++ resultRows) ]
                        ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , numShowing
                    , small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]
                    ]
                ]
            , div [ class "panel panel-default" ]
                [ div [ class "panel-body" ]
                    [ showTypes model.samples
                    , searchView model
                    ]
                ]
            , body
            ]
        ]


showAll : Model -> Html Msg
showAll model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        catter sample =
            String.concat
                (List.intersperse " "
                    [ sample.sample_name
                    , sample.project.project_name
                    , sample.sample_type
                    ]
                )
                |> String.toLower

        filteredSamples =
            List.filter
                (\sample -> String.contains lowerQuery (catter sample))
                model.samples

        acceptableSamples =
            case List.length model.sampleTypeRestriction of
                0 ->
                    filteredSamples

                _ ->
                    List.filter
                        (\v ->
                            List.member v.sample_type
                                model.sampleTypeRestriction
                        )
                        filteredSamples

        count =
            List.length acceptableSamples

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        body =
            case count of
                0 ->
                    noResults

                _ ->
                    div [] [ Table.view (config model.cart) model.tableState acceptableSamples ]
    in
        div [ class "container" ]
            [ div [ class "row" ]
                [ div [ class "page-header" ]
                    [ h1 []
                        [ text (model.pageTitle ++ " ")
                        , numShowing
                        , small [ class "right" ] [ input [ placeholder "Search", onInput SetQuery ] [] ]
                        ]
                    ]
                , div [ class "panel panel-default" ]
                    [ div [ class "panel-body" ]
                        [ showTypes model.samples
                        , searchView model
                        ]
                    ]
                , body
                ]
            ]


noResults : Html Msg
noResults =
    div [ class "italic gray", style [("font-size", "2em")] ] [ text "No results" ]


showTypes : List Sample -> Html Msg
showTypes samples =
    let
        sampleTypes =
            List.map (\x -> x.sample_type) samples
                |> List.filter ((/=) "")
                |> List.sort
                |> List.Extra.unique
    in
    case List.length sampleTypes of
        0 ->
            text ""

        _ ->
            fieldset []
                (span [ class "bold" ] [ text "Types: " ]
                    :: List.map mkCheckbox sampleTypes
                )


mkCheckbox : String -> Html Msg
mkCheckbox val =
    label [ style [("padding-left", "1em")]]
        [ input [ type_ "checkbox", onCheck (SelectType val) ] []
        , text (" " ++ val)
        ]


nameColumn : Table.Column Sample Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy (String.toLower << .sample_name)
        }


nameLink : Sample -> Table.HtmlDetails Msg
nameLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample sample.sample_id) ]
            [ text <| Util.truncate sample.sample_name ]
        ]


projectColumn : Table.Column Sample Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy (.project >> .project_name >> String.toLower)
        }


projectLink : Sample -> Table.HtmlDetails Msg
projectLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project sample.project_id) ]
            [ text <| Util.truncate sample.project.project_name ]
        ]


addToCartColumn : Cart.Model -> Table.Column Sample Msg
addToCartColumn cart =
    Table.veryCustomColumn
        { name = "Cart"
        , viewData = (\sample -> addToCartButton cart sample)
        , sorter = Table.unsortable
        }


addToCartButton : Cart.Model -> Sample -> Table.HtmlDetails Msg
addToCartButton cart sample =
    Table.HtmlDetails []
        [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg
        ]



addSelectedParam : Model -> String -> List ( String, String )
addSelectedParam model optionName =
    case Dict.get optionName model.params of
        Just dataType ->
            model.selectedParams ++ [ ( optionName, dataType ) ]

        _ ->
            model.selectedParams


mkParamsSelect : Model -> Html Msg
mkParamsSelect model =
    let
        first =
            Html.option [ selected True ] [ text "-- Select --" ]

        paramList =
            case Dict.isEmpty model.restrictedParams of
                True ->
                    model.params

                _ ->
                    model.restrictedParams

        alreadySelected =
            List.map Tuple.first model.selectedParams |> Set.fromList

        showKeys =
            Dict.keys paramList
                |> List.filter (\v -> not (Set.member v alreadySelected))

        rest =
            List.map mkParamOption showKeys
    in
    div [ class "padded", style [("padding-left","1em")] ]
        [ text "Add: "
        , select [ onInput AddParamOption ] (first :: rest)
        ]


mkParamOption : String -> Html msg
mkParamOption s =
    Html.option [ value s ] [ text (prettyName s) ]


prettyName : String -> String
prettyName s =
    let


        parts =
            String.split "__" s

        ( category, name ) =
            case parts of
                first :: rest :: [] ->
                    ( first, rest )

                _ ->
                    ( "NA", String.join "_" parts )

        nameParts =
            String.split "_" name
    in
    case s of
        "specimen__sample_name" -> "Sample"

        "specimen__project_name" -> "Project"

        "specimen__sample_type" -> "Type"

        _ ->
            SE.toSentenceCase category
                ++ ": "
                ++ String.join " " (List.map SE.toSentenceCase nameParts)


mkOptionTable : Model -> Html Msg
mkOptionTable model =
    let
        options =
            model.selectedParams

        rows =
            List.map (mkOptionRow model.possibleOptionValues) options

--        searchButtonRow =
--            [ tr []
--                [ td [ colspan 4, style [ ( "text-align", "center" ) ] ]
--                    [ button [ class "padded btn btn-primary", onClick Search ] [ text "Search" ] ]
--                ]
--            ]
    in
    case rows of
        [] ->
            text ""

        _ ->
            table [ style [ ( "width", "100%" ), ( "margin-left", "2em" ) ] ] rows


unpackJsonType : JsonType -> String
unpackJsonType v =
    case v of
        FloatType n ->
            toString n

        IntType n ->
            toString n

        StrType s ->
            s

        ValueType x ->
            toString x


mkOptionRow : Dict String (List JsonType) -> ( String, String ) -> Html Msg
mkOptionRow possibleOptionValues ( optionName, dataType ) =
    let
        title =
            [ th [] [ text (prettyName optionName) ] ]

        vals =
            case Dict.get optionName possibleOptionValues of
                Just v ->
                    v

                _ ->
                    []

        (minVal, maxVal) =
            maxMinForOpt optionName possibleOptionValues

        strVals =
            let
                mkOption s =
                    Html.option [ value s ] [ text s ]
            in
            case List.length vals > 1000 of
                True ->
                    input
                        [ onInput (UpdateOptionValue optionName)
                        , type_ "text"
                        , placeholder dataType
                        ]
                        []

                False ->
                    mkMultiSelect optionName vals

        minName =
            "min__" ++ optionName

        maxName =
            "max__" ++ optionName

        el =
            case dataType of
                "number" ->
                    [ td [ onInput (UpdateOptionValue minName) ]
                        [ text "Min: "
                        , input [ type_ "text", name minName, placeholder minVal ] []
                        ]
                    , td [ onInput (UpdateOptionValue maxName) ]
                        [ text "Max: "
                        , input [ type_ "text", name maxName, placeholder maxVal ] []
                        ]
                    ]

                _ ->
                    [ td [] [ strVals ]
                    , td [] []
                    ]

        buttons =
            [ td [] [ button [ class "btn btn-default btn-sm", onClick (RemoveOption optionName) ] [ text "Remove" ] ]
            ]
    in
    tr [ class "padded", style [("padding", "10px")] ] (title ++ el ++ buttons)


maxMinForOpt : String -> Dict String (List JsonType) -> (String, String)
maxMinForOpt optionName possibleOptionValues =
    let
        vals =
            case Dict.get optionName possibleOptionValues of
                Just v ->
                    v

                _ ->
                    []

        minVal =
            case List.take 1 vals of
                x :: [] ->
                    unpackJsonType x

                _ ->
                    ""

        maxVal =
            case List.drop (List.length vals - 1) vals of
                x :: [] ->
                    unpackJsonType x

                _ ->
                    ""
   in
   (minVal, maxVal)


mkMultiSelect : String -> List JsonType -> Html Msg
mkMultiSelect optionName vals =
    let
        strings =
            List.map unpackJsonType vals

        items =
            List.map (\s -> { value = s, text = s, enabled = True }) strings
    in
    Multi.multiSelect
        { onChange = UpdateMultiOptionValue optionName
        , items = items
        }
        []
        []


rmParam : List ( String, String ) -> String -> List ( String, String )
rmParam paramsList optToRemove =
    List.filter (\( k, v ) -> k /= optToRemove) paramsList


rmOptionValue : Dict String (List String) -> String -> Dict String (List String)
rmOptionValue optionValues optToRemove =
    let
        names =
            Set.fromList
                [ optToRemove
                , "min__" ++ optToRemove
                , "max__" ++ optToRemove
                ]
    in
    Dict.toList optionValues
        |> List.filter (\( k, v ) -> not (Set.member k names))
        |> Dict.fromList


oneOfJsonType : Decode.Decoder JsonType
oneOfJsonType =
    [ Decode.string
        |> Decode.map StrType
    , Decode.int
        |> Decode.map IntType
    , Decode.float
        |> Decode.map FloatType
    , Decode.value
        |> Decode.map ValueType
    ]
        |> Decode.oneOf


serializeForm :
    Dict String (List String)
    -> Dict String (List JsonType)
    -> Dict String String
    -> Encode.Value
serializeForm optionValues possibleOptionValues paramTypes =
    let
        mkFloats =
            List.filterMap (String.toFloat >> Result.toMaybe)

        encodeVals param vals =
            let
                paramName =
                    case
                        String.startsWith "min__" param
                            || String.startsWith "max__" param
                    of
                        True ->
                            String.dropLeft 5 param

                        False ->
                            param

                dataType =
                    EDict.getWithDefault "string" paramName paramTypes

                enc f xs =
                    case xs of
                        [] ->
                            Encode.null

                        x :: [] ->
                            f x

                        _ ->
                            Encode.list (List.map f xs)
            in
            case dataType of
                "number" ->
                    enc Encode.float (mkFloats vals)

                _ ->
                    enc Encode.string vals
    in
    Dict.toList optionValues
        |> List.map (\( k, vs ) -> ( k, encodeVals k vs ))
        |> Encode.object


--FIXME move to Request/MetaSearch.elm
doSearch : Model -> Cmd Msg
doSearch model =
    let
        url =
            apiBaseUrl ++ "/samples/search"

        body =
            serializeForm model.optionValues model.possibleOptionValues model.params
                |> Http.jsonBody

        decoderDict =
            Decode.dict oneOfJsonType

        decoder =
            Decode.list decoderDict
    in
    Http.post url body decoder
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults


--FIXME move to Request/MetaSearch.elm
getParamValues :
    String
    -> Dict String (List String)
    -> Dict String (List JsonType)
    -> Dict String String
    -> Cmd Msg
getParamValues optionName optionValues possibleOptionValues params =
    let
        url =
            apiBaseUrl ++ "/search_param_values"

        decoder =
            Decode.dict (Decode.list oneOfJsonType)

        body =
            Encode.object
                [ ( "param", Encode.string optionName )
                , ( "query", serializeForm optionValues possibleOptionValues params )
                ]
                |> Http.jsonBody
    in
    Http.post url body decoder
        |> Http.send UpdatePossibleOptionValues


mkRestrictedParams :
    Dict String String
    -> WebData (List (Dict String JsonType))
    -> Dict String String
mkRestrictedParams curParams searchResults =
    case searchResults of
        Success data ->
            let
                keys =
                    List.map Dict.keys data
                        |> List.concat
                        |> List.filter (\v -> v /= "_id")
                        |> List.Extra.unique

                types =
                    List.filterMap (\k -> Dict.get k curParams) keys
            in
            Dict.fromList (List.map2 (,) keys types)

        _ ->
            Dict.empty


mkResultRow : Cart.Model -> List ( String, String ) -> Dict String JsonType -> Html Msg
mkResultRow cart fieldList result =
    let
        mkTd : ( String, String ) -> Html msg
        mkTd ( fldName, dataType ) =
            let
                align =
--                    if dataType == "number" then
--                        "right"
--                    else
                        "left"
            in
            td [ style [ ( "text-align", align ) ] ] [ text (getVal fldName result) ]

        projectCol =
            let
                name =
                    getVal "specimen__project_name" result

                projectLink =
                    case String.toInt (getVal "specimen__project_id" result) of
                        Ok projectId ->
                            a [ Route.href (Route.Project projectId) ]
                                [ text name ]

                        Err _ ->
                            text name
            in
            td [ style [ ( "text-align", "left" ) ] ] [ projectLink ]

        nameCol =
            let
                name =
                    getVal "specimen__sample_name" result

                sampleLink =
                    case String.toInt (getVal "specimen__sample_id" result) of
                        Ok sampleId ->
                            a [ Route.href (Route.Sample sampleId) ]
                                [ text name ]

                        Err _ ->
                            text name
            in
            td [ style [ ( "text-align", "left" ) ] ] [ sampleLink ]

        typeCol =
            let
                name =
                    getVal "specimen__sample_type" result
            in
            td [ style [ ( "text-align", "left" ) ] ] [ text name ]

        cartCol =
            let
                sampleId =
                    case String.toInt (getVal "specimen__sample_id" result) of
                        Ok sampleId ->
                            sampleId

                        Err _ ->
                            0
            in
            td [ class "col-md-1" ] [ Cart.addToCartButton cart sampleId |> Html.map CartMsg ]

        otherCols =
            List.map mkTd fieldList
    in
    tr [] (projectCol :: nameCol :: typeCol :: otherCols ++ [cartCol])


getVal : String -> Dict String JsonType -> String
getVal fldName result =
    case Dict.get fldName result of
        Just (StrType s) ->
            s

        Just (IntType i) ->
            toString i

        Just (FloatType f) ->
            toString f

        Just (ValueType v) ->
            toString v

        _ ->
            "NA"
