module Page.MetaSearch exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.Cart
import Dict exposing (Dict)
import Exts.Dict as EDict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import MultiSelect as Multi
import Page.Error as Error exposing (PageLoadError)
import RemoteData exposing (..)
import Request.MetaSearch
import Route
import Set
import String.Extra as SE
import Task exposing (Task)
import Config exposing (apiBaseUrl)
import View.Cart as Cart
import View.Widgets
import Util exposing ((=>))



---- MODEL ----


type JsonType
    = StrType String
    | IntType Int
    | FloatType Float
    | ValueType Decode.Value


type alias Model =
    { pageTitle : String
    , query : String
    , params : Dict String String
    , selectedParams : List ( String, String )
    , optionValues : Dict String (List String)
    , searchResults : WebData (List (Dict String JsonType))
    , possibleOptionValues : Dict String (List JsonType)
    , restrictedParams : Dict String String
    , cart : Cart.Model
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadParams =
            Request.MetaSearch.getParams |> Http.toTask
    in
    Task.andThen
        (\initialParams ->
            Task.succeed
                { pageTitle = "Sample Search"
                , query = ""
                , params = initialParams
                , selectedParams = []
                , optionValues = Dict.empty
                , searchResults = NotAsked
                , possibleOptionValues = Dict.empty
                , restrictedParams = Dict.empty
                , cart = Cart.init session.cart Cart.Editable
                }
        )
        loadParams
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = AddParamOption String
    | RemoveOption String
    | UpdateOptionValue String String
    | UpdateMultiOptionValue String (List String)
    | UpdatePossibleOptionValues (Result Http.Error (Dict String (List JsonType)))
    | Search
    | UpdateSearchResults (WebData (List (Dict String JsonType)))
    | SetQuery String
    | CartMsg Cart.Msg


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
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
                    }
            in
            newModel => doSearch newModel  => NoOp

        Search ->
            model => doSearch model => NoOp

        UpdateOptionValue opt val ->
            { model
                | optionValues = Dict.insert opt [ val ] model.optionValues
            }
            => Cmd.none
            => NoOp

        UpdateMultiOptionValue opt vals ->
            { model
                | optionValues = Dict.insert opt vals model.optionValues
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
                            }
            in
            --update session Search newModel--Cmd.none
            newModel => doSearch newModel
            => NoOp

        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none => NoOp

        CartMsg subMsg ->
            let
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
                [ h1 [] [ text model.pageTitle ]
                ]
            , div [ style [ ( "text-align", "center" ) ] ]
                [ div [] [ mkParamsSelect model ]
                , div [] [ mkOptionTable model ]
                ]
            , div [] [ showResults model ]
            ]
        ]


mkParamsSelect : Model -> Html Msg
mkParamsSelect model =
    let
        first =
            Html.option [] [ text "-- Select --" ]

        paramList =
            if Dict.isEmpty model.restrictedParams then
                model.params
            else
                model.restrictedParams

        alreadySelected =
            List.map Tuple.first model.selectedParams |> Set.fromList

        showKeys =
            Dict.keys paramList
                |> List.filter (\v -> not (Set.member v alreadySelected))

        rest =
            List.map mkParamOption showKeys
    in
    div [ class "padded-xl" ]
        [ text "Field: "
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
    SE.toSentenceCase category
        ++ ": "
        ++ String.join " " (List.map SE.toSentenceCase nameParts)


addSelectedParam : Model -> String -> List ( String, String )
addSelectedParam model optionName =
    case Dict.get optionName model.params of
        Just dataType ->
            model.selectedParams ++ [ ( optionName, dataType ) ]

        _ ->
            model.selectedParams


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
    if rows == [] then
        text ""
    else
        table [ style [ ( "width", "100%" ) ] ] rows


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
            if List.length vals > 1000 then
                input
                    [ onInput (UpdateOptionValue optionName)
                    , type_ "text"
                    , placeholder dataType
                    ]
                    []
            else
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
    tr [ class "padded border-top border-bottom", style [("padding", "10px")] ] (title ++ el ++ buttons)


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
                    if String.startsWith "min__" param || String.startsWith "max__" param then
                        String.dropLeft 5 param
                    else
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


showResults : Model -> Html Msg
showResults model =
    case model.searchResults of
        NotAsked ->
            text ""

        Loading ->
            text "Loading ..."

        Failure e ->
            text (toString e)

        Success data ->
            if List.length data == 0 then
                text "No results"
            else
                resultsTable model.cart model.selectedParams model.query data


resultsTable : Cart.Model -> List ( String, String ) -> String -> List (Dict String JsonType) -> Html Msg
resultsTable cart fieldList query results =
    let
        mkTh fld =
            th [] [ text (prettyName fld) ]

        fieldNames =
            List.map Tuple.first fieldList

        cartTh =
            th [ class "nowrap" ] [ Cart.addAllToCartButton cart Nothing sampleIds |> Html.map CartMsg ]

        headerRow =
            [ tr [] ((List.map mkTh ("specimen__sample_name" :: fieldNames)) ++ [cartTh]) ]

        resultRows =
            results
                |> List.filter (\result -> String.contains (String.toLower query) (String.toLower (getVal "specimen__sample_name" result)))
                |> List.map (mkResultRow cart fieldList)

        sampleIdFromResult result =
            case String.toInt (getVal "specimen__sample_id" result) of
                Ok sampleId ->
                    sampleId

                Err _ ->
                    0

        sampleIds =
            List.map sampleIdFromResult results
    in
    div [ style [("padding-top", "1em")] ]
        [ h2 []
            [ text "Results "
            , View.Widgets.counter (List.length resultRows)
            , small [ class "pull-right" ] [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
            ]
        , table [ class "table" ] [ tbody [] (headerRow ++ resultRows) ]
        ]


mkResultRow : Cart.Model -> List ( String, String ) -> Dict String JsonType -> Html Msg
mkResultRow cart fieldList result =
    let
        mkTd : ( String, String ) -> Html msg
        mkTd ( fldName, dataType ) =
            let
                align =
                    if dataType == "number" then
                        "right"
                    else
                        "left"
            in
            td [ style [ ( "text-align", align ) ] ] [ text (getVal fldName result) ]

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
    tr [] (nameCol :: otherCols ++ [cartCol])


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
