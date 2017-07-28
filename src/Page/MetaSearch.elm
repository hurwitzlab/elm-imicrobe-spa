module Page.MetaSearch exposing (Model, Msg, init, update, view)

import Data.MetaSearch
import Dict
import Exts.Dict as EDict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import MultiSelect as Multi
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import RemoteData exposing (..)
import Request.MetaSearch
import Route
import Set
import String.Extra as SE
import Table
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type JsonType
    = StrType String
    | IntType Int
    | FloatType Float
    | ValueType Decode.Value


type alias Model =
    { pageTitle : String
    , query : String
    , params : Dict.Dict String String
    , selectedParams : List ( String, String )
    , optionValues : Dict.Dict String (List String)
    , searchResults : WebData (List (Dict.Dict String JsonType))
    , possibleOptionValues : Dict.Dict String (List JsonType)
    , restrictedParams : Dict.Dict String String
    }


init : Task PageLoadError Model
init =
    let
        loadParams =
            Request.MetaSearch.getParams |> Http.toTask

        handleLoadError err =
            -- If a resource task fail load error page
            let
                errMsg =
                    case err of
                        Http.BadStatus response ->
                            case String.length response.body of
                                0 ->
                                    "Bad status"

                                _ ->
                                    response.body

                        _ ->
                            toString err
            in
            Error.pageLoadError Page.Home errMsg
    in
    Task.andThen
        (\initialParams ->
            Task.succeed
                { pageTitle = ""
                , query = ""
                , params = initialParams
                , selectedParams = []
                , optionValues = Dict.empty
                , searchResults = NotAsked
                , possibleOptionValues = Dict.empty
                , restrictedParams = Dict.empty
                }
        )
        loadParams
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = AddParamOption String
    | RemoveOption String
    | UpdateOptionValue String String
    | UpdateMultiOptionValue String (List String)
    | UpdatePossibleOptionValues (Result Http.Error (Dict.Dict String (List JsonType)))
    | Search
    | UpdateSearchResults (WebData (List (Dict.Dict String JsonType)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddParamOption opt ->
            ( { model | selectedParams = addSelectedParam model opt }
            , getParamValues opt model.optionValues model.params
            )

        RemoveOption opt ->
            let
                newModel =
                    { model
                        | selectedParams = rmParam model.selectedParams opt
                        , optionValues =
                            rmOptionValue model.optionValues opt
                    }
            in
            ( newModel, doSearch newModel )

        Search ->
            ( model, doSearch model )

        UpdateOptionValue opt val ->
            ( { model
                | optionValues = Dict.insert opt [ val ] model.optionValues
              }
            , Cmd.none
            )

        UpdateMultiOptionValue opt vals ->
            ( { model
                | optionValues = Dict.insert opt vals model.optionValues
              }
            , Cmd.none
            )

        UpdateSearchResults response ->
            ( { model
                | searchResults = response
                , restrictedParams = mkRestrictedParams model.params response

                -- , restrictedOptionValues = mkRestrictedOptionValues response
              }
            , Cmd.none
            )

        UpdatePossibleOptionValues (Err err) ->
            ( model, Cmd.none )

        UpdatePossibleOptionValues (Ok response) ->
            ( { model
                | possibleOptionValues =
                    Dict.union response model.possibleOptionValues
              }
            , Cmd.none
            )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ style [ ( "text-align", "center" ) ] ]
                [ h2 [] [ text model.pageTitle ]
                , div [] (mkParamsSelect model)
                , div [] [ mkOptionTable model ]
                , div [] [ showResults model ]
                ]

            {--
            , div [] [ text ("restrictedParams = " ++ toString model.restrictedParams) ]
            , div [] [ text ("optionValues = " ++ toString model.optionValues) ]
            , div [] [ text ("searchResults = " ++ toString model.searchResults) ]
            , div [] [ text ("possibleOptionValues" ++ toString model.possibleOptionValues) ]
            --}
            ]
        ]



{--
config : Table.Config Data.Search.SearchResult Msg
config =
    Table.config
        { toId = toString << .id
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Type" .table_name
            , nameColumn
            ]
        }
        --}


mkParamsSelect : Model -> List (Html Msg)
mkParamsSelect model =
    let
        first =
            Html.option [] [ text "-- Select --" ]

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

        searchButtonRow =
            [ tr []
                [ td [ colspan 4, style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick Search ] [ text "Search" ] ]
                ]
            ]
    in
    case rows of
        [] ->
            text ""

        _ ->
            table [ style [ ( "width", "100%" ) ] ]
                (rows ++ searchButtonRow)


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


mkOptionRow : Dict.Dict String (List JsonType) -> ( String, String ) -> Html Msg
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
                        , input [ type_ "text", placeholder minVal, name minName ] []
                        ]
                    , td [ onInput (UpdateOptionValue maxName) ]
                        [ text "Max: "
                        , input [ type_ "text", placeholder maxVal, name maxName ] []
                        ]
                    ]

                _ ->
                    [ td [] [ strVals ]
                    , td [] []
                    ]

        buttons =
            [ td [] [ button [ onClick (RemoveOption optionName) ] [ text "Remove" ] ]
            ]
    in
    tr [] (title ++ el ++ buttons)


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


rmOptionValue : Dict.Dict String (List String) -> String -> Dict.Dict String (List String)
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
    Dict.Dict String (List String)
    -> Dict.Dict String String
    -> Encode.Value
serializeForm optionValues paramTypes =
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


doSearch : Model -> Cmd Msg
doSearch model =
    let
        url =
            "http://localhost:3006/samplesearch"

        body =
            serializeForm model.optionValues model.params
                |> Http.jsonBody

        decoderDict =
            Decode.dict oneOfJsonType

        decoder =
            Decode.list decoderDict
    in
    Http.post url body decoder
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults


getParamValues :
    String
    -> Dict.Dict String (List String)
    -> Dict.Dict String String
    -> Cmd Msg
getParamValues optionName optionValues params =
    let
        url =
            "http://localhost:3006/search_param_values"

        decoder =
            Decode.dict (Decode.list oneOfJsonType)

        body =
            Encode.object
                [ ( "param", Encode.string optionName )
                , ( "query", serializeForm optionValues params )
                ]
                |> Http.jsonBody
    in
    Http.post url body decoder
        |> Http.send UpdatePossibleOptionValues


showResults : Model -> Html msg
showResults model =
    case model.searchResults of
        NotAsked ->
            text ""

        Loading ->
            text "Loading ..."

        Failure e ->
            text (toString e)

        Success data ->
            case List.length data of
                0 ->
                    text "No results"

                _ ->
                    resultsTable model.selectedParams data


resultsTable : List ( String, String ) -> List (Dict.Dict String JsonType) -> Html msg
resultsTable fieldList results =
    let
        mkTh fld =
            th [] [ text (prettyName fld) ]

        fieldNames =
            List.map Tuple.first fieldList

        headerRow =
            [ tr [] (List.map mkTh ("specimen__sample_name" :: fieldNames)) ]

        resultRows =
            List.map (mkResultRow fieldList) results
    in
    div []
        [ text ("Found " ++ toString (List.length results))
        , table [] (headerRow ++ resultRows)
        ]


mkResultRow : List ( String, String ) -> Dict.Dict String JsonType -> Html msg
mkResultRow fieldList result =
    let
        getVal : String -> String
        getVal fldName =
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

        mkTd : ( String, String ) -> Html msg
        mkTd ( fldName, dataType ) =
            let
                align =
                    if dataType == "number" then
                        "right"
                    else
                        "left"
            in
            td [ style [ ( "text-align", align ) ] ] [ text (getVal fldName) ]

        nameCol =
            let
                name =
                    getVal "specimen__sample_name"

                sampleLink =
                    case String.toInt (getVal "specimen__sample_id") of
                        Ok sampleId ->
                            a [ Route.href (Route.Sample sampleId) ]
                                [ text name ]

                        Err _ ->
                            text name
            in
            td [ style [ ( "text-align", "left" ) ] ] [ sampleLink ]

        otherCols =
            List.map mkTd fieldList
    in
    tr [] (nameCol :: otherCols)


mkRestrictedParams :
    Dict.Dict String String
    -> WebData (List (Dict.Dict String JsonType))
    -> Dict.Dict String String
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
