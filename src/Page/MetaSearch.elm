module Page.MetaSearch exposing (Model, Msg, init, update, view)

import Data.MetaSearch
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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
    , optionValues : Dict.Dict String String
    , searchResults : WebData (List (Dict.Dict String JsonType))
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
                }
        )
        loadParams
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = AddParamOption String
    | RemoveOption String
    | UpdateOptionValue String String
    | Search
    | UpdateSearchResults (WebData (List (Dict.Dict String JsonType)))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddParamOption opt ->
            ( { model | selectedParams = addSelectedParam model opt }, Cmd.none )

        RemoveOption opt ->
            let
                newOptionValues =
                    rmOptionValue model.optionValues opt
            in
            ( { model
                | selectedParams = rmParam model.selectedParams opt
                , optionValues = rmOptionValue model.optionValues opt
              }
            , doSearch newOptionValues
            )

        Search ->
            ( model, doSearch model.optionValues )

        UpdateOptionValue opt val ->
            ( { model
                | optionValues = Dict.insert opt val model.optionValues
              }
            , Cmd.none
            )

        UpdateSearchResults response ->
            ( { model
                | searchResults = response

                -- , restrictedOptionList = mkRestrictedOptionList model.optionList response
                -- , restrictedOptionValues = mkRestrictedOptionValues response
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
                , div [] [ mkOptionTable model.selectedParams ]
                ]
            , div [] [ text (toString model.optionValues) ]
            , div [] [ text (toString model.searchResults) ]
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

        alreadySelected =
            List.map Tuple.first model.selectedParams |> Set.fromList

        showKeys =
            Dict.keys model.params
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


mkOptionTable : List ( String, String ) -> Html Msg
mkOptionTable options =
    let
        rows =
            List.map mkOptionRow options

        searchButtonRow =
            [ tr []
                [ td [ colspan 4, style [ ( "text-align", "center" ) ] ]
                    [ button [ onClick Search ] [ text "Search" ] ]
                ]
            ]
    in
    case rows of
        [] ->
            text "No options"

        _ ->
            table [ style [ ( "width", "100%" ) ] ]
                (rows ++ searchButtonRow)


mkOptionRow : ( String, String ) -> Html Msg
mkOptionRow ( optionName, dataType ) =
    let
        title =
            [ th [] [ text (prettyName optionName) ] ]

        minName =
            "min__" ++ optionName

        maxName =
            "max__" ++ optionName

        el =
            case dataType of
                "number" ->
                    [ td [ onInput (UpdateOptionValue minName) ]
                        [ text "Min: "
                        , input [ type_ "text", placeholder "min", name minName ] []
                        ]
                    , td [ onInput (UpdateOptionValue maxName) ]
                        [ text "Max: "
                        , input [ type_ "text", placeholder "max", name maxName ] []
                        ]
                    ]

                _ ->
                    [ td [ onInput (UpdateOptionValue optionName) ]
                        [ input [ type_ "text", placeholder dataType ] [] ]
                    , td [] []
                    ]

        buttons =
            [ td [] [ button [ onClick (RemoveOption optionName) ] [ text "Remove" ] ]
            ]
    in
    tr [] (title ++ el ++ buttons)


rmParam : List ( String, String ) -> String -> List ( String, String )
rmParam paramsList optToRemove =
    List.filter (\( k, v ) -> k /= optToRemove) paramsList


rmOptionValue : Dict.Dict String String -> String -> Dict.Dict String String
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


doSearch : Dict.Dict String String -> Cmd Msg
doSearch options =
    let
        url =
            -- "https://www.imicrobe.us/sample/search_results.json"
            "http://localhost:3006/samplesearch"

        dictList =
            Dict.toList options

        encoded =
            Encode.object
                (List.map (\( k, v ) -> ( k, Encode.string v )) dictList)

        body =
            Http.jsonBody encoded

        decoderDict =
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
                |> Decode.dict

        decoder =
            -- Decode.at [ "samples" ] (Decode.list decoderDict)
            Decode.list decoderDict
    in
    Http.post url body decoder
        |> RemoteData.sendRequest
        |> Cmd.map UpdateSearchResults
