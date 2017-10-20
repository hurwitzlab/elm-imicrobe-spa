module Page.Sample exposing (Model, Msg, init, update, view)

import Data.Sample as Sample exposing (Sample, SampleFile, SampleFile2, Ontology, SampleUProC)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Sample
import Route
import Task exposing (Task)
import Config exposing (dataCommonsUrl)
import Table exposing (defaultCustomizations)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , sample_id : Int
    , sample : Sample
    , loadingProteins : Bool
    , loadedProteins : Bool
    , proteins : List SampleUProC
    , attrTableState : Table.State
    , proteinTableState : Table.State
    , attrQuery : String
    , proteinQuery : String
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        loadSample =
            Request.Sample.get id |> Http.toTask
    in
    loadSample
        |> Task.andThen
            (\sample ->
                Task.succeed
                    { pageTitle = "Sample"
                    , sample_id = id
                    , sample = sample
                    , loadingProteins = False
                    , loadedProteins = False
                    , proteins = []
                    , attrTableState = Table.initialSort "Name"
                    , proteinTableState = Table.initialSort "Read Count"
                    , attrQuery = ""
                    , proteinQuery = ""
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetAttrQuery String
    | SetProteinQuery String
    | SetAttrTableState Table.State
    | SetProteinTableState Table.State
    | GetProteins
    | SetProteins (List SampleUProC)


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        SetAttrQuery newQuery ->
            ( { model | attrQuery = newQuery }
            , Cmd.none
            )

        SetProteinQuery newQuery ->
            ( { model | proteinQuery = newQuery }
            , Cmd.none
            )

        SetAttrTableState newState ->
            ( { model | attrTableState = newState }
            , Cmd.none
            )

        SetProteinTableState newState ->
            ( { model | proteinTableState = newState }
            , Cmd.none
            )

        GetProteins ->
            let
                loadProteins =
                    Request.Sample.proteins model.sample_id |> Http.toTask

                handleProteins proteins =
                    case proteins of
                        Ok proteins ->
                            SetProteins proteins

                        Err _ ->
                            let
                                _ = Debug.log "Error" "could not retrieve proteins"
                            in
                            SetProteins []
            in
            { model | loadingProteins = True } => Task.attempt handleProteins loadProteins

        SetProteins proteins ->
            { model | loadedProteins = True, proteins = proteins } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.sample.sample_name ]
                    ]
                ]
            , viewSample model.sample
            , viewFiles model.sample.sample_files
            , viewOntologies model.sample.ontologies
            , viewAttributes model
            , viewProteins model
            ]
        ]


viewSample : Sample -> Html msg
viewSample sample =
    let
        numFiles =
            List.length sample.sample_files

        numOntologies =
            List.length sample.ontologies
    in
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Project" ]
            , td []
                [ a [ Route.href (Route.Project sample.project_id) ] [ text sample.project.project_name ]
                ]
            ]
        , tr []
            [ th [] [ text "Sample" ]
            , td [] [ text sample.sample_name ]
            ]
        , tr []
            [ th [] [ text "Code" ]
            , td [] [ text sample.sample_acc ]
            ]
        , tr []
            [ th [] [ text "Sample Type" ]
            , td [] [ text sample.sample_type ]
            ]
        ]


viewFiles : List SampleFile2 -> Html msg
viewFiles files =
    let
        numFiles =
            List.length files

        label =
            case numFiles of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numFiles)
                        ]

        body =
            case numFiles of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (List.map viewFile files)
    in
    div []
        [ h2 []
            [ text "Files "
            , label
            ]
        , body
        ]


viewFile : SampleFile2 -> Html msg
viewFile file =
    tr []
        [ td []
            [ a [ href (dataCommonsUrl ++ file.file) ] [ text file.file ]
            ]
        ]


viewOntologies : List Ontology -> Html msg
viewOntologies ontologies =
    let
        numOntologies =
            List.length ontologies

        label =
            case numOntologies of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numOntologies)
                        ]

        body =
            case numOntologies of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (List.map viewOntology ontologies)
    in
    div []
        [ h2 []
            [ text "Ontologies "
            , label
            ]
        , body
        ]


viewOntology : Ontology -> Html msg
viewOntology ont =
    let
        display =
            ont.ontology_acc
                ++ (case ont.label of
                        "" ->
                            ""

                        _ ->
                            " (" ++ ont.label ++ ")"
                   )
    in
    tr []
        [ td []
            [ text display
            ]
        ]


attrTableConfig : Table.Config Sample.Attribute Msg
attrTableConfig =
    Table.customConfig
        { toId = toString << .sample_attr_id
        , toMsg = SetAttrTableState
        , columns =
            [ typeColumn
            , aliasColumn
            , valueColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-condensed"
    ]


typeColumn : Table.Column Sample.Attribute Msg
typeColumn =
    Table.customColumn
        { name = "Type"
        , viewData = .type_ << .sample_attr_type
        , sorter = Table.increasingOrDecreasingBy (.type_ << .sample_attr_type)
        }


aliasColumn : Table.Column Sample.Attribute Msg
aliasColumn =
    Table.customColumn
        { name = "Aliases"
        , viewData = aliasesToString << .sample_attr_type_aliases << .sample_attr_type
        , sorter = Table.increasingOrDecreasingBy (.type_ << .sample_attr_type)
        }


aliasesToString : List Sample.AttributeTypeAlias -> String
aliasesToString aliases =
    String.join ", " (List.map .alias_ aliases)


valueColumn : Table.Column Sample.Attribute Msg
valueColumn =
    Table.customColumn
        { name = "Value"
        , viewData = .attr_value
        , sorter = Table.increasingOrDecreasingBy .attr_value
        }


viewAttributes : Model -> Html Msg
viewAttributes model =
    let
        lowerQuery =
            String.toLower model.attrQuery

        attrFilter attr =
            ( (String.contains lowerQuery (String.toLower attr.attr_value))
                || (String.contains lowerQuery (aliasesToString attr.sample_attr_type.sample_attr_type_aliases |> String.toLower))
                || (String.contains lowerQuery (String.toLower attr.sample_attr_type.type_)) )

        acceptableAttributes =
            List.filter attrFilter model.sample.sample_attrs

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableAttributes

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]
        display =
            case acceptableAttributes of
                [] ->
                    text "None"

                _ ->
                    Table.view attrTableConfig model.attrTableState acceptableAttributes

    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Attributes "
                , numShowing
                , small [ class "right" ]
                    [ input [ placeholder "Search", onInput SetAttrQuery ] [] ]
                ]
            , display
            ]
        ]


proteinTableConfig : Table.Config SampleUProC Msg
proteinTableConfig =
    Table.customConfig
        { toId = toString << .sample_uproc_id
        , toMsg = SetProteinTableState
        , columns =
            [ uprocIdColumn
            , countColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


uprocIdColumn : Table.Column SampleUProC Msg
uprocIdColumn =
    Table.veryCustomColumn
        { name = "ID"
        , viewData = uprocIdLink
        , sorter = Table.increasingOrDecreasingBy (String.toLower << .uproc_id)
        }


uprocIdLink : SampleUProC -> Table.HtmlDetails Msg
uprocIdLink protein =
    let
        url =
            "http://pfam.xfam.org/family/" ++ protein.uproc_id
    in
    Table.HtmlDetails []
        [ a [ href url ] [ text protein.uproc_id ]
        ]


countColumn : Table.Column SampleUProC Msg
countColumn =
    Table.veryCustomColumn
        { name = "Read Count"
        , viewData = viewCount
        , sorter = Table.decreasingOrIncreasingBy .count
        }


viewCount : SampleUProC -> Table.HtmlDetails Msg
viewCount protein =
    Table.HtmlDetails []
        [ text (toString protein.count) ]


viewProteins : Model -> Html Msg
viewProteins model =
    let
        lowerQuery =
            String.toLower model.proteinQuery

        proteinFilter protein =
            ( (String.contains lowerQuery (String.toLower protein.uproc_id))
                || (String.contains lowerQuery (String.toLower (toString protein.count))) )

        acceptableProteins =
            List.filter proteinFilter model.proteins

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    case acceptableProteins of
                        [] ->
                            case model.proteinQuery of
                                 "" ->
                                    model.sample.protein_count

                                 _ ->
                                    0

                        _ ->
                            List.length acceptableProteins

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
            case model.proteins of
                [] ->
                    span [] []

                _ ->
                    small [ class "right" ]
                        [ input [ placeholder "Search", onInput SetProteinQuery ] [] ]

        body =
            case model.sample.protein_count of
                0 ->
                    text "None"

                _ ->
                    case model.loadedProteins of
                        True ->
                            case acceptableProteins of
                                [] ->
                                    text "None"

                                _ ->
                                    Table.view proteinTableConfig model.proteinTableState acceptableProteins

                        False ->
                            case model.loadingProteins of
                                True ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ div [ class "center" ] [ div [ class "padded-xl spinner" ] [] ] ] ] ] ]

                                False ->
                                    table [ class "table" ] [ tbody [] [ tr [] [ td [] [ button [ class "btn btn-default", onClick GetProteins ] [ text "Show Proteins" ] ] ] ] ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 []
                [ text "Proteins "
                , numShowing
                , searchBar
                ]
            , body
            ]
        ]