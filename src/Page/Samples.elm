module Page.Samples exposing (Model, Msg, init, update, view)

import Data.Sample
import Formatting as Fmt exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput)
import Http
import List exposing (map)
import List.Extra
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Sample
import Route
import String exposing (join)
import Table
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , samples : List Data.Sample.Sample
    , tableState : Table.State
    , query : String
    , sampleTypeRestriction : List String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Samples"

        loadSamples =
            Request.Sample.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""

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
    Task.map5 Model title loadSamples tblState qry (Task.succeed [])
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SelectOption String Bool
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        SelectOption value bool ->
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
            ( { model | sampleTypeRestriction = newOpts }, Cmd.none )


config : Table.Config Data.Sample.Sample Msg
config =
    Table.config
        { toId = toString << .sample_id
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , Table.stringColumn "Type" .sample_type
            ]
        }



-- VIEW --


view : Model -> Html Msg
view model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        catter sample =
            String.concat
                (List.intersperse " "
                    [ sample.sample_name
                    , sample.project_name
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

        sampleTypes =
            List.map (\x -> x.sample_type) model.samples
                |> List.filter ((/=) "")
                |> List.sort
                |> List.Extra.unique

        restrict =
            case List.length sampleTypes of
                0 ->
                    text ""

                _ ->
                    fieldset []
                        (text "Limit: "
                            :: List.map mkCheckbox sampleTypes
                        )
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [ style [ ( "text-align", "center" ) ] ]
                [ input [ placeholder "Search by Name", onInput SetQuery ] []
                , text ("Showing " ++ toString (List.length acceptableSamples))
                , restrict
                ]
            , div [] [ Table.view config model.tableState acceptableSamples ]
            ]
        ]


mkCheckbox : String -> Html Msg
mkCheckbox val =
    label []
        [ input [ type_ "checkbox", onCheck (SelectOption val) ] []
        , text val
        ]


maxColumnWidth : Int
maxColumnWidth =
    40


truncate : String -> String
truncate string =
    if String.length string <= maxColumnWidth then
        string
    else
        String.left (maxColumnWidth - 3) string ++ "..."


nameColumn : Table.Column Data.Sample.Sample Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy .sample_name
        }


nameLink : Data.Sample.Sample -> Table.HtmlDetails Msg
nameLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample sample.sample_id) ]
            [ text <| truncate sample.sample_name ]
        ]


projectColumn : Table.Column Data.Sample.Sample Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy .project_name
        }


projectLink : Data.Sample.Sample -> Table.HtmlDetails Msg
projectLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project sample.project_id) ]
            [ text <| truncate sample.project_name ]
        ]
