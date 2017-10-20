module Page.Assemblies exposing (Model, Msg, init, update, view)

import Data.Assembly
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import List exposing (map)
import Page.Error as Error exposing (PageLoadError)
import Request.Assembly
import Route
import String exposing (join)
import Table exposing (defaultCustomizations)
import Task exposing (Task)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , assemblies : List Data.Assembly.Assembly
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Assemblies"

        loadAssemblies =
            Request.Assembly.list |> Http.toTask

        tblState =
            Task.succeed (Table.initialSort "Name")

        qry =
            Task.succeed ""
    in
    Task.map4 Model title loadAssemblies tblState qry
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
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


config : Table.Config Data.Assembly.Assembly Msg
config =
    Table.customConfig
        { toId = .assembly_name
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , Table.stringColumn "Organism" .organism
            , cdsColumn
            , ntColumn
            , pepColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


projectName : Data.Assembly.Assembly -> String
projectName assembly =
    case assembly.project of
        Nothing ->
            "NA"

        Just project ->
            project.project_name


projectColumn : Table.Column Data.Assembly.Assembly Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Projects"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy projectName
        }


projectLink : Data.Assembly.Assembly -> Table.HtmlDetails Msg
projectLink assembly =
    let
        link =
            case assembly.project of
                Nothing ->
                    text "NA"

                Just project ->
                    a [ Route.href (Route.Project project.project_id) ]
                        [ text project.project_name ]
    in
    Table.HtmlDetails [] [ link ]


nameColumn : Table.Column Data.Assembly.Assembly Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy (.assembly_name >> String.toLower)
        }


nameLink : Data.Assembly.Assembly -> Table.HtmlDetails Msg
nameLink assembly =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Assembly assembly.assembly_id) ]
            [ text assembly.assembly_name ]
        ]


cdsText : Data.Assembly.Assembly -> String
cdsText assembly =
    case assembly.cds_file of
        "" -> "No"

        _ -> "Yes"


cdsColumn : Table.Column Data.Assembly.Assembly Msg
cdsColumn =
    Table.veryCustomColumn
        { name = "CDS"
        , viewData = cdsLink
        , sorter = Table.increasingOrDecreasingBy cdsText
        }


cdsLink : Data.Assembly.Assembly -> Table.HtmlDetails Msg
cdsLink assembly =
    Table.HtmlDetails [ style [("min-width","3em")] ] -- min-width is to prevent column header from wrapping
        [ text (cdsText assembly) ]


ntText : Data.Assembly.Assembly -> String
ntText assembly =
    case assembly.nt_file of
        "" -> "No"

        _ -> "Yes"


ntColumn : Table.Column Data.Assembly.Assembly Msg
ntColumn =
    Table.veryCustomColumn
        { name = "NT"
        , viewData = ntLink
        , sorter = Table.increasingOrDecreasingBy ntText
        }


ntLink : Data.Assembly.Assembly -> Table.HtmlDetails Msg
ntLink assembly =
    Table.HtmlDetails []
        [ text (ntText assembly) ]


pepText : Data.Assembly.Assembly -> String
pepText assembly =
    case assembly.pep_file of
        "" -> "No"

        _ -> "Yes"


pepColumn : Table.Column Data.Assembly.Assembly Msg
pepColumn =
    Table.veryCustomColumn
        { name = "PEP"
        , viewData = pepLink
        , sorter = Table.increasingOrDecreasingBy pepText
        }


pepLink : Data.Assembly.Assembly -> Table.HtmlDetails Msg
pepLink assembly =
    Table.HtmlDetails []
        [ text (pepText assembly) ]


-- VIEW --


view : Model -> Html Msg
view model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        acceptableAssemblies =
            List.filter (String.contains lowerQuery << String.toLower << .assembly_name) model.assemblies

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableAssemblies

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , numShowing
                , small [ class "right" ]
                    [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , Table.view config model.tableState acceptableAssemblies
            ]
        ]