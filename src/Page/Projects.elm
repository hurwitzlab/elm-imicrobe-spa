module Page.Projects exposing (Model, Msg, init, update, view)

import Data.Project exposing (Project, Domain, Investigator)
import Data.Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import List
import Page.Error as Error exposing (PageLoadError)
import Request.Project
import Route
import String exposing (join)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , user_id : Int
    , projects : List Project
    , tableState : Table.State
    , query : String
    , selectedRowId : Int
    , permFilterType : String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadProjects =
            Request.Project.list |> Http.toTask

        user_id =
            case session.user of
                Nothing ->
                    0

                Just user ->
                    user.user_id
    in
    loadProjects
        |> Task.andThen
            (\projects ->
                Task.succeed
                    { pageTitle = "Projects"
                    , user_id = user_id
                    , projects = projects
                    , tableState = Table.initialSort "Name"
                    , query = ""
                    , selectedRowId = 0
                    , permFilterType = "All"
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State
    | SelectRow Int
    | FilterPermType String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none

        SelectRow id ->
            let
                selectedRowId =
                    if model.selectedRowId == id then
                        0 -- unselect
                    else
                         id
            in
            { model | selectedRowId = selectedRowId } => Cmd.none

        FilterPermType filterType ->
            { model | permFilterType = filterType } => Cmd.none


tableConfig : Int -> Table.Config Project Msg
tableConfig selectedRowId =
    Table.customConfig
        { toId = .project_name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , Table.stringColumn "Type" (.project_type)
            , domainColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toRowAttrs selectedRowId }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-hover"
    ]


toRowAttrs : Int -> Project -> List (Attribute Msg)
toRowAttrs selectedRowId data =
    onClick (SelectRow data.project_id)
    :: (if (data.project_id == selectedRowId) then
            [ attribute "class" "active" ]
         else
            []
        )


domainColumn : Table.Column Project Msg
domainColumn =
    Table.customColumn
        { name = "Domains"
        , viewData = domainsToString << .domains
        , sorter = Table.increasingOrDecreasingBy (domainsToString << .domains)
        }


domainsToString : List Domain -> String
domainsToString domains =
    join ", " (List.map .domain_name domains)


nameColumn : Table.Column Project Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = nameLink
        , sorter = Table.unsortable
        }


nameLink : Project -> Table.HtmlDetails Msg
nameLink project =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project project.project_id) ]
            [ text project.project_name ]
        ]



-- VIEW --


view : Model -> Html Msg
view model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        checkPerms project =
            case model.permFilterType of
                "All" ->
                    True

                _ ->
                    List.map .user_id project.users |> List.member model.user_id

        filter project =
            ((String.contains lowerQuery (String.toLower project.project_name))
                || (String.contains lowerQuery (String.toLower project.project_type))
                || (List.map (String.contains lowerQuery << .domain_name) project.domains |> List.foldr (||) False))
                && checkPerms project

        acceptableProjects =
            List.filter filter model.projects

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableProjects

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    text ""

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        filterButton label =
            let
                classes =
                    if label == model.permFilterType then
                        "btn btn-default active"
                    else
                        "btn btn-default"
            in
            button [ class classes, onClick (FilterPermType label) ] [ text label ]

        (infoPanel, sizeClass) =
            case viewInfo model of
                Nothing ->
                    (text "", "")

                Just panel ->
                    (panel, "col-md-8")

    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ div []
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , numShowing
                    , small [ class "pull-right" ]
                        [ input [ placeholder "Search", onInput SetQuery ] [] ]
                    ]
                , div [ class "btn-group" ]
                    [ filterButton "All"
                    , filterButton "Mine"
                    ]
                , br [] []
                , br [] []
                , div [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class sizeClass ]
                            [ Table.view (tableConfig model.selectedRowId) model.tableState acceptableProjects
                            ]
                        , infoPanel
                        ]
                    ]
                ]
           ]
        ]


viewInfo : Model -> Maybe (Html Msg)
viewInfo model =
    case List.filter (\p -> p.project_id == model.selectedRowId) model.projects of
        [] ->
            Nothing

        project :: _ ->
            Just (div [ class "col-md-4" ]
                [ table [ class "info-table" ]
                    [ tr []
                        [ th [] [ text "Name " ]
                        , td [] [ text project.project_name ]
                        ]
                    , tr []
                        [ th [] [ text "Code " ]
                        , td [] [ text project.project_code ]
                        ]
                    , tr []
                        [ th [] [ text "Type " ]
                        , td [] [ text project.project_type ]
                        ]
                    , tr []
                        [ th [] [ text "URL " ]
                        , td [] [ text project.url ]
                        ]
--                    , tr []
--                        [ th [] [ text "PI " ]
--                        , td [] [ text project.pi ]
--                        ]
                    , tr []
                        [ th [] [ text "Investigators " ]
                        , td [] (List.map investigatorLink project.investigators |> List.intersperse (text ", "))
                        ]
                    , tr []
                        [ th [] [ text "Type " ]
                        , td [] [ text project.project_type ]
                        ]
                    ]
                ])


investigatorLink : Investigator -> Html Msg
investigatorLink investigator =
    a [ Route.href (Route.Investigator investigator.investigator_id) ] [ text investigator.investigator_name ]
