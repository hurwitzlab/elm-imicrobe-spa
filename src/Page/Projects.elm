module Page.Projects exposing (Model, Msg, init, update, view)

import Data.Project exposing (Project, Domain, Investigator)
import Data.Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onCheck)
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Http
import List.Extra
import Page.Error as Error exposing (PageLoadError)
import Request.Project
import Route
import String exposing (join)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import View.Project
import View.FilterButtonGroup
import Util exposing ((=>), capitalize)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , user_id : Maybe Int
    , projects : List Project
    , tableState : Table.State
    , query : String
    , selectedRowId : Int
    , permFilterType : String
    , typeRestriction : List String
    }


init : Session -> Task PageLoadError Model
init session =
    let
        loadProjects =
            Request.Project.list session.token |> Http.toTask

        user_id =
            Maybe.map .user_id session.user
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
                    , typeRestriction = []
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State
    | SelectRow Int
    | FilterPermType String
    | SelectType String Bool


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
            { model | permFilterType = filterType, selectedRowId = 0 } => Cmd.none

        SelectType value bool ->
            let
                curOptions =
                    model.typeRestriction

                newOpts =
                    case bool of
                        True ->
                            List.sort (value :: curOptions)

                        False ->
                            List.filter ((/=) value) curOptions
            in
            { model | typeRestriction = newOpts } => Cmd.none


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
        lowerQuery =
            String.toLower model.query

        checkPerms project =
            if model.permFilterType == "All" then
                True
            else
                case model.user_id of
                    Nothing ->
                        False

                    Just id ->
                        (List.map .user_id project.users |> List.member id) ||
                        (List.map .users project.project_groups |> List.concat |> List.map .user_id |> List.member id)


        searchFilter project =
            ((String.contains lowerQuery (String.toLower project.project_name))
                || (String.contains lowerQuery (String.toLower project.project_type))
                || (List.map (String.contains lowerQuery << .domain_name) project.domains |> List.foldr (||) False))

        filterOnType result =
            if model.typeRestriction == [] then
                True
            else
                List.member (result.project_type |> capitalize) model.typeRestriction

        acceptableProjects =
            model.projects
            |> List.filter searchFilter
            |> List.filter filterOnType
            |> List.filter checkPerms

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableProjects

                numStr =
                    count |> toFloat |> format myLocale
            in
            if count == 0 then
                text ""
            else
                span [ class "badge" ]
                    [ text numStr ]

        (infoPanel, sizeClass) =
            case List.filter (\p -> p.project_id == model.selectedRowId) model.projects of
                [] ->
                    (text "", "")

                project :: _ ->
                    (View.Project.viewInfo project, "col-md-9")

        noProjects =
            if model.user_id /= Nothing then
                div [ class "well" ]
                    [ p [] [ text "You don't have any projects yet." ]
                    , p []
                        [ text "To create a project, go to the "
                        , a [ Route.href Route.Dashboard ] [ text "Dashboard" ]
                        , text " and click 'New'."
                        ]
                    ]
            else
                div [ class "well" ]
                    [ p []
                        [ text "Please "
                        , a [ Route.href Route.Login ] [ text "login" ]
                        , text " to see your projects."
                        ]
                    ]
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
                , div [ class "panel panel-default" ]
                    [ div [ class "panel-body" ]
                        [ viewTypes model.projects
                        , viewAccessFilter model.permFilterType
                        ]
                    ]
                , div [ class "container" ]
                    [ div [ class "row" ]
                        [ div [ class sizeClass ]
                            [ if model.projects == [] then
                                text "None"
                              else if acceptableProjects == [] then
                                noProjects
                              else
                                Table.view (tableConfig model.selectedRowId) model.tableState acceptableProjects
                            ]
                        , infoPanel
                        ]
                    ]
                ]
           ]
        ]


viewTypes : List Project -> Html Msg
viewTypes projects =
    let
        types =
            List.map (\p -> p.project_type) projects
                |> List.filter ((/=) "")
                |> List.map capitalize
                |> List.sort
                |> List.Extra.unique
    in
    if List.length types == 0 then
        text ""
    else
        fieldset []
            (span [ class "bold" ] [ text "Types: " ]
                :: List.map mkCheckbox types
            )


mkCheckbox : String -> Html Msg
mkCheckbox val =
    span [ style [("padding-left", "1em")]]
        [ input [ type_ "checkbox", onCheck (SelectType val) ] []
        , text (" " ++ val)
        ]


viewAccessFilter : String -> Html Msg
viewAccessFilter permFilterType =
    div []
        [ span [ class "bold" ] [ text "Access: " ]
        , View.FilterButtonGroup.view permissionFilterConfig permFilterType
        ]


permissionFilterConfig : View.FilterButtonGroup.Config Msg
permissionFilterConfig =
    View.FilterButtonGroup.Config [ "All", "Mine" ] FilterPermType
