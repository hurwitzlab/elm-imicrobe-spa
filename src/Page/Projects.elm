module Page.Projects exposing (Model, Msg(..), init, update, view)

import Data.Project exposing (Project, Domain, Investigator)
import Data.Session exposing (Session)
--import Data.Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick, onDoubleClick, onCheck)
import Dialog
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
--import View.Cart as Cart
import View.Widgets
import Util exposing ((=>), capitalize)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , user_id : Maybe Int
--    , cart : Cart.Model
    , projects : List Project
    , tableState : Table.State
    , query : String
    , selectedRowId : Int
    , permFilterType : String
    , typeRestriction : List String
    , showInfoDialog : Bool
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
--                    , cart = (Cart.init session.cart Cart.Editable)
                    , projects = projects
                    , tableState = Table.initialSort "Name"
                    , query = ""
                    , selectedRowId = 0
                    , permFilterType = "All"
                    , typeRestriction = []
                    , showInfoDialog = False
                    }
            )
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State
    | FilterPermType String
    | SelectType String Bool
    | OpenInfoDialog Int
    | CloseInfoDialog
--    | CartMsg Cart.Msg
--    | SetSession Session


--type ExternalMsg
--    = NoOp
--    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
--        CartMsg subMsg ->
--            let
--                ( ( newCart, subCmd ), msgFromPage ) =
--                    Cart.update session subMsg model.cart
--            in
--            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart
--
--        SetSession newSession ->
--            let
--                newCart =
--                    Cart.init newSession.cart Cart.Editable
--
--                (subModel, cmd) =
--                    Cart.update newSession (Cart.SetSession newSession) model.cart
--            in
--            { model | cart = newCart } => Cmd.none => NoOp

        SetQuery newQuery ->
            { model | query = newQuery } => Cmd.none

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none

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

        OpenInfoDialog id ->
            { model | showInfoDialog = True, selectedRowId = id } => Cmd.none

        CloseInfoDialog ->
            { model | showInfoDialog = False } => Cmd.none



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
                    , View.Widgets.counter (List.length acceptableProjects)
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
                        [ if model.projects == [] then
                            text "None"
                          else if acceptableProjects == [] then
                            noProjects
                          else
                            Table.view (tableConfig model.selectedRowId) model.tableState acceptableProjects
                        ]
                    ]
                ]
           ]
           , Dialog.view
               (if model.showInfoDialog then
                   Just (infoDialogConfig model)
                else
                    Nothing
               )
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


tableConfig : Int -> Table.Config Project Msg
tableConfig selectedRowId =
    Table.customConfig
        { toId = .project_name
        , toMsg = SetTableState
        , columns =
            [ nameColumn
            , Table.stringColumn "Type" (.project_type)
            , domainColumn
--            , addToCartColumn cart
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs, rowAttrs = toRowAttrs selectedRowId }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table table-hover" ]


toRowAttrs : Int -> Project -> List (Attribute Msg)
toRowAttrs selectedRowId data =
    [ onDoubleClick (OpenInfoDialog data.project_id) ]


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
        , sorter = Table.increasingOrDecreasingBy .project_name
        }


nameLink : Project -> Table.HtmlDetails Msg
nameLink project =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project project.project_id) ]
            [ text project.project_name ]
        ]


--addToCartColumn : Cart.Model -> Table.Column Project Msg
--addToCartColumn cart =
--    Table.veryCustomColumn
--        { name = "Cart"
--        , viewData = (\project -> addToCartButton cart project)
--        , sorter = Table.unsortable
--        }
--
--
--addToCartButton : Cart.Model -> Project -> Table.HtmlDetails Msg
--addToCartButton cart project =
--    let
--        sampleIds =
--            List.map .sample_id project.samples
--    in
--    Table.HtmlDetails []
--        [ Cart.addAllToCartButton cart (Just ("Add Samples", "Remove Samples")) sampleIds |> Html.map CartMsg
--        ]


infoDialogConfig : Model -> Dialog.Config Msg
infoDialogConfig model =
    let
        content =
            case List.filter (\p -> p.project_id == model.selectedRowId) model.projects of
                [] ->
                    text ""

                project :: _ ->
                    div [ style [ ("margin-left","2em"), ("margin-right","2em") ] ]
                        [ View.Project.viewInfo project ]

        footer =
            button [ class "btn btn-default", onClick CloseInfoDialog ] [ text "Close" ]
    in
    { closeMessage = Just CloseInfoDialog
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Project Info" ])
    , body = Just content
    , footer = Just footer
    }
