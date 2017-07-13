module Page.Investigators exposing (Model, Msg, init, update, view)

import Data.Investigator
import Debug
import Dict
import Exts.Dict as EDict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Investigator
import Route
import Table
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , investigators : List Data.Investigator.Investigator
    , tableState : Table.State
    , query : String
    }


init : Task PageLoadError Model
init =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Investigators"

        loadInvestigators =
            Request.Investigator.list |> Http.toTask

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
    Task.map4 Model title loadInvestigators tblState qry
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetQuery newQuery ->
            let
                foo =
                    Debug.log "newQuery" newQuery
            in
            ( { model | query = newQuery }
            , Cmd.none
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )


config : Table.Config Data.Investigator.Investigator Msg
config =
    Table.config
        { toId = .investigator_name
        , toMsg = SetTableState
        , columns =
            [ Table.stringColumn "Name" .investigator_name
            , Table.stringColumn "Inst" .institution
            ]
        }



-- VIEW --
-- , div [] [ viewInvestigators model.investigators ]


view : Model -> Html Msg
view model =
    let
        query =
            model.query

        lowerQuery =
            String.toLower query

        acceptablePeople =
            List.filter (String.contains lowerQuery << String.toLower << .investigator_name) model.investigators
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , text ("query = " ++ model.query)
            , input [ placeholder "Search by Name", onInput SetQuery ] []
            , Table.view config model.tableState acceptablePeople
            ]
        ]


viewInvestigators invs =
    case List.length invs of
        0 ->
            text "No investigators"

        _ ->
            table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Institution" ]
                        ]
                    ]
                , tbody []
                    (List.map rowInv invs)
                ]


rowInv inv =
    tr []
        [ td [] [ a [ Route.href (Route.Investigator inv.investigator_id) ] [ text inv.investigator_name ] ]
        , td [] [ text inv.institution ]
        ]
