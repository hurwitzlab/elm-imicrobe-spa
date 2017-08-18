module Page.Samples exposing (Model, Msg, init, update, view)

import Data.Sample
import Data.Session as Session exposing (Session)
import Data.Cart
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onClick)
import Http
import List exposing (map)
import List.Extra
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Sample
import Route
import Set
import String exposing (join)
import Table exposing (defaultCustomizations)
import Task exposing (Task)
import Util exposing ((=>), truncate)
import View.Page as Page
import View.Cart as Cart


---- MODEL ----


type alias Model =
    { pageTitle : String
    , samples : List Data.Sample.Sample
    , tableState : Table.State
    , query : String
    , sampleTypeRestriction : List String
    , cart : Cart.Model
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadSamples =
            Request.Sample.list |> Http.toTask

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
    loadSamples
        |> Task.andThen
            (\samples ->
                Task.succeed
                    { pageTitle = "Samples"
                    , samples = samples
                    , tableState = Table.initialSort "Name"
                    , query = ""
                    , sampleTypeRestriction = []
                    , cart = (Cart.init session.cart)
                    }
            )
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetQuery String
    | SelectOption String Bool
    | SetTableState Table.State
    | CartMsg Cart.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
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

        CartMsg subMsg ->
            let
                ( newCart, subCmd ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd


config : Table.Config Data.Sample.Sample Msg
config =
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , Table.stringColumn "Type" .sample_type
            , addToCartColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]



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
                    , sample.project.project_name
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
                        (text "Types: "
                            :: List.map mkCheckbox sampleTypes
                        )

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length acceptableSamples

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
                , small [ class "right" ] [ input [ placeholder "Search by Name", onInput SetQuery ] [] ]
                ]
            , div [ class "panel panel-default" ]
                [ div [ class "panel-body" ]
                    [ restrict
                    ]
                ]
            , div [] [ Table.view config model.tableState acceptableSamples ]
            ]
        ]


mkCheckbox : String -> Html Msg
mkCheckbox val =
    label [ style [("padding-left", "1em")]]
        [ input [ type_ "checkbox", onCheck (SelectOption val) ] []
        , text (" " ++ val)
        ]


nameColumn : Table.Column Data.Sample.Sample Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy (String.toLower << .sample_name)
        }


nameLink : Data.Sample.Sample -> Table.HtmlDetails Msg
nameLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample sample.sample_id) ]
            [ text <| Util.truncate sample.sample_name ]
        ]


projectColumn : Table.Column Data.Sample.Sample Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy (.project >> .project_name >> String.toLower)
        }


projectLink : Data.Sample.Sample -> Table.HtmlDetails Msg
projectLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project sample.project_id) ]
            [ text <| Util.truncate sample.project.project_name ]
        ]


addToCartColumn : Table.Column Data.Sample.Sample Msg
addToCartColumn =
    Table.veryCustomColumn
        { name = "Cart"
        , viewData = addToCartLink
        , sorter = Table.unsortable
        }


addToCartLink : Data.Sample.Sample -> Table.HtmlDetails Msg
addToCartLink sample =
    Table.HtmlDetails []
        [ Cart.addToCartButton sample.sample_id |> Html.map CartMsg
        ]