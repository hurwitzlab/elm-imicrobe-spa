module Page.Files exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Sample as Sample exposing (Sample, SampleFile)
import Data.Cart
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Request.Sample
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import View.Page as Page
import View.Cart as Cart
import Route
import Table exposing (defaultCustomizations)
import Set
import Debug
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , tableState : Table.State
    , filterType : String
    , files : List SampleFile
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Files"

        tableState =
            Task.succeed (Table.initialSort "Type")

        filterType =
            Task.succeed "All"

        id_list =
            session.cart.contents |> Set.toList

        loadSampleFiles =
            Request.Sample.files id_list |> Http.toTask

        handleLoadError error =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home (toString error)
    in
    Task.map4 Model title tableState filterType loadSampleFiles
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = SetTableState Table.State
    | Filter String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        Filter newType ->
            ( { model | filterType = newType }
            , Cmd.none
            )


-- VIEW --


config : Table.Config SampleFile Msg
config =
    Table.customConfig
        { toId = toString << .sample_file_id
        , toMsg = SetTableState
        , columns =
            [ sampleColumn
            , Table.stringColumn "Type" (.file_type << .sample_file_type)
            , fileColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


sampleColumn : Table.Column SampleFile Msg
sampleColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = sampleLink
        , sorter = Table.increasingOrDecreasingBy (.sample >> .sample_name >> String.toLower)
        }


sampleLink : SampleFile -> Table.HtmlDetails Msg
sampleLink file =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample file.sample_id) ]
            [ text <| Util.truncate file.sample.sample_name ]
        ]


fileColumn : Table.Column SampleFile Msg
fileColumn =
    Table.veryCustomColumn
        { name = "File"
        , viewData = fileLink
        , sorter = Table.increasingOrDecreasingBy (.file >> String.toLower)
        }


fileLink : SampleFile -> Table.HtmlDetails Msg
fileLink file =
    Table.HtmlDetails []
        [ a [ attribute "href" ("http://datacommons.cyverse.org/browse" ++ file.file) ] --TODO move Data Commons base URL to config file
            [ text <| file.file ]
        ]


view : Model -> Html Msg
view model =
    let
        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length model.files

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
                ]
            , div []
                [ viewFiles model
                ]
            ]
        ]


viewFiles : Model -> Html Msg
viewFiles model =
    let
        _ = Debug.log "filterType" model.filterType

        filteredFiles =
            List.filter (\f -> (model.filterType == "All" || f.sample_file_type.file_type == model.filterType)) model.files
    in
    case filteredFiles of
        [] -> text "No files to show"

        _ ->
            div []
                [ viewToolbar model
                , Table.view config model.tableState filteredFiles
                ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    let
        types =
            Set.toList <| Set.fromList <| List.map (.sample_file_type >> .file_type) model.files

        numTypes =
            List.length types

        btn label =
            button [ class "btn btn-default", onClick (Filter label) ] [ text label ]

        lia label =
            li [] [ a [ onClick (Filter label) ] [ text label ] ]
    in
    if (numTypes < 2) then
        text ""

    else if (numTypes < 10) then
        div [ class "padded btn-group", attribute "role" "group", attribute "aria-label" "..."]
           (btn "All" :: List.map (\t -> btn t) types)

    else
        div [ class "dropdown" ]
            [ button [ class "btn btn-default dropdown-toggle", attribute "type" "button", id "dropdownMenu1", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "true" ]
                [ text "Filter by Type "
                , span [ class "caret" ] []
                ]
            , ul [ class "dropdown-menu", attribute "aria-labelledby" "dropdownMenu1" ]
                (lia "All" :: List.map (\t -> lia t) types)
            ]