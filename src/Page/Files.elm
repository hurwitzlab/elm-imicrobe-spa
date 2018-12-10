module Page.Files exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Sample as Sample exposing (Sample, SampleFile)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Request.Sample
import Request.SampleGroup
import Page.Error as Error exposing (PageLoadError)
import Task exposing (Task)
import Route
import Table exposing (defaultCustomizations)
import Set
import String.Extra
import Util exposing ((=>), dropFileName)
import Config exposing (dataCommonsUrl, apiBaseUrl)
import View.Widgets



---- MODEL ----


type alias Model =
    { pageTitle : String
    , token : String
    , tableState : Table.State
    , filterType : String
    , files : List SampleFile
    }


init : Session -> Maybe Int -> Task PageLoadError Model
init session id =
    let
        id_list = -- sample IDs
            session.cart.contents |> Set.toList

        loadSampleFiles =
            case id of
                Nothing -> -- Current
                    if id_list == [] then
                        Task.succeed []
                    else
                        Request.Sample.files session.token id_list |> Http.toTask

                Just id ->
                    Request.SampleGroup.files session.token id |> Http.toTask
    in
    loadSampleFiles
        |> Task.andThen
            (\files ->
                Task.succeed
                    { pageTitle = "Files"
                    , token = session.token
                    , tableState = Table.initialSort "Type"
                    , filterType = "All"
                    , files = files
                    }
            )
            |> Task.mapError Error.handleLoadError



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


config : String -> Table.Config SampleFile Msg
config token =
    Table.customConfig
        { toId = toString << .sample_file_id
        , toMsg = SetTableState
        , columns =
            [ sampleColumn
            , Table.stringColumn "Type" (.file_type << .sample_file_type)
            , fileColumn token
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table" ]


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


fileColumn : String -> Table.Column SampleFile Msg
fileColumn token =
    Table.veryCustomColumn
        { name = "File path (click link to download)"
        , viewData = fileLink token
        , sorter = Table.increasingOrDecreasingBy (.file >> String.toLower)
        }


fileLink : String -> SampleFile -> Table.HtmlDetails Msg
fileLink token file =
    let
        path =
            if String.startsWith "/iplant/home" file.file then
                String.Extra.replace "/iplant/home" "" file.file
            else
                file.file

        url =
            if String.startsWith "/iplant/home/shared/" file.file then
                dataCommonsUrl ++ file.file
            else
                apiBaseUrl ++ "/download" ++ path ++ "?token=" ++ token
    in
    if token /= "" || String.startsWith "/iplant/home/shared/" file.file then
        Table.HtmlDetails []
            [ a [ href url, target "_blank" ] [ text <| file.file ]
            ]
    else
        Table.HtmlDetails []
            [ text <| file.file ++ " (login to download)" ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , View.Widgets.counter (List.length model.files)
                ]
            , div []
                [ viewFiles model
                ]
            ]
        ]


viewFiles : Model -> Html Msg
viewFiles model =
    let
        filteredFiles =
            List.filter (\f -> (model.filterType == "All" || f.sample_file_type.file_type == model.filterType)) model.files
    in
    if filteredFiles == [] then
        text "No files to show"
    else
        div []
            [ viewToolbar model
            , Table.view (config model.token) model.tableState filteredFiles
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
        div [ class "btn-group margin-top-bottom", attribute "role" "group", attribute "aria-label" "..."]
           (btn "All" :: List.map (\t -> btn t) types)
    else
        div [ class "dropdown" ]
            [ button [ class "btn btn-default dropdown-toggle margin-top-bottom", attribute "type" "button", id "dropdownMenu1", attribute "data-toggle" "dropdown", attribute "aria-haspopup" "true", attribute "aria-expanded" "true" ]
                [ text "Filter by Type "
                , span [ class "caret" ] []
                ]
            , ul [ class "dropdown-menu", attribute "aria-labelledby" "dropdownMenu1" ]
                (lia "All" :: List.map (\t -> lia t) types)
            ]