module Page.Publication exposing (Model, Msg, init, update, view)

import Data.Publication exposing (Publication, ProjectFile)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Publication
import Route
import Task exposing (Task)
import Set
import Config exposing (dataCommonsUrl)
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , publication_id : Int
    , publication : Publication
    , filterType : String
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        loadPublication =
            Request.Publication.get id |> Http.toTask
    in
    Task.map4 Model (Task.succeed "Publication") (Task.succeed id) loadPublication (Task.succeed "All")
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = FilterOnFileType String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilterOnFileType newType ->
            let
                _ = Debug.log "newType" newType
            in
            { model | filterType = newType } => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text "Publication "
                    , small []
                        [ text model.publication.title ]
                    ]
                ]
            , viewPublication model.publication
            , viewFiles model.publication.project_files model.filterType
            ]
        ]


viewPublication : Publication -> Html msg
viewPublication pub =
    let
        projectLink =
            case pub.project of
                Nothing ->
                    text "NA"

                Just project ->
                    a
                        [ Route.href (Route.Project project.project_id) ]
                        [ text project.project_name ]

        pubMedLink =
            case pub.pubmed_id of
                0 ->
                    text "NA"

                _ ->
                    a
                        [ href <|
                            "https://www.ncbi.nlm.nih.gov/pubmed/?term="
                                ++ toString pub.pubmed_id
                        ]
                        [ text <| toString pub.pubmed_id ]
    in
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Title" ]
            , td [] [ text pub.title ]
            ]
        , tr []
            [ th [] [ text "Authors" ]
            , td [] [ text pub.author ]
            ]
        , tr []
            [ th [] [ text "Published" ]
            , td [] [ text pub.pub_date ]
            ]
        , tr []
            [ th [] [ text "PubMed" ]
            , td [] [ pubMedLink ]
            ]
        , tr []
            [ th [] [ text "DOI" ]
            , td [] [ text pub.doi ]
            ]
        , tr []
            [ th [] [ text "Project" ]
            , td [] [ projectLink ]
            ]
        ]


viewFiles : List ProjectFile -> String -> Html Msg
viewFiles files filterType =
    let
        filteredFiles =
            List.filter (\f -> (filterType == "All" || f.project_file_type.type_ == filterType)) files

        count =
            List.length filteredFiles

        label =
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString count)
                        ]

        cols =
            tr []
                [ th [] [ text "Name" ]
                , th [] [ text "Description" ]
                , th [] [ text "Type" ]
                ]

        body =
            let
                tbl =
                    table [ class "table table-condensed" ]
                        [ tbody []
                            (cols :: (List.map viewFile filteredFiles))
                        ]
            in
            if count == 0 then
                text "None"

            else if count < 50 then
                tbl

            else
                div [ class "scrollable" ] [ tbl ]
    in
    case count of
        0 -> text ""

        _ ->
            div []
                [ h2 []
                    [ text "Files "
                    , label
                    ]
                , viewToolbar files
                , body
                ]


viewFile : ProjectFile -> Html msg
viewFile file =
    tr []
        [ td [] [ a [ href (dataCommonsUrl ++ file.file), target "_blank" ] [ text file.file ] ]
        , td [] [ text file.description ]
        , td [] [ text file.project_file_type.type_ ]
        ]


viewToolbar : List ProjectFile -> Html Msg
viewToolbar files =
    let
        types =
            Set.toList <| Set.fromList <| List.map (.project_file_type >> .type_) files

        numTypes =
            List.length types

        btn label =
            button [ class "btn btn-default", onClick (FilterOnFileType label) ] [ text label ]

        lia label =
            li [] [ a [ onClick (FilterOnFileType label) ] [ text label ] ]
    in
    if (numTypes < 2) then
        text ""

    else if (numTypes < 10) then
        div [ class "btn-group margin-bottom", attribute "role" "group", attribute "aria-label" "..."]
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