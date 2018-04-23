module View.Sample exposing (viewInfo, viewActions)

import Html exposing (Html, div, table, tbody, th, tr, td, text, button, span, a)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Route


viewInfo : { a | sample_name : String, sample_acc : String, sample_type : String, project : { b | project_id : Int, project_name : String }, sample_files : List { c | sample_file_id : Int } } -> Html msg
viewInfo { sample_name, sample_acc, sample_type, project, sample_files } =
    let
        numFiles =
            List.length sample_files

        numFilesText =
            if numFiles == 0 then
                "None"
            else
                toString numFiles
    in
    div [ class "row" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table info-table" ]
                [ tbody []
                    [ tr []
                        [ th [] [ text "Name " ]
                        , td [] [ text sample_name ]
                        ]
                    , tr []
                        [ th [] [ text "Code " ]
                        , td [] [ text sample_acc ]
                        ]
                    , tr []
                        [ th [] [ text "Type " ]
                        , td [] [ text sample_type ]
                        ]
                  , tr []
                      [ th [] [ text "Project " ]
                      , td []
                        [ a [ Route.href (Route.Project project.project_id) ] [ text project.project_name ] ]
                      ]
                  , tr []
                      [ th [] [ text "Files " ]
                      , td [] [ text numFilesText ]
                      ]
                    ]
                ]
            ]
        ]


viewActions : { a | sample_id : Int } -> msg -> Html msg
viewActions { sample_id } deleteMsg =
    div [ class "row" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table info-table" ]
                [ tbody []
                    [ tr []
                        [ td []
                            [ a [ class "btn btn-link", Route.href (Route.Sample sample_id) ]
                                [ span [ class "glyphicon glyphicon-share-alt" ] [], text " Open"
                                ]
                            ]
                        ]
                    , tr []
                        [ td []
                            [ button [ class "btn btn-link", onClick deleteMsg ]
                                [ span [ class "glyphicon glyphicon-trash" ] [], text " Delete"
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
