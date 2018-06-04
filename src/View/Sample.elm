module View.Sample exposing (viewInfo, viewActions)

import Html exposing (Html, div, table, tbody, th, tr, td, text, button, span, a, col, colgroup)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Route


viewInfo : { a | sample_id : Int, sample_name : String, sample_acc : String, sample_type : String, sample_file_count : Int, project : { b | project_id : Int, project_name : String } } -> Html msg
viewInfo { sample_id, sample_name, sample_acc, sample_type, sample_file_count, project } =
    let
        numFilesText =
            if sample_file_count == 0 then
                "None"
            else
                toString sample_file_count
    in
    div [ class "row" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table info-table" ]
                [ tbody []
                    [ colgroup []
                        [ col [ class "col-md-1" ] [] ]
                    , tr []
                        [ th [] [ text "Name " ]
                        , td [ class "nowrap" ]
                            [ a [ Route.href (Route.Sample sample_id) ] [ text sample_name ] ]
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
                      , td [ class "nowrap" ]
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


viewActions : { a | sample_id : Int } -> Bool -> msg -> Html msg
viewActions { sample_id } isDeleteable deleteMsg =
    div [ class "row" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table info-table" ]
                [ tbody []
                    ( tr []
                        [ td []
                            [ a [ class "btn btn-link", Route.href (Route.Sample sample_id) ]
                                [ span [ class "glyphicon glyphicon-share-alt" ] [], text " Open"
                                ]
                            ]
                        ] ::
                    (if isDeleteable then
                        [ tr []
                            [ td []
                                [ button [ class "btn btn-link", onClick deleteMsg ]
                                    [ span [ class "glyphicon glyphicon-trash" ] [], text " Delete"
                                    ]
                                ]
                            ]
                        ]
                    else
                        []
                    )
                    )
                ]
            ]
        ]
