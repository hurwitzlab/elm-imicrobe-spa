module View.Sample exposing (viewInfo)

import Html exposing (Html, div, table, th, tr, td, text)
import Html.Attributes exposing (class)


viewInfo : { a | sample_name : String, sample_acc : String, sample_type : String } -> Html msg
viewInfo { sample_name, sample_acc, sample_type } =
    div [ class "col-md-4" ]
        [ table [ class "info-table" ]
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
--            , tr []
--                [ th [] [ text "Project " ]
--                , td [] [ text sample.project.project_name ]
--                ]
--            , tr []
--                [ th [] [ text "Files " ]
--                , td [] [ text "" ]
--                ]
            ]
        ]