module View.Project exposing (viewInfo, viewActions)

import Html exposing (Html, div, a, table, th, tr, td, text, button, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Data.Project exposing (Investigator, Publication, Sample)
import Route


viewInfo
  : { a
    | project_name : String
    , project_code : String
    , project_type : String
    , url : String
    , investigators : List Investigator
    , publications : List Publication
    , sample_count : Int
    }
  -> Html msg
viewInfo { project_name, project_code, project_type, url, investigators, publications, sample_count } =
    let
        numSamples =
            if sample_count == 0 then
                "None"
            else
                toString sample_count
    in
    div [ class "row" ]
        [ div [ class "col-md-4" ]
            [ table [ class "info-table" ]
                [ tr []
                    [ th [] [ text "Name " ]
                    , td [] [ text project_name ]
                    ]
                , tr []
                    [ th [] [ text "Code " ]
                    , td [] [ text project_code ]
                    ]
                , tr []
                    [ th [] [ text "Type " ]
                    , td [] [ text project_type ]
                    ]
                , tr []
                    [ th [] [ text "URL " ]
                    , td [] [ text url ]
                    ]
--              , tr []
--                  [ th [] [ text "PI " ]
--                  , td [] [ text project.pi ]
--                  ]
                , tr []
                    [ th [] [ text "Investigators " ]
                    , td [] (viewInvestigators investigators)
                    ]
                , tr []
                    [ th [] [ text "Publications " ]
                    , td [] [ viewPublications publications ]
                    ]
                , tr []
                    [ th [] [ text "Samples " ]
                    , td [] [ text numSamples ]
                    ]
                ]
            ]
        ]


viewActions : msg -> Html msg
viewActions deleteMsg =
        div [ class "row" ]
            [ div [ class "col-md-4" ]
                [ table [ class "info-table" ]
                    [ tr []
                        [ td []
                            [ button [ class "btn btn-link btn-xs", onClick deleteMsg ]
                                [ span [ class "glyphicon glyphicon-trash" ] [], text " Delete"
                                ]
                            ]
                        ]
                    ]
                ]
            ]


viewInvestigators : List Investigator -> List (Html msg)
viewInvestigators investigators =
    if List.length investigators == 0 then
        [ text "None" ]
    else
        List.map investigatorLink investigators |> List.intersperse (text ", ")


investigatorLink : Investigator -> Html msg
investigatorLink investigator =
    a [ Route.href (Route.Investigator investigator.investigator_id) ] [ text investigator.investigator_name ]


viewPublications : List Publication -> Html msg
viewPublications publications =
    case publications of
        [] ->
            text "None"

        _ ->
            text (List.length publications |> toString)
