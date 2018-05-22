module View.Project exposing (viewInfo, viewActions)

import Html exposing (Html, div, a, table, tbody, th, tr, td, text, button, span)
import Html.Attributes exposing (class, style)
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
    , samples : List { b | sample_id : Int }
    }
  -> Html msg
viewInfo { project_name, project_code, project_type, url, investigators, publications, samples } =
    let
        numSamples =
            List.length samples

        numSamplesText =
            if numSamples == 0 then
                "None"
            else
                toString numSamples
    in
    div [ class "row" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table info-table" ]
                [ tbody []
                    [ tr []
                        [ th [] [ text "Name " ]
                        , td [ class "nowrap" ] [ text project_name ]
                        ]
                    , tr []
                        [ th [] [ text "Code " ]
                        , td [] [ text project_code ]
                        ]
                    , tr []
                        [ th [] [ text "Type " ]
                        , td [ class "nowrap" ] [ text project_type ]
                        ]
                    , tr []
                        [ th [] [ text "URL " ]
                        , td [] [ text url ]
                        ]
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
                        , td [] [ text numSamplesText ]
                        ]
                    ]
                ]
            ]
        ]


viewActions : { a | project_id : Int } -> msg -> Html msg
viewActions { project_id } deleteMsg =
        div [ class "row" ]
            [ div [ class "table-responsive" ]
                [ table [ class "table info-table" ]
                    [ tr []
                        [ td []
                            [ a [ class "btn btn-link", Route.href (Route.Project project_id) ]
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


viewInvestigators : List Investigator -> List (Html msg)
viewInvestigators investigators =
    if List.length investigators == 0 then
        [ text "None" ]
    else
        List.map investigatorLink investigators |> List.intersperse (text ", ")


investigatorLink : Investigator -> Html msg
investigatorLink investigator =
    a [ Route.href (Route.Investigator investigator.investigator_id), class "nowrap" ] [ text investigator.investigator_name ]


viewPublications : List Publication -> Html msg
viewPublications publications =
    case publications of
        [] ->
            text "None"

        _ ->
            text (List.length publications |> toString)
