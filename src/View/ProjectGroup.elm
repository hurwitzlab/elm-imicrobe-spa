module View.ProjectGroup exposing (viewInfo, viewActions)

import Html exposing (Html, div, a, table, tbody, th, tr, td, text, button, span)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Route


viewInfo
  : { a
    | project_group_id : Int
    , group_name : String
    , description : String
    , url : String
    , projects : List { b | project_id : Int, project_name : String }
    , users : List { c | user_id : Int, user_name : String, first_name : String, last_name : String }
    }
  -> Html msg
viewInfo { project_group_id, group_name, description, url, projects, users } =
    div [ class "row" ]
        [ div [ class "table-responsive" ]
            [ table [ class "table info-table" ]
                [ tbody []
                    [ tr []
                        [ th [] [ text "Name " ]
                        , td [ class "nowrap" ]
                            [ a [ Route.href (Route.ProjectGroup project_group_id) ] [ text group_name ] ]
                        ]
                    , tr []
                        [ th [] [ text "Description " ]
                        , td [] [ text description ]
                        ]
                    , tr []
                        [ th [] [ text "URL " ]
                        , td [] [ text url ]
                        ]
                    , tr []
                        [ th [] [ text "Projects " ]
                        , td [] (viewProjects projects)
                        ]
                    , tr []
                        [ th [] [ text "Users " ]
                        , td [] (viewUsers users)
                        ]
                    ]
                ]
            ]
        ]


viewActions : { a | project_group_id : Int } -> Bool -> msg -> Html msg
viewActions { project_group_id } isDeleteable deleteMsg =
        div [ class "row" ]
            [ div [ class "table-responsive" ]
                [ table [ class "table info-table" ]
                    (tr []
                        [ td []
                            [ a [ class "btn btn-link", Route.href (Route.ProjectGroup project_group_id) ]
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


viewProjects : List { a | project_id : Int, project_name : String } -> List (Html msg)
viewProjects projects =
    if List.length projects == 0 then
        [ text "None" ]
    else
        List.map projectLink projects |> List.intersperse (text ", ")


projectLink : { a | project_id : Int, project_name : String } -> Html msg
projectLink project =
    a [ Route.href (Route.Project project.project_id), class "nowrap" ] [ text project.project_name ]


viewUsers : List { a | user_id : Int, user_name : String, first_name : String, last_name : String } -> List (Html msg)
viewUsers users =
    if users == [] then
        [ text "None" ]
    else
        List.map viewUser users |> List.intersperse (text ", ")


viewUser : { a | user_id : Int, user_name : String, first_name : String, last_name : String } -> Html msg
viewUser user =
    let
        displayName =
            user.first_name ++ " " ++ user.last_name
    in
    span [ class "nowrap" ] [ text displayName ]
