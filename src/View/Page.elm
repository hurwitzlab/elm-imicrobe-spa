module View.Page exposing (ActivePage(..), layout)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)


type ActivePage
    = About
    | App
    | Apps
    | Assembly
    | Assemblies
    | CombinedAssembly
    | CombinedAssemblies
    | Domains
    | Domain
    | Home
    | Investigator
    | Investigators
    | Other
    | Pubchase
    | Publication
    | Publications
    | Profile
    | Project
    | Projects
    | ProjectGroups
    | ProjectGroup
    | Sample
    | Samples
    | MetaSearch
    | Search
    | Map


{-| Take a page's Html and layout it with a header and footer.

isLoading can be used to slow loading during slow transitions

-}
layout : Bool -> ActivePage -> Html msg -> Html msg
layout isLoading page content =
    div []
        [ viewHeader page isLoading
        , div [] [ content ]
        , viewFooter
        ]


viewHeader : ActivePage -> Bool -> Html msg
viewHeader page isLoading =
    nav [ class "navbar navbar-default navbar-static-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-collapse collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    [ li []
                        [ a [ Route.href Route.Home ]
                            [ img [ src "/img/nav-header.png" ] [] ]
                        ]
                    , li [ class "dropdown" ]
                        [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-expanded" "false" ]
                            [ text "Search"
                            , span [ class "caret" ] []
                            ]
                        , ul
                            [ class "dropdown-menu", style [ ( "role", "menu" ) ] ]
                            [ li [] [ a [ Route.href Route.Search ] [ text "General Search" ] ]
                            , li [] [ a [ Route.href Route.MetaSearch ] [ text "Sample Search" ] ]
                            ]
                        ]
                    , li [ class "dropdown" ]
                        [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-expanded" "false" ]
                            [ text "Browse"
                            , span [ class "caret" ] []
                            ]
                        , ul
                            [ class "dropdown-menu", style [ ( "role", "menu" ) ] ]
                            [ li [] [ a [ Route.href Route.Investigators ] [ text "Investigators" ] ]
                            , li [] [ a [ Route.href Route.Domains ] [ text "Domains" ] ]
                            , li [] [ a [ Route.href Route.Projects ] [ text "Projects" ] ]
                            , li [] [ a [ Route.href Route.ProjectGroups ] [ text "Project Groups" ] ]
                            , li [] [ a [ Route.href Route.Publications ] [ text "Publications" ] ]
                            , li [] [ a [ Route.href Route.Samples ] [ text "Samples" ] ]
                            , li [] [ a [ Route.href Route.Assemblies ] [ text "Assemblies" ] ]
                            , li [] [ a [ Route.href Route.CombinedAssemblies ] [ text "CombinedAssemblies" ] ]
                            , li [] [ a [ Route.href Route.Pubchase ] [ text "Recommended Readings" ] ]
                            , li [] [ a [ Route.href Route.Apps ] [ text "Apps" ] ]
                            ]
                        ]
                    , li []
                        [ a [ href "ftp://ftp.imicrobe.us" ]
                            [ text "Downloads" ]
                        ]
                    , li []
                        [ a [ Route.href Route.About ]
                            [ text "About" ]
                        ]
                    , li []
                        [ a [ Route.href Route.Login ]
                            [ text "Login" ]
                        ]
                    ]
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [] []
        ]
