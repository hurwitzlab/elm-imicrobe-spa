module View.Page exposing (ActivePage(..), layout)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Data.Session as Session exposing (Session)



type ActivePage
    = About
    | App
    | Apps
    | Assembly
    | Assemblies
    | Cart
    | CombinedAssembly
    | CombinedAssemblies
    | Domains
    | Domain
    | Files
    | Home
    | Investigator
    | Investigators
    | Job
    | Jobs
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
layout : Bool -> Session -> ActivePage -> Html msg -> Html msg
layout isLoading session page content =
    div []
        [ viewHeader page isLoading session
        , div [] [ content ]
        , viewFooter
        ]


viewHeader : ActivePage -> Bool -> Session -> Html msg
viewHeader page isLoading session =
    let
        profile = session.profile

        loginMenuItem =
            case profile of
                Nothing ->
                    li [] [ a [ Route.href Route.Login ] [ text "Login" ] ]

                Just profile ->
                    li [ class "dropdown" ]
                        [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-expanded" "false" ]
                            [ text (profile.first_name ++ " " ++ profile.last_name)
                            , span [ class "caret" ] []
                            ]
                        , ul [ class "dropdown-menu", style [ ( "role", "menu" ) ] ]
                            [ li [] [ a [ Route.href Route.Profile ] [ text "Profile" ] ]
                            , li [] [ a [ Route.href Route.Logout ] [ text "Sign out" ] ]
                            ]
                        ]


    in
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
                            [ li [] [ a [ Route.href Route.Projects ] [ text "Projects" ] ]
                            , li [] [ a [ Route.href Route.ProjectGroups ] [ text "Project Groups" ] ]
                            , li [] [ a [ Route.href Route.Investigators ] [ text "Investigators" ] ]
                            , li [] [ a [ Route.href Route.Domains ] [ text "Domains" ] ]
                            , li [] [ a [ Route.href Route.Assemblies ] [ text "Assemblies" ] ]
                            , li [] [ a [ Route.href Route.CombinedAssemblies ] [ text "CombinedAssemblies" ] ]
                            , li [] [ a [ Route.href Route.Samples ] [ text "Samples" ] ]
                            , li [] [ a [ Route.href Route.Publications ] [ text "Publications" ] ]
                            , li [] [ a [ Route.href Route.Pubchase ] [ text "Recommended Readings" ] ]
                            , li [] [ a [ Route.href Route.Apps ] [ text "Apps" ] ]
                            , li [] [ a [ Route.href Route.Jobs ] [ text "Jobs" ] ]
                            , li [] [ a [ Route.href Route.Cart ] [ text "Cart" ] ]
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
                    , loginMenuItem
                    ]
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [] []
        ]
