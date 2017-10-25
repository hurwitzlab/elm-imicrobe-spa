module View.Page exposing (ActivePage(..), layout)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Data.Session as Session exposing (Session)
import Data.Cart as Cart exposing (size)



type ActivePage
    = App
    | Apps
    | Assembly
    | Assemblies
    | Cart
    | CombinedAssembly
    | CombinedAssemblies
    | Contact
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
                            [ text "My Account"
                            , span [ class "caret" ] []
                            ]
                        , ul [ class "dropdown-menu", style [ ( "role", "menu" ) ] ]
                            [ li [] [ a [ Route.href Route.Profile ] [ text "Profile" ] ]
                            , li [] [ a [ Route.href Route.Logout ] [ text "Sign out" ] ]
                            ]
                        ]

        numItemsInCart =
            Cart.size session.cart

        cartButton =
            let
                label =
                    case numItemsInCart of
                        0 -> []

                        _ ->
                            [ span [ class "gray absolute" ] [ text (toString numItemsInCart) ] ]
            in
            div [ class "pull-right", style [("padding-top", "21px"), ("margin-left", "2em")] ]
                [ a [ Route.href Route.Cart ]
                    (span [ class "icon-button glyphicon glyphicon-shopping-cart" ] [] :: label)
                ]

        helpButton =
            div [ class "pull-right", style [("padding-top", "21px"), ("margin-left", "2em")] ]
                [ a [ Route.href Route.Contact ]
                    [ span [ class "icon-button glyphicon glyphicon-question-sign" ] [] ]
                ]
    in
    nav [ class "navbar navbar-default navbar-static-top", style [("padding-top", "10px")] ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ a [ class "navbar-brand", Route.href Route.Home ]
                    [ img [ src "/img/nav-header.png" ] [] ]
                ]
            , div [ class "navbar-collapse collapse" ]
                [ ul [ class "nav navbar-nav" ]
                    [ li [ class "dropdown" ]
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
                            , li [] [ a [ Route.href Route.Projects ] [ text "Projects" ] ]
--                            , li [] [ a [ Route.href Route.ProjectGroups ] [ text "Project Groups" ] ]
--                            , li [] [ a [ Route.href Route.Domains ] [ text "Domains" ] ]
--                            , li [] [ a [ Route.href Route.Assemblies ] [ text "Assemblies" ] ]
--                            , li [] [ a [ Route.href Route.CombinedAssemblies ] [ text "CombinedAssemblies" ] ]
                            , li [] [ a [ Route.href Route.Samples ] [ text "Samples" ] ]
                            , li [] [ a [ Route.href Route.Publications ] [ text "Publications" ] ]
                            , li [] [ a [ Route.href Route.Pubchase ] [ text "Recommended Readings" ] ]
                            ]
                        ]
                    , li [ class "dropdown" ]
                        [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-expanded" "false" ]
                            [ text "Tools"
                            , span [ class "caret" ] []
                            ]
                        , ul
                            [ class "dropdown-menu", style [ ( "role", "menu" ) ] ]
                            [ li [] [ a [ Route.href Route.Apps ] [ text "Apps" ] ]
                            , li [] [ a [ Route.href Route.Jobs ] [ text "Jobs" ] ]
                            , li [] [ a [ Route.href Route.Cart ] [ text ("Cart (" ++ (toString numItemsInCart) ++ ")") ] ]
                            ]
                        ]
                    , li []
                        [ a [ href "ftp://ftp.imicrobe.us" ]
                            [ text "Download" ]
                        ]
                    , loginMenuItem
                    ]
                , helpButton
                , cartButton
                , searchBar
                ]
            ]
        ]


viewFooter : Html msg
viewFooter =
    footer [] []


searchBar : Html msg
searchBar =
    div [ class "pull-right", style [("padding-top", "10px")] ]
        [ Html.form [ class "navbar-form" ]
            [ div [ class "input-group" ]
                [ input [ class "form-control", placeholder "Search" ] []
                , div [ class "input-group-btn" ]
                    [ button [ class "btn btn-default", type_ "submit" ]
                        [ i [ class "glyphicon glyphicon-search" ] []
                        ]
                    ]
                ]
            ]
        ]