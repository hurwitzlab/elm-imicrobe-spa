module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | About
    | Domains
    | Domain Int
    | Investigator Int
    | Investigators
    | Login
    | Publications
    | Profile String
    | Project Int
    | Projects
    | ProjectGroups
    | ProjectGroup Int
    | Sample Int
    | Samples
    | MetaSearch
    | Search
    | Map String String


routeMather : Parser (Route -> a) a
routeMather =
    oneOf
        [ Url.map About (s "about")
        , Url.map Home (s "")
        , Url.map Domains (s "domains")
        , Url.map Domain (s "domains" </> Url.int)
        , Url.map Investigator (s "investigators" </> Url.int)
        , Url.map Investigators (s "investigators")
        , Url.map Login (s "login")
        , Url.map Publications (s "publications")
        , Url.map Profile (s "profile" </> Url.string)
        , Url.map Project (s "projects" </> Url.int)
        , Url.map Projects (s "projects")
        , Url.map ProjectGroup (s "project_groups" </> Url.int)
        , Url.map ProjectGroups (s "project_groups")
        , Url.map Sample (s "samples" </> Url.int)
        , Url.map Samples (s "samples")
        , Url.map MetaSearch (s "metasearch")
        , Url.map Search (s "search")
        , Url.map Map (s "map" </> Url.string </> Url.string)

        --    When needing parameters on the form base/item/3
        --    , Url.map Item (s "item" </> Item.itemParser)
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pagePath =
            case page of
                About ->
                    [ "about" ]

                Home ->
                    []

                Domains ->
                    [ "domains" ]

                Domain id ->
                    [ "domains", toString id ]

                Investigator id ->
                    [ "investigators", toString id ]

                Investigators ->
                    [ "investigators" ]

                Login ->
                    [ "login" ]

                Publications ->
                    [ "publications" ]

                Profile token ->
                    [ "profile", token ]

                Project id ->
                    [ "projects", toString id ]

                Projects ->
                    [ "projects" ]

                ProjectGroup id ->
                    [ "project_groups", toString id ]

                ProjectGroups ->
                    [ "project_groups" ]

                Sample id ->
                    [ "samples", toString id ]

                Samples ->
                    [ "samples" ]

                MetaSearch ->
                    [ "metasearch" ]

                Search ->
                    [ "search" ]

                Map lat lng ->
                    [ "map", lat, lng ]

        --    When needing parameters on the form base/item/3
        --                    Item ->
        --                    [ "item", Item.itemToString item ]
    in
    "#/" ++ String.join "/" pagePath



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash routeMather location
