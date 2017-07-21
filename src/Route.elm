module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | About
    | Investigator Int
    | Investigators
    | Login
    | Profile String
    | Project Int
    | Projects
    | Sample Int
    | Samples
    | MetaSearch
    | Search


routeMather : Parser (Route -> a) a
routeMather =
    oneOf
        [ Url.map About (s "about")
        , Url.map Home (s "")
        , Url.map Investigator (s "investigators" </> Url.int)
        , Url.map Investigators (s "investigators")
        , Url.map Login (s "login")
        , Url.map Profile (s "profile" </> Url.string)
        , Url.map Project (s "projects" </> Url.int)
        , Url.map Projects (s "projects")
        , Url.map Sample (s "samples" </> Url.int)
        , Url.map Samples (s "samples")
        , Url.map MetaSearch (s "metasearch")
        , Url.map Search (s "search")

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

                Investigator id ->
                    [ "investigators", toString id ]

                Investigators ->
                    [ "investigators" ]

                Login ->
                    [ "login" ]

                Profile token ->
                    [ "profile", token ]

                Project id ->
                    [ "projects", toString id ]

                Projects ->
                    [ "projects" ]

                Sample id ->
                    [ "samples", toString id ]

                Samples ->
                    [ "samples" ]

                MetaSearch ->
                    [ "metasearch" ]

                Search ->
                    [ "search" ]

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
