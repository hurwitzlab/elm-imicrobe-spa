module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | About
    | Login
    | Profile String
    | Investigators
    | Investigator Int
    | Projects
    | Project Int
    | Samples
    | Sample Int
    | Search
    | Map String String


routeMather : Parser (Route -> a) a
routeMather =
    oneOf
        [ Url.map Home (s "")
        , Url.map About (s "about")
        , Url.map Login (s "login")
        , Url.map Profile (s "profile" </> Url.string)
        , Url.map Investigators (s "investigators")
        , Url.map Investigator (s "investigators" </> Url.int)
        , Url.map Projects (s "projects")
        , Url.map Project (s "projects" </> Url.int)
        , Url.map Samples (s "samples")
        , Url.map Sample (s "samples" </> Url.int)
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
                Home ->
                    []

                About ->
                    [ "about" ]

                Login ->
                    [ "login" ]

                Profile token ->
                    [ "profile", token ]

                Investigator id ->
                    [ "investigators", toString id ]

                Investigators ->
                    [ "investigators" ]

                Projects ->
                    [ "projects" ]

                Project id ->
                    [ "projects", toString id ]

                Samples ->
                    [ "samples" ]

                Sample id ->
                    [ "samples", toString id ]

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
