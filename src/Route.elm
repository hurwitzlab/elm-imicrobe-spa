module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, oneOf, parseHash, s, string)


-- ROUTING --


type Route
    = Home
    | About
    | Investigators
    | Investigator Int


routeMather : Parser (Route -> a) a
routeMather =
    oneOf
        [ Url.map Home (s "")
        , Url.map About (s "about")
        , Url.map Investigators (s "investigators")
        , Url.map Investigator (s "investigator" </> Url.int)

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

                Investigator id ->
                    [ "investigator", toString id ]

                Investigators ->
                    [ "investigators" ]

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
