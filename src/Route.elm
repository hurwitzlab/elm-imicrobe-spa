module Route exposing (Route(..), routeToString, fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, map, s, string, int, intParam)



type Route
    = Home
    | Apps
    | App Int
    | Assemblies
    | Assembly Int
    | Cart
    | CombinedAssemblies
    | CombinedAssembly Int
    | Contact
    | Dashboard
    | Domains
    | Domain Int
    | Files
    | Investigator Int
    | Investigators
    | Jobs
    | Job String
    | Login
    | Logout
    | Map String String
--    | MetaSearch
    | Profile
    | Project Int
    | Projects
    | ProjectGroups
    | ProjectGroup Int
    | Pubchase
    | Publication Int
    | Publications
    | Sample Int
    | Samples
    | Search String
    | TaxonomySearch String
    | ProteinSearch String


routeMather : Parser (Route -> a) a
routeMather =
    oneOf
        [ map Apps (s "apps")
        , map App (s "apps" </> int)
        , map Assemblies (s "assemblies")
        , map Assembly (s "assemblies" </> int)
        , map Cart (s "cart")
        , map CombinedAssemblies (s "combined_assemblies")
        , map CombinedAssembly (s "combined_assemblies" </> int)
        , map Contact (s "contact")
        , map Dashboard (s "dashboard")
        , map Domains (s "domains")
        , map Domain (s "domains" </> int)
        , map Files (s "files")
        , map Home (s "")
        , map Investigator (s "investigators" </> int)
        , map Investigators (s "investigators")
        , map Jobs (s "jobs")
        , map Job (s "jobs" </> string)
        , map Login (s "login")
        , map Logout (s "logout")
        , map Map (s "map" </> string </> string)
--        , map MetaSearch (s "metasearch")
        , map Pubchase (s "pubchase")
        , map Publication (s "publications" </> int)
        , map Publications (s "publications")
        , map Profile (s "profile")
        , map Project (s "projects" </> int) -- map Project (s "projects" </> int <?> intParam "edit") -- query param parsing not working
        , map Projects (s "projects")
        , map ProjectGroup (s "project_groups" </> int)
        , map ProjectGroups (s "project_groups")
        , map Sample (s "samples" </> int)
        , map Samples (s "samples")
        , map Search (s "search" </> string)
        , map TaxonomySearch (s "taxonomy_search" </> string)
        , map ProteinSearch (s "protein_search" </> string)
        ]


routeToString : Route -> String
routeToString page =
    let
        pagePath =
            case page of
                Apps ->
                    [ "apps" ]

                App id ->
                    [ "apps", toString id ]

                Assemblies ->
                    [ "assemblies" ]

                Assembly id ->
                    [ "assemblies", toString id ]

                Cart ->
                    [ "cart" ]

                CombinedAssemblies ->
                    [ "combined_assemblies" ]

                CombinedAssembly id ->
                    [ "combined_assemblies", toString id ]

                Contact ->
                    [ "contact" ]

                Dashboard ->
                    [ "dashboard" ]

                Home ->
                    []

                Files ->
                    [ "files" ]

                Domains ->
                    [ "domains" ]

                Domain id ->
                    [ "domains", toString id ]

                Investigator id ->
                    [ "investigators", toString id ]

                Investigators ->
                    [ "investigators" ]

                Jobs ->
                    [ "jobs" ]

                Job id ->
                    [ "jobs", id ]

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Map lat lng ->
                    [ "map", lat, lng ]

--                MetaSearch ->
--                    [ "metasearch" ]

                Pubchase ->
                    [ "pubchase" ]

                Publication id ->
                    [ "publications", toString id ]

                Publications ->
                    [ "publications" ]

                Profile ->
                    [ "profile" ]

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

                Search query ->
                    [ "search", query ]

                TaxonomySearch query ->
                    [ "taxonomy_search", query ]

                ProteinSearch query ->
                    [ "protein_search", query ]
    in
    "#/" ++ String.join "/" pagePath


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
