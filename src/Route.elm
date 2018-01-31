module Route exposing (Route(..), routeToString, fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), (<?>), Parser, oneOf, parseHash, s, string)



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
    | MetaSearch
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
        [ Url.map Apps (s "apps")
        , Url.map App (s "apps" </> Url.int)
        , Url.map Assemblies (s "assemblies")
        , Url.map Assembly (s "assemblies" </> Url.int)
        , Url.map Cart (s "cart")
        , Url.map CombinedAssemblies (s "combined_assemblies")
        , Url.map CombinedAssembly (s "combined_assemblies" </> Url.int)
        , Url.map Contact (s "contact")
        , Url.map Home (s "")
        , Url.map Domains (s "domains")
        , Url.map Domain (s "domains" </> Url.int)
        , Url.map Files (s "files")
        , Url.map Investigator (s "investigators" </> Url.int)
        , Url.map Investigators (s "investigators")
        , Url.map Jobs (s "jobs")
        , Url.map Job (s "jobs" </> Url.string)
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Map (s "map" </> Url.string </> Url.string)
        , Url.map MetaSearch (s "metasearch")
        , Url.map Pubchase (s "pubchase")
        , Url.map Publication (s "publications" </> Url.int)
        , Url.map Publications (s "publications")
        , Url.map Profile (s "profile")
        , Url.map Project (s "projects" </> Url.int)
        , Url.map Projects (s "projects")
        , Url.map ProjectGroup (s "project_groups" </> Url.int)
        , Url.map ProjectGroups (s "project_groups")
        , Url.map Sample (s "samples" </> Url.int)
        , Url.map Samples (s "samples")
        , Url.map Search (s "search" </> Url.string)
        , Url.map TaxonomySearch (s "taxonomy_search" </> Url.string)
        , Url.map ProteinSearch (s "protein_search" </> Url.string)
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

                MetaSearch ->
                    [ "metasearch" ]

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
