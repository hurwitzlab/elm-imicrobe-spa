module View.Page exposing (ActivePage(..))

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


