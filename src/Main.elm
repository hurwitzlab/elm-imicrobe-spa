port module Main exposing (..)

import Data.Session exposing (Session)
import Debug exposing (log)
import Html exposing (..)
import Navigation exposing (Location)
import OAuth
import OAuth.Implicit
import Page.About as About
import Page.App as App
import Page.Apps as Apps
import Page.Assembly as Assembly
import Page.Assemblies as Assemblies
import Page.CombinedAssembly as CombinedAssembly
import Page.CombinedAssemblies as CombinedAssemblies
import Page.Domain as Domain
import Page.Domains as Domains
import Page.Error as Error exposing (PageLoadError)
import Page.Home as Home
import Page.Investigator as Investigator
import Page.Investigators as Investigators
import Page.Map as Map
import Page.MetaSearch as MetaSearch
import Page.NotFound as NotFound
import Page.Profile as Profile
import Page.Project as Project
import Page.ProjectGroup as ProjectGroup
import Page.ProjectGroups as ProjectGroups
import Page.Projects as Projects
import Page.Pubchase as Pubchase
import Page.Publication as Publication
import Page.Publications as Publications
import Page.Sample as Sample
import Page.Samples as Samples
import Page.Search as Search
import Route exposing (..)
import Set
import Task
import Util exposing ((=>))
import View.Page as Page exposing (ActivePage)


---- MODEL ----


type alias Model =
    { pageState : PageState
    , session : Session
    , oauth :
        { authEndpoint : String
        , clientId : String
        , redirectUri : String
        }
    , token : Maybe OAuth.Token
    , error : Maybe String
    }


type Page
    = About About.Model
    | Blank
    | Apps Apps.Model
    | App Int App.Model
    | Assemblies Assemblies.Model
    | Assembly Int Assembly.Model
    | CombinedAssemblies CombinedAssemblies.Model
    | CombinedAssembly Int CombinedAssembly.Model
    | Domain Int Domain.Model
    | Domains Domains.Model
    | Error PageLoadError
    | Home Home.Model
    | Investigator Int Investigator.Model
    | Investigators Investigators.Model
    | Map String String Map.Model
    | MetaSearch MetaSearch.Model
    | NotFound
    | Profile String Profile.Model
    | Project Int Project.Model
    | ProjectGroup Int ProjectGroup.Model
    | ProjectGroups ProjectGroups.Model
    | Projects Projects.Model
    | Pubchase Pubchase.Model
    | Publication Int Publication.Model
    | Publications Publications.Model
    | Sample Int Sample.Model
    | Samples Samples.Model
    | Search Search.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



---- UPDATE ----


type Msg
    = AboutLoaded (Result PageLoadError About.Model)
    | AboutMsg About.Msg
    | AddToCart Int
    | AppLoaded Int (Result PageLoadError App.Model)
    | AppMsg App.Msg
    | AppsLoaded (Result PageLoadError Apps.Model)
    | AppsMsg Apps.Msg
    | AssemblyLoaded Int (Result PageLoadError Assembly.Model)
    | AssemblyMsg Assembly.Msg
    | AssembliesLoaded (Result PageLoadError Assemblies.Model)
    | AssembliesMsg Assemblies.Msg
    | Authorize (Result PageLoadError Home.Model)
    | CombinedAssemblyLoaded Int (Result PageLoadError CombinedAssembly.Model)
    | CombinedAssemblyMsg CombinedAssembly.Msg
    | CombinedAssembliesLoaded (Result PageLoadError CombinedAssemblies.Model)
    | CombinedAssembliesMsg CombinedAssemblies.Msg
    | DomainLoaded Int (Result PageLoadError Domain.Model)
    | DomainMsg Domain.Msg
    | DomainsLoaded (Result PageLoadError Domains.Model)
    | DomainsMsg Domains.Msg
    | EmptyCart
    | HomeLoaded (Result PageLoadError Home.Model)
    | HomeMsg Home.Msg
    | InvestigatorLoaded Int (Result PageLoadError Investigator.Model)
    | InvestigatorMsg Investigator.Msg
    | InvestigatorsLoaded (Result PageLoadError Investigators.Model)
    | InvestigatorsMsg Investigators.Msg
    | MapLoaded String String (Result PageLoadError Map.Model)
    | MapMsg Map.Msg
    | MetaSearchLoaded (Result PageLoadError MetaSearch.Model)
    | MetaSearchMsg MetaSearch.Msg
    | ProfileLoaded String (Result PageLoadError Profile.Model)
    | ProfileMsg Profile.Msg
    | ProjectGroupLoaded Int (Result PageLoadError ProjectGroup.Model)
    | ProjectGroupMsg ProjectGroup.Msg
    | ProjectGroupsLoaded (Result PageLoadError ProjectGroups.Model)
    | ProjectGroupsMsg ProjectGroups.Msg
    | ProjectLoaded Int (Result PageLoadError Project.Model)
    | ProjectMsg Project.Msg
    | ProjectsLoaded (Result PageLoadError Projects.Model)
    | ProjectsMsg Projects.Msg
    | PubchaseLoaded (Result PageLoadError Pubchase.Model)
    | PubchaseMsg Pubchase.Msg
    | PublicationLoaded Int (Result PageLoadError Publication.Model)
    | PublicationMsg Publication.Msg
    | PublicationsLoaded (Result PageLoadError Publications.Model)
    | PublicationsMsg Publications.Msg
    | RemoveFromCart Int
    | SampleLoaded Int (Result PageLoadError Sample.Model)
    | SampleMsg Sample.Msg
    | SamplesLoaded (Result PageLoadError Samples.Model)
    | SamplesMsg Samples.Msg
    | SearchLoaded (Result PageLoadError Search.Model)
    | SearchMsg Search.Msg
    | SetRoute (Maybe Route)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Task.attempt toMsg task

        error =
            pageError model
    in
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } => Cmd.none

        Just Route.About ->
            transition AboutLoaded About.init

        Just (Route.App id) ->
            transition (AppLoaded id) (App.init id)

        Just Route.Apps ->
            transition AppsLoaded Apps.init

        Just (Route.Assembly id) ->
            transition (AssemblyLoaded id) (Assembly.init id)

        Just Route.Assemblies ->
            transition AssembliesLoaded Assemblies.init

        Just (Route.CombinedAssembly id) ->
            transition (CombinedAssemblyLoaded id) (CombinedAssembly.init id)

        Just Route.CombinedAssemblies ->
            transition CombinedAssembliesLoaded CombinedAssemblies.init

        Just Route.Home ->
            transition HomeLoaded (Home.init model.session)

        Just Route.Domains ->
            transition DomainsLoaded Domains.init

        Just (Route.Domain id) ->
            transition (DomainLoaded id) (Domain.init id)

        Just (Route.Investigator id) ->
            transition (InvestigatorLoaded id) (Investigator.init id)

        Just Route.Investigators ->
            transition InvestigatorsLoaded Investigators.init

        Just Route.Login ->
            transition Authorize (Home.init model.session)

        Just Route.Pubchase ->
            transition PubchaseLoaded Pubchase.init

        Just Route.Publications ->
            transition PublicationsLoaded Publications.init

        Just (Route.Publication id) ->
            transition (PublicationLoaded id) (Publication.init id)

        Just (Route.Profile token) ->
            transition (ProfileLoaded token) (Profile.init token)

        Just (Route.Project id) ->
            transition (ProjectLoaded id) (Project.init id)

        Just Route.Projects ->
            transition ProjectsLoaded Projects.init

        Just (Route.ProjectGroup id) ->
            transition (ProjectGroupLoaded id) (ProjectGroup.init id)

        Just Route.ProjectGroups ->
            transition ProjectGroupsLoaded ProjectGroups.init

        Just (Route.Sample id) ->
            transition (SampleLoaded id) (Sample.init id)

        Just Route.Samples ->
            transition SamplesLoaded Samples.init

        Just Route.MetaSearch ->
            transition MetaSearchLoaded MetaSearch.init

        Just Route.Search ->
            transition SearchLoaded Search.init

        Just (Route.Map lat lng) ->
            transition (MapLoaded lat lng) (Map.init lat lng)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


pageError : Model -> ActivePage -> String -> ( Model, Cmd msg )
pageError model activePage errorMessage =
    let
        error =
            Error.pageLoadError activePage errorMessage
    in
    { model | pageState = Loaded (Error error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


--updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
--updatePage page msg model =
--    let
--        session =
--            model.session
--
--        toPage toModel toMsg subUpdate subMsg subModel =
--            let
--                ( newModel, newCmd ) =
--                    subUpdate subMsg subModel
--            in
--            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )
--
--        error =
--            pageError model
--    in
--    case ( msg, page ) of
--        ( SetRoute route, _ ) ->
--            setRoute route model
--
--        ( AddToCart id, _ ) ->
--            let
--                newCart =
--                    Set.insert id model.session.cart
--            in
--            ( { model | session = { cart = newCart } }, Cmd.none )
--
--        ( Authorize (Ok subModel), _ ) ->
--            model
--                ! [ OAuth.Implicit.authorize
--                        { clientId = model.oauth.clientId
--                        , redirectUri = model.oauth.redirectUri
--                        , responseType = OAuth.Token
--                        , scope = [ "PRODUCTION" ]
--                        , state = Just "000"
--                        , url = model.oauth.authEndpoint
--                        }
--                  ]
--
--        ( AboutLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (About subModel) } => Cmd.none
--
--        ( AboutLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( AppLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (App id subModel) } => Cmd.none
--
--        ( AppLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( AppsLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Apps subModel) } => Cmd.none
--
--        ( AppsLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( AssemblyLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Assembly id subModel) } => Cmd.none
--
--        ( AssemblyLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( AssembliesLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Assemblies subModel) } => Cmd.none
--
--        ( AssembliesLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( AssembliesMsg subMsg, Assemblies subModel ) ->
--            toPage Assemblies AssembliesMsg Assemblies.update subMsg subModel
--
--        ( CombinedAssemblyLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (CombinedAssembly id subModel) } => Cmd.none
--
--        ( CombinedAssemblyLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( CombinedAssembliesLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (CombinedAssemblies subModel) } => Cmd.none
--
--        ( CombinedAssembliesLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( CombinedAssembliesMsg subMsg, CombinedAssemblies subModel ) ->
--            toPage CombinedAssemblies CombinedAssembliesMsg CombinedAssemblies.update subMsg subModel
--
--        ( DomainsLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Domains subModel) } => Cmd.none
--
--        ( DomainsLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( DomainsMsg subMsg, Domains subModel ) ->
--            toPage Domains DomainsMsg Domains.update subMsg subModel
--
--        ( DomainLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Domain id subModel) } => Cmd.none
--
--        ( DomainLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( EmptyCart, _ ) ->
--            ( { model | session = { cart = Set.empty } }, Cmd.none )
--
--        ( HomeLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Home subModel) } => Cmd.none
--
--        ( HomeLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( InvestigatorLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Investigator id subModel) } => Cmd.none
--
--        ( InvestigatorLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( InvestigatorsLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Investigators subModel) } => Cmd.none
--
--        ( InvestigatorsLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( InvestigatorsMsg subMsg, Investigators subModel ) ->
--            toPage Investigators InvestigatorsMsg Investigators.update subMsg subModel
--
--        ( MetaSearchLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (MetaSearch subModel) } => Cmd.none
--
--        ( MetaSearchLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( MetaSearchMsg subMsg, MetaSearch subModel ) ->
--            toPage MetaSearch MetaSearchMsg MetaSearch.update subMsg subModel
--
--        ( PubchaseLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Pubchase subModel) } => Cmd.none
--
--        ( PubchaseLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( PubchaseMsg subMsg, Pubchase subModel ) ->
--            toPage Pubchase PubchaseMsg Pubchase.update subMsg subModel
--
--        ( PublicationLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Publication id subModel) } => Cmd.none
--
--        ( PublicationLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( PublicationsLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Publications subModel) } => Cmd.none
--
--        ( PublicationsLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( PublicationsMsg subMsg, Publications subModel ) ->
--            toPage Publications PublicationsMsg Publications.update subMsg subModel
--
--        ( ProfileLoaded token (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Profile token subModel) } => Cmd.none
--
--        ( ProfileLoaded token (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( ProjectsLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Projects subModel) } => Cmd.none
--
--        ( ProjectsLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( ProjectsMsg subMsg, Projects subModel ) ->
--            toPage Projects ProjectsMsg Projects.update subMsg subModel
--
--        ( ProjectLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Project id subModel) } => Cmd.none
--
--        ( ProjectLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( ProjectGroupLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (ProjectGroup id subModel) } => Cmd.none
--
--        ( ProjectGroupLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( ProjectGroupsLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (ProjectGroups subModel) } => Cmd.none
--
--        ( ProjectGroupsLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( ProjectGroupsMsg subMsg, ProjectGroups subModel ) ->
--            toPage ProjectGroups ProjectGroupsMsg ProjectGroups.update subMsg subModel
--
--        ( RemoveFromCart id, _ ) ->
--            let
--                newCart =
--                    Set.remove id model.session.cart
--            in
--            ( { model | session = { cart = newCart } }, Cmd.none )
--
--        ( SampleLoaded id (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Sample id subModel) } => Cmd.none
--
--        ( SampleLoaded id (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( SamplesLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Samples subModel) } => Cmd.none
--
--        ( SamplesLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( SamplesMsg subMsg, Samples subModel ) ->
--            toPage Samples SamplesMsg Samples.update subMsg subModel
--
--        ( SearchLoaded (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Search subModel) } => Cmd.none
--
--        ( SearchLoaded (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( SearchMsg subMsg, Search subModel ) ->
--            toPage Search SearchMsg Search.update subMsg subModel
--
--        {--
--        ( HomeMsg subMsg, Home subModel ) ->
--            toPage Home HomeMsg Home.update subMsg subModel
--            --}
--        ( HomeMsg subMsg, Home subModel ) ->
--            let
--                ( ( pageModel, cmd ), msgFromPage ) =
--                    Home.update subMsg subModel
--
--                newModel =
--                    case msgFromPage of
--                        Home.NoOp ->
--                            model
--
--                        Home.AddToCart id ->
--                            let
--                                newCart =
--                                    Set.insert id model.session.cart
--                            in
--                            { model | session = { cart = newCart } }
--
--                        Home.EmptyCart ->
--                            { model | session = { cart = Set.empty } }
--
--                        Home.RemoveFromCart id ->
--                            let
--                                newCart =
--                                    Set.remove id model.session.cart
--                            in
--                            { model | session = { cart = newCart } }
--            in
--            { newModel | pageState = Loaded (Home pageModel) }
--                => Cmd.map HomeMsg cmd
--
--        ( AboutMsg subMsg, About subModel ) ->
--            toPage About AboutMsg About.update subMsg subModel
--
--        ( MapLoaded lat lng (Ok subModel), _ ) ->
--            { model | pageState = Loaded (Map lat lng subModel) } => Cmd.none
--
--        ( MapLoaded lat lng (Err error), _ ) ->
--            { model | pageState = Loaded (Error error) } => Cmd.none
--
--        ( MapMsg subMsg, Map lat lng subModel ) ->
--            toPage (Map lat lng) MapMsg Map.update subMsg subModel
--
--        ( _, NotFound ) ->
--            -- Disregard incoming messages when we're on the
--            -- NotFound page.
--            model => Cmd.none
--
--        ( _, _ ) ->
--            -- Disregard incoming messages that arrived for the wrong page
--            model => Cmd.none
updatePage : Page -> Msg -> Model -> ( Model, Cmd Msg )
updatePage page msg model =
    let
        session =
            model.session

        toPage toModel toMsg subUpdate subMsg subModel =
            let
                ( newModel, newCmd ) =
                    subUpdate subMsg subModel
            in
            ( { model | pageState = Loaded (toModel newModel) }, Cmd.map toMsg newCmd )

        error =
            pageError model
    in
    case msg of
        SetRoute route ->
            setRoute route model

        AddToCart id ->
            let
                newCart =
                    Set.insert id model.session.cart
            in
            ( { model | session = { cart = newCart } }, Cmd.none )

        Authorize (Ok subModel) ->
            model
                ! [ OAuth.Implicit.authorize
                        { clientId = model.oauth.clientId
                        , redirectUri = model.oauth.redirectUri
                        , responseType = OAuth.Token
                        , scope = [ "PRODUCTION" ]
                        , state = Just "000"
                        , url = model.oauth.authEndpoint
                        }
                  ]

        AboutLoaded (Ok subModel) ->
            { model | pageState = Loaded (About subModel) } => Cmd.none

        AboutLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AppLoaded id (Ok subModel) ->
            { model | pageState = Loaded (App id subModel) } => Cmd.none

        AppLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AppsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Apps subModel) } => Cmd.none

        AppsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AssemblyLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Assembly id subModel) } => Cmd.none

        AssemblyLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AssembliesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Assemblies subModel) } => Cmd.none

        AssembliesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AssembliesMsg subMsg ->
            case page of
                Assemblies subModel ->
                    toPage Assemblies AssembliesMsg Assemblies.update subMsg subModel

                _ ->
                    model => Cmd.none

        CombinedAssemblyLoaded id (Ok subModel) ->
            { model | pageState = Loaded (CombinedAssembly id subModel) } => Cmd.none

        CombinedAssemblyLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        CombinedAssembliesLoaded (Ok subModel) ->
            { model | pageState = Loaded (CombinedAssemblies subModel) } => Cmd.none

        CombinedAssembliesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        CombinedAssembliesMsg subMsg ->
            case page of
                CombinedAssemblies subModel ->
                    toPage CombinedAssemblies CombinedAssembliesMsg CombinedAssemblies.update subMsg subModel

                _ ->
                    model => Cmd.none

        DomainsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Domains subModel) } => Cmd.none

        DomainsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        DomainsMsg subMsg ->
            case page of
                Domains subModel ->
                    toPage Domains DomainsMsg Domains.update subMsg subModel

                _ ->
                    model => Cmd.none

        DomainLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Domain id subModel) } => Cmd.none

        DomainLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        EmptyCart ->
            ( { model | session = { cart = Set.empty } }, Cmd.none )

        HomeLoaded (Ok subModel) ->
            { model | pageState = Loaded (Home subModel) } => Cmd.none

        HomeLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        InvestigatorLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Investigator id subModel) } => Cmd.none

        InvestigatorLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        InvestigatorsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Investigators subModel) } => Cmd.none

        InvestigatorsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        InvestigatorsMsg subMsg ->
            case page of
                Investigators subModel ->
                    ( toPage Investigators InvestigatorsMsg Investigators.update subMsg subModel )

                _ ->
                    model => Cmd.none

        MetaSearchLoaded (Ok subModel) ->
            { model | pageState = Loaded (MetaSearch subModel) } => Cmd.none

        MetaSearchLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        MetaSearchMsg subMsg ->
            case page of
                MetaSearch subModel ->
                    toPage MetaSearch MetaSearchMsg MetaSearch.update subMsg subModel

                _ ->
                    model => Cmd.none

        PubchaseLoaded (Ok subModel) ->
            { model | pageState = Loaded (Pubchase subModel) } => Cmd.none

        PubchaseLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        PubchaseMsg subMsg ->
            case page of
                Pubchase subModel ->
                    toPage Pubchase PubchaseMsg Pubchase.update subMsg subModel

                _ ->
                    model => Cmd.none

        PublicationLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Publication id subModel) } => Cmd.none

        PublicationLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        PublicationsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Publications subModel) } => Cmd.none

        PublicationsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        PublicationsMsg subMsg ->
            case page of
                Publications subModel ->
                    toPage Publications PublicationsMsg Publications.update subMsg subModel

                _ ->
                    model => Cmd.none

        ProfileLoaded token (Ok subModel) ->
            { model | pageState = Loaded (Profile token subModel) } => Cmd.none

        ProfileLoaded token (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Projects subModel) } => Cmd.none

        ProjectsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectsMsg subMsg ->
            case page of
                Projects subModel ->
                    toPage Projects ProjectsMsg Projects.update subMsg subModel

                _ ->
                    model => Cmd.none

        ProjectLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Project id subModel) } => Cmd.none

        ProjectLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectGroupLoaded id (Ok subModel) ->
            { model | pageState = Loaded (ProjectGroup id subModel) } => Cmd.none

        ProjectGroupLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectGroupsLoaded (Ok subModel) ->
            { model | pageState = Loaded (ProjectGroups subModel) } => Cmd.none

        ProjectGroupsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectGroupsMsg subMsg ->
            case page of
                ProjectGroups subModel ->
                    toPage ProjectGroups ProjectGroupsMsg ProjectGroups.update subMsg subModel

                _ ->
                    model => Cmd.none

        RemoveFromCart id ->
            let
                newCart =
                    Set.remove id model.session.cart
            in
            ( { model | session = { cart = newCart } }, Cmd.none )

        SampleLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Sample id subModel) } => Cmd.none

        SampleLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SamplesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Samples subModel) } => Cmd.none

        SamplesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SamplesMsg subMsg ->
            case page of
                Samples subModel ->
                    toPage Samples SamplesMsg Samples.update subMsg subModel

                _ ->
                    model => Cmd.none

        SearchLoaded (Ok subModel) ->
            { model | pageState = Loaded (Search subModel) } => Cmd.none

        SearchLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SearchMsg subMsg ->
            case page of
                Search subModel ->
                    toPage Search SearchMsg Search.update subMsg subModel

                _ ->
                    model => Cmd.none

        {--
        ( HomeMsg subMsg, Home subModel ) ->
            toPage Home HomeMsg Home.update subMsg subModel
            --}
        HomeMsg subMsg ->
            case page of
                Home subModel ->
                let
                    ( ( pageModel, cmd ), msgFromPage ) =
                        Home.update subMsg subModel

                    newModel =
                        case msgFromPage of
                            Home.NoOp ->
                                model

                            Home.AddToCart id ->
                                let
                                    newCart =
                                        Set.insert id model.session.cart
                                in
                                { model | session = { cart = newCart } }

                            Home.EmptyCart ->
                                { model | session = { cart = Set.empty } }

                            Home.RemoveFromCart id ->
                                let
                                    newCart =
                                        Set.remove id model.session.cart
                                in
                                { model | session = { cart = newCart } }
                in
                { newModel | pageState = Loaded (Home pageModel) }
                    => Cmd.map HomeMsg cmd

                _ ->
                    model => Cmd.none

        AboutMsg subMsg ->
            case page of
                About subModel ->
                    toPage About AboutMsg About.update subMsg subModel

                _ ->
                    model => Cmd.none

        MapLoaded lat lng (Ok subModel) ->
            { model | pageState = Loaded (Map lat lng subModel) } => Cmd.none

        MapLoaded lat lng (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        MapMsg subMsg ->
            case page of
                Map lat lng subModel ->
                    toPage (Map lat lng) MapMsg Map.update subMsg subModel

                _ ->
                    model => Cmd.none

        _ ->
            case page of
                NotFound ->
                    -- Disregard incoming messages when we're on the
                    -- NotFound page.
                    model => Cmd.none

                _ ->
                    -- Disregard incoming messages that arrived for the wrong page
                    model => Cmd.none



---- VIEW ----


view : Model -> Html Msg
view model =
    case model.pageState of
        Loaded page ->
            viewPage model.session False page

        TransitioningFrom page ->
            viewPage model.session True page


viewPage : Session -> Bool -> Page -> Html Msg
viewPage session isLoading page =
    let
        layout =
            Page.layout isLoading
    in
    case page of
        NotFound ->
            layout Page.Other NotFound.view

        Blank ->
            -- This is for the very intial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text ""
                |> layout Page.Other

        About subModel ->
            About.view subModel
                |> layout Page.About
                |> Html.map AboutMsg

        App id subModel ->
            App.view subModel
                |> layout Page.App
                |> Html.map AppMsg

        Apps subModel ->
            Apps.view subModel
                |> layout Page.Apps
                |> Html.map AppsMsg

        Assembly id subModel ->
            Assembly.view subModel
                |> layout Page.Assembly
                |> Html.map AssemblyMsg

        Assemblies subModel ->
            Assemblies.view subModel
                |> layout Page.Assemblies
                |> Html.map AssembliesMsg

        CombinedAssembly id subModel ->
            CombinedAssembly.view subModel
                |> layout Page.CombinedAssembly
                |> Html.map CombinedAssemblyMsg

        CombinedAssemblies subModel ->
            CombinedAssemblies.view subModel
                |> layout Page.CombinedAssemblies
                |> Html.map CombinedAssembliesMsg

        Domains subModel ->
            Domains.view subModel
                |> layout Page.Domains
                |> Html.map DomainsMsg

        Domain id subModel ->
            Domain.view subModel
                |> layout Page.Domain
                |> Html.map DomainMsg

        Error subModel ->
            Error.view subModel
                |> layout Page.Other

        Home subModel ->
            Home.view session subModel
                |> layout Page.Home
                |> Html.map HomeMsg

        Investigator id subModel ->
            Investigator.view subModel
                |> layout Page.Investigator
                |> Html.map InvestigatorMsg

        Investigators subModel ->
            Investigators.view subModel
                |> layout Page.Investigators
                |> Html.map InvestigatorsMsg

        Map lat lng subModel ->
            Map.view subModel
                |> layout Page.Map
                |> Html.map MapMsg

        MetaSearch subModel ->
            MetaSearch.view subModel
                |> layout Page.MetaSearch
                |> Html.map MetaSearchMsg

        Publication id subModel ->
            Publication.view subModel
                |> layout Page.Publication
                |> Html.map PublicationMsg

        Publications subModel ->
            Publications.view subModel
                |> layout Page.Publications
                |> Html.map PublicationsMsg

        Profile token subModel ->
            Profile.view subModel
                |> layout Page.Profile
                |> Html.map ProfileMsg

        Project id subModel ->
            Project.view subModel
                |> layout Page.Project
                |> Html.map ProjectMsg

        Projects subModel ->
            Projects.view subModel
                |> layout Page.Projects
                |> Html.map ProjectsMsg

        ProjectGroup id subModel ->
            ProjectGroup.view subModel
                |> layout Page.ProjectGroup
                |> Html.map ProjectGroupMsg

        ProjectGroups subModel ->
            ProjectGroups.view subModel
                |> layout Page.ProjectGroups
                |> Html.map ProjectGroupsMsg

        Pubchase subModel ->
            Pubchase.view subModel
                |> layout Page.Pubchase
                |> Html.map PubchaseMsg

        Sample id subModel ->
            Sample.view subModel
                |> layout Page.Sample
                |> Html.map SampleMsg

        Samples subModel ->
            Samples.view subModel
                |> layout Page.Samples
                |> Html.map SamplesMsg

        Search subModel ->
            Search.view subModel
                |> layout Page.Search
                |> Html.map SearchMsg



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- PORTS ----


port saveAuthToken : String -> Cmd msg


port saveCart : String -> Cmd msg



--port getAuthToken : (String -> msg) -> Sub msg
---- PROGRAM ----


initialPage : Page
initialPage =
    Blank


type alias Flags =
    { oauthClientId : String
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        model =
            { oauth =
                { authEndpoint = "https://agave.iplantc.org/authorize"
                , clientId = flags.oauthClientId
                , redirectUri = "http://localhost:8080/" --location.origin ++ location.pathname
                }
            , session = { cart = Set.empty }
            , error = Nothing
            , token = Nothing
            , pageState = Loaded initialPage
            }

        _ =
            Debug.log "flags " flags

        -- Kludge for Agave not returning required "token_type=bearer" in redirect
        location2 =
            { location | hash = location.hash ++ "&token_type=bearer" }
    in
    case OAuth.Implicit.parse location2 of
        Ok { token } ->
            let
                saveToken =
                    saveAuthToken (toString token)
            in
            Tuple.mapSecond (\c -> Cmd.batch [ c, saveToken ])
                (setRoute (Just (Route.Profile (toString token))) { model | token = Just token })

        Err OAuth.Empty ->
            let
                _ =
                    Debug.log "OAuth.Empty" ""
            in
            setRoute (Route.fromLocation location) model

        Err (OAuth.OAuthErr err) ->
            let
                _ =
                    Debug.log "OAuth.OAuthErr" err
            in
            { model | error = Just <| OAuth.showErrCode err.error }
                ! [ Navigation.modifyUrl model.oauth.redirectUri ]

        Err a ->
            let
                _ =
                    Debug.log "Error" (toString a ++ toString location2)
            in
            { model | error = Just "parsing error" } ! []


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
