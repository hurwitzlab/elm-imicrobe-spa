module Main exposing (..)

import Config as Config
import Data.Session as Session exposing (Session)
import Data.User as User
import Data.Agave as Agave
import Data.App
import Data.Cart
import Data.ORCID as ORCID
import Debug exposing (log)
import Json.Decode as Decode exposing (Value)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http
import Navigation exposing (Location)
import OAuth
import OAuth.Implicit
import OAuth.AuthorizationCode
import Page.App as App
import Page.Apps as Apps
import Page.Assembly as Assembly
import Page.Assemblies as Assemblies
import Page.Cart as Cart
import Page.CombinedAssembly as CombinedAssembly
import Page.CombinedAssemblies as CombinedAssemblies
import Page.Contact as Contact
import Page.Dashboard as Dashboard
import Page.Domain as Domain
import Page.Domains as Domains
import Page.Error as Error exposing (PageLoadError, redirectLoadError)
import Page.Files as Files
import Page.Home as Home
import Page.Investigator as Investigator
import Page.Investigators as Investigators
import Page.Job as Job
import Page.Jobs as Jobs
import Page.Map as Map
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
import Page.TaxonomySearch as TaxonomySearch
import Page.ProteinSearch as ProteinSearch
import Route exposing (..)
import Request.Agave
import Request.ORCID
import Request.User
import Ports
import Task exposing (Task)
import Time exposing (Time)
import Util exposing ((=>))
import View.Page as Page exposing (ActivePage)
import Events exposing (onKeyDown)



---- MODEL ----


type alias Model =
    { pageState : PageState
    , currentRoute : Maybe Route -- mdb added 11/15/17 for oauth redirect back to original page
    , session : Session
    , oauth :
        { authEndpoint : String
        , clientId : String
        , redirectUri : String
        }
    , query : String
    , error : Maybe String
    }


type Page
    = Blank
    | Apps Apps.Model
    | App Int App.Model
    | Assemblies Assemblies.Model
    | Assembly Int Assembly.Model
    | Cart Cart.Model
    | CombinedAssemblies CombinedAssemblies.Model
    | CombinedAssembly Int CombinedAssembly.Model
    | Contact Contact.Model
    | Dashboard Dashboard.Model
    | Domain Int Domain.Model
    | Domains Domains.Model
    | Error PageLoadError
    | Files Files.Model
    | Home Home.Model
    | Investigator Int Investigator.Model
    | Investigators Investigators.Model
    | Job String Job.Model
    | Jobs Jobs.Model
    | Map String String Map.Model
    | NotFound
    | Profile Profile.Model
    | Project Int Project.Model
    | ProjectGroup Int ProjectGroup.Model
    | ProjectGroups ProjectGroups.Model
    | Projects Projects.Model
    | Pubchase Pubchase.Model
    | Publication Int Publication.Model
    | Publications Publications.Model
    | Sample Int Sample.Model
    | Samples Samples.Model
    | Search String Search.Model
    | TaxonomySearch String TaxonomySearch.Model
    | ProteinSearch String ProteinSearch.Model


type PageState
    = Loaded Page
    | TransitioningFrom Page



---- UPDATE ----


type Msg
    = AppLoaded Int (Result PageLoadError App.Model)
    | AppMsg App.Msg
    | AppsLoaded (Result PageLoadError Apps.Model)
    | AppsMsg Apps.Msg
    | AssemblyLoaded Int (Result PageLoadError Assembly.Model)
    | AssemblyMsg Assembly.Msg
    | AssembliesLoaded (Result PageLoadError Assemblies.Model)
    | AssembliesMsg Assemblies.Msg
    | Authorize
    | Deauthorize
    | AuthenticateORCID (Result Http.Error ORCID.TokenResponse)
    | CartLoaded (Result PageLoadError Cart.Model)
    | CartMsg Cart.Msg
    | CombinedAssemblyLoaded Int (Result PageLoadError CombinedAssembly.Model)
    | CombinedAssemblyMsg CombinedAssembly.Msg
    | CombinedAssembliesLoaded (Result PageLoadError CombinedAssemblies.Model)
    | CombinedAssembliesMsg CombinedAssemblies.Msg
    | ContactLoaded (Result PageLoadError Contact.Model)
    | ContactMsg Contact.Msg
    | DashboardLoaded (Result PageLoadError Dashboard.Model)
    | DashboardMsg Dashboard.Msg
    | DomainLoaded Int (Result PageLoadError Domain.Model)
    | DomainMsg Domain.Msg
    | DomainsLoaded (Result PageLoadError Domains.Model)
    | DomainsMsg Domains.Msg
    | FilesLoaded (Result PageLoadError Files.Model)
    | FilesMsg Files.Msg
    | HomeLoaded (Result PageLoadError Home.Model)
    | HomeMsg Home.Msg
    | InvestigatorLoaded Int (Result PageLoadError Investigator.Model)
    | InvestigatorMsg Investigator.Msg
    | InvestigatorsLoaded (Result PageLoadError Investigators.Model)
    | InvestigatorsMsg Investigators.Msg
    | JobLoaded String (Result PageLoadError Job.Model)
    | JobMsg Job.Msg
    | JobsLoaded (Result PageLoadError Jobs.Model)
    | JobsMsg Jobs.Msg
    | MapLoaded String String (Result PageLoadError Map.Model)
    | MapMsg Map.Msg
    | LoadProfile Agave.Profile
    | LoginRecorded User.Login
    | ProfileLoaded (Result PageLoadError Profile.Model)
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
    | SampleLoaded Int (Result PageLoadError Sample.Model)
    | SampleMsg Sample.Msg
    | SamplesLoaded (Result PageLoadError Samples.Model)
    | SamplesMsg Samples.Msg
    | SearchLoaded String (Result PageLoadError Search.Model)
    | SearchMsg Search.Msg
    | TaxonomySearchLoaded String (Result PageLoadError TaxonomySearch.Model)
    | TaxonomySearchMsg TaxonomySearch.Msg
    | ProteinSearchLoaded String (Result PageLoadError ProteinSearch.Model)
    | ProteinSearchMsg ProteinSearch.Msg
    | SetRoute (Maybe Route)
    | SetSession (Maybe Session)
    | SelectFile Data.App.FileBrowser
    | FileUploadFileSelected (Maybe Ports.FileToUpload)
    | FileUploadDone (Maybe (Request.Agave.Response Agave.UploadResult))
    | PollTimerTick Time
    | InputTimerTick Time
    | SearchBarInput String
    | SearchBarKeyDown Int
    | SearchBarQuery


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        routeString =
            case maybeRoute of
                Nothing -> "Nothing"

                Just route -> Route.routeToString route

        transition toMsg task =
--            let
--                scrollToTop : Task x ()
--                scrollToTop =
--                    Dom.Scroll.toTop "page-body"
--                        -- It's not worth showing the user anything special if scrolling fails.
--                        -- If anything, we'd log this to an error recording service.
--                        |> Task.onError (\_ -> Task.succeed ())
--            in
            { model | pageState = TransitioningFrom (getPage model.pageState), currentRoute = maybeRoute }
                => Cmd.batch
                    [ Ports.updateAnalytics routeString
                    , Task.attempt toMsg task

                    -- Can't get scrollToTop working per Richard Feldman's demo:  https://github.com/rtfeldman/elm-spa-example/blob/74cb1e5da06492050ed4cf3002c3eef2d2ca184d/src/Views/Article/Feed.elm#L267
                    -- As a workaround added Ports.scrollToTop which is triggered in updatePage below for each relevant page
--                    , Task.perform (\_ -> NoOp) scrollToTop -- doesn't work
--                    , Ports.scrollToTop "page-body" -- scrolls before page is loaded
                    ]

--        error =
--            pageError model
    in
    case maybeRoute of
        Nothing ->
            { model | pageState = Loaded NotFound } => Cmd.none

        Just (Route.App id) ->
            transition (AppLoaded id) (App.init model.session id)

        Just Route.Apps ->
            transition AppsLoaded Apps.init

        Just (Route.Assembly id) ->
            transition (AssemblyLoaded id) (Assembly.init id)

        Just Route.Assemblies ->
            transition AssembliesLoaded Assemblies.init

        Just Route.Cart ->
            transition CartLoaded (Cart.init model.session)

        Just (Route.CombinedAssembly id) ->
            transition (CombinedAssemblyLoaded id) (CombinedAssembly.init id)

        Just Route.CombinedAssemblies ->
            transition CombinedAssembliesLoaded CombinedAssemblies.init

        Just Route.Contact ->
            transition ContactLoaded (Contact.init model.session)

        Just Route.Dashboard ->
            transition DashboardLoaded (Dashboard.init model.session)

        Just Route.Domains ->
            transition DomainsLoaded Domains.init

        Just (Route.Domain id) ->
            transition (DomainLoaded id) (Domain.init id)

        Just Route.Files ->
            transition FilesLoaded (Files.init model.session)

        Just Route.Home ->
            transition HomeLoaded (Home.init model.session)

        Just (Route.Investigator id) ->
            transition (InvestigatorLoaded id) (Investigator.init id)

        Just Route.Investigators ->
            transition InvestigatorsLoaded Investigators.init

        Just Route.Jobs ->
            transition JobsLoaded (Jobs.init model.session)

        Just (Route.Job id) ->
            transition (JobLoaded id) (Job.init model.session id)

        Just Route.Login ->
            --transition Authorize (Home.init model.session)
            update Authorize model

        Just Route.Logout ->
            --transition Deauthorize (Home.init model.session)
            update Deauthorize model

        Just (Route.Map lat lng) ->
            transition (MapLoaded lat lng) (Map.init lat lng)

        Just Route.Pubchase ->
            transition PubchaseLoaded Pubchase.init

        Just Route.Publications ->
            transition PublicationsLoaded Publications.init

        Just (Route.Publication id) ->
            transition (PublicationLoaded id) (Publication.init id)

        Just Route.Profile ->
            transition ProfileLoaded (Profile.init model.session)

        Just (Route.Project id) ->
            transition (ProjectLoaded id) (Project.init model.session id)

        Just Route.Projects ->
            transition ProjectsLoaded (Projects.init model.session)

        Just (Route.ProjectGroup id) ->
            transition (ProjectGroupLoaded id) (ProjectGroup.init model.session id)

        Just Route.ProjectGroups ->
            transition ProjectGroupsLoaded (ProjectGroups.init model.session)

        Just (Route.Sample id) ->
            transition (SampleLoaded id) (Sample.init model.session id)

        Just Route.Samples ->
            transition SamplesLoaded (Samples.init model.session)

        Just (Route.Search query) ->
            transition (SearchLoaded query) (Search.init query)

        Just (Route.TaxonomySearch query) ->
            transition (TaxonomySearchLoaded query) (TaxonomySearch.init model.session query)

        Just (Route.ProteinSearch query) ->
            transition (ProteinSearchLoaded query) (ProteinSearch.init model.session query)


getPage : PageState -> Page
getPage pageState =
    case pageState of
        Loaded page ->
            page

        TransitioningFrom page ->
            page


--pageError : Model -> ActivePage -> String -> ( Model, Cmd msg )
--pageError model activePage errorMessage =
--    let
--        error =
--            Error.pageLoadError activePage errorMessage
--    in
--    { model | pageState = Loaded (Error error) } => Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updatePage (getPage model.pageState) msg model


-- FIXME updatePage rewritten without tuple-matching due to performance issues in Elm v0.18 which should be
-- fixed in v0.19.
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

--        error =
--            pageError model

        scrollToTop =
            Ports.scrollToTop "page-body"
    in
    case msg of
        SetRoute route ->
            setRoute route model

        Authorize ->
            let
                routeStr =
                    case model.currentRoute of
                        Nothing -> ""

                        Just route -> routeToString route
            in
            model
                ! [ OAuth.Implicit.authorize
                        { clientId = model.oauth.clientId
                        , redirectUri = model.oauth.redirectUri
                        , responseType = OAuth.Token
                        , scope = [ "PRODUCTION" ]
                        , state = Just routeStr --Just "000"
                        , url = model.oauth.authEndpoint
                        }
                  ]

        Deauthorize ->
            let
                newSession =
                    { session | token = "", user = Nothing, profile = Nothing }
            in
            { model | session = newSession } => Cmd.batch [ Session.store newSession, Route.modifyUrl Route.Home ]

        AuthenticateORCID res ->
            case res of
                Err err ->
                    { model | error = Just "unable to authenticate ORCID" } ! []

                Ok { access_token, orcid } ->
                    let
                        _ = Debug.log "orcid " (toString orcid)
                    in
                    model => Route.modifyUrl Route.Profile

        AppLoaded id (Ok subModel) ->
            { model | pageState = Loaded (App id subModel) } => scrollToTop

        AppLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        AppMsg subMsg ->
            case page of
                App id subModel ->
                    toPage (App id) AppMsg (App.update session) subMsg subModel

                _ ->
                    model => Cmd.none

        AppsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Apps subModel) } => scrollToTop

        AppsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AppsMsg subMsg ->
            case page of
                Apps subModel ->
                    toPage Apps AppsMsg Apps.update subMsg subModel

                _ ->
                    model => Cmd.none

        AssemblyLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Assembly id subModel) } => scrollToTop

        AssemblyLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AssembliesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Assemblies subModel) } => scrollToTop

        AssembliesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AssembliesMsg subMsg ->
            case page of
                Assemblies subModel ->
                    toPage Assemblies AssembliesMsg Assemblies.update subMsg subModel

                _ ->
                    model => Cmd.none

        CartLoaded (Ok subModel) ->
            { model | pageState = Loaded (Cart subModel) } => scrollToTop

        CartLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        CartMsg subMsg ->
            case page of
                Cart subModel ->
                    let
--                        _ = Debug.log "Main.CartMsg" (toString subMsg)

                        ( ( pageModel, cmd ), msgFromPage ) =
                            Cart.update model.session subMsg subModel

                        newModel =
                            case msgFromPage of
                                Cart.NoOp ->
                                    model

                                Cart.SetCart newCart ->
                                    let
                                        newSession =
                                            { session | cart = newCart }
                                    in
                                    { model | session = newSession }

                    in
                    { newModel | pageState = Loaded (Cart pageModel) }
                        => Cmd.map CartMsg cmd

                _ ->
                    model => Cmd.none

        CombinedAssemblyLoaded id (Ok subModel) ->
            { model | pageState = Loaded (CombinedAssembly id subModel) } => scrollToTop

        CombinedAssemblyLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        CombinedAssembliesLoaded (Ok subModel) ->
            { model | pageState = Loaded (CombinedAssemblies subModel) } => scrollToTop

        CombinedAssembliesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        CombinedAssembliesMsg subMsg ->
            case page of
                CombinedAssemblies subModel ->
                    toPage CombinedAssemblies CombinedAssembliesMsg CombinedAssemblies.update subMsg subModel

                _ ->
                    model => Cmd.none

        ContactLoaded (Ok subModel) ->
            { model | pageState = Loaded (Contact subModel) } => scrollToTop

        ContactLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ContactMsg subMsg ->
            case page of
                Contact subModel ->
                    toPage Contact ContactMsg Contact.update subMsg subModel

                _ ->
                    model => Cmd.none

        DashboardLoaded (Ok subModel) ->
            { model | pageState = Loaded (Dashboard subModel) } => scrollToTop

        DashboardLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        DashboardMsg subMsg ->
            case page of
                Dashboard subModel ->
                    toPage Dashboard DashboardMsg (Dashboard.update session) subMsg subModel

                _ ->
                    model => Cmd.none

        DomainsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Domains subModel) } => scrollToTop

        DomainsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        DomainsMsg subMsg ->
            case page of
                Domains subModel ->
                    toPage Domains DomainsMsg Domains.update subMsg subModel

                _ ->
                    model => Cmd.none

        DomainLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Domain id subModel) } => scrollToTop

        DomainLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        FilesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Files subModel) } => scrollToTop

        FilesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        FilesMsg subMsg ->
            case page of
                Files subModel ->
                    toPage Files FilesMsg Files.update subMsg subModel

                _ ->
                    model => Cmd.none

        HomeLoaded (Ok subModel) ->
            { model | pageState = Loaded (Home subModel) } => scrollToTop

        HomeLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        HomeMsg subMsg ->
            case page of
                Home subModel ->
                    toPage Home HomeMsg Home.update subMsg subModel

                _ ->
                    model => Cmd.none

        InvestigatorLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Investigator id subModel) } => scrollToTop

        InvestigatorLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        InvestigatorsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Investigators subModel) } => scrollToTop

        InvestigatorsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        InvestigatorsMsg subMsg ->
            case page of
                Investigators subModel ->
                    toPage Investigators InvestigatorsMsg Investigators.update subMsg subModel

                _ ->
                    model => Cmd.none

        JobsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Jobs subModel) } => scrollToTop

        JobsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        JobsMsg subMsg ->
            case page of
                Jobs subModel ->
                    toPage Jobs JobsMsg Jobs.update subMsg subModel

                _ ->
                    model => Cmd.none

        JobLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Job id subModel) } => scrollToTop

        JobLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        JobMsg subMsg ->
            case page of
                Job id subModel ->
                    toPage (Job id) JobMsg (Job.update session) subMsg subModel

                _ ->
                    model => Cmd.none

        PubchaseLoaded (Ok subModel) ->
            { model | pageState = Loaded (Pubchase subModel) } => scrollToTop

        PubchaseLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        PubchaseMsg subMsg ->
            case page of
                Pubchase subModel ->
                    toPage Pubchase PubchaseMsg Pubchase.update subMsg subModel

                _ ->
                    model => Cmd.none

        PublicationLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Publication id subModel) } => scrollToTop

        PublicationLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        PublicationMsg subMsg ->
            case page of
                Publication id subModel ->
                    toPage (Publication id) PublicationMsg Publication.update subMsg subModel

                _ ->
                    model => Cmd.none

        PublicationsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Publications subModel) } => scrollToTop

        PublicationsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        PublicationsMsg subMsg ->
            case page of
                Publications subModel ->
                    toPage Publications PublicationsMsg Publications.update subMsg subModel

                _ ->
                    model => Cmd.none

        LoadProfile profile ->
            let
                session = model.session

                newSession =
                    { session | profile = Just profile }

                recordLogin username =
                    Request.User.recordLogin session.token username |> Http.toTask |> Task.attempt handleRecordLogin

                handleRecordLogin login =
                    case login of
                        Ok login ->
                            LoginRecorded login

                        Err error ->
                            let
                                _ = Debug.log "Error" "could not record login: " ++ (toString error)
                            in
                            SetRoute (Just Route.Home)
            in
            { model | session = newSession } => Cmd.batch [ Session.store newSession, (recordLogin profile.username) ]

        LoginRecorded login ->
            let
                session = model.session

                newSession =
                    { session | user = Just login.user }
            in
            { model | session = newSession } => Cmd.batch [ Session.store newSession ]

        MapLoaded lat lng (Ok subModel) ->
            { model | pageState = Loaded (Map lat lng subModel) } => scrollToTop

        MapLoaded lat lng (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        MapMsg subMsg ->
            case page of
                Map lat lng subModel ->
                    toPage (Map lat lng) MapMsg Map.update subMsg subModel

                _ ->
                    model => Cmd.none

        ProfileLoaded (Ok subModel) ->
            { model | pageState = Loaded (Profile subModel) } => scrollToTop

        ProfileLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        ProfileMsg subMsg ->
            case page of
                Profile subModel ->
                    toPage Profile ProfileMsg Profile.update subMsg subModel

                _ ->
                    model => Cmd.none

        ProjectsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Projects subModel) } => scrollToTop

        ProjectsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectsMsg subMsg ->
            case page of
                Projects subModel ->
                    toPage Projects ProjectsMsg (Projects.update session) subMsg subModel

--                Projects subModel ->
--                    let
--                        ( ( pageModel, cmd ), msgFromPage ) =
--                            Projects.update model.session subMsg subModel
--
--                        newModel =
--                            case msgFromPage of
--                                Projects.NoOp ->
--                                    model
--
--                                Projects.SetCart newCart ->
--                                    let
--                                        newSession =
--                                            { session | cart = newCart }
--                                    in
--                                    { model | session = newSession }
--
--                    in
--                    { newModel | pageState = Loaded (Projects pageModel) }
--                        => Cmd.map ProjectsMsg cmd

                _ ->
                    model => Cmd.none

        ProjectLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Project id subModel) } => scrollToTop

        ProjectLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        ProjectMsg subMsg ->
            case page of
                Project id subModel ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                            Project.update model.session subMsg subModel

                        newModel =
                            case msgFromPage of
                                Project.NoOp ->
                                    model

                                Project.SetCart newCart ->
                                    let
                                        newSession =
                                            { session | cart = newCart }
                                    in
                                    { model | session = newSession }

                    in
                    { newModel | pageState = Loaded (Project id pageModel) }
                        => Cmd.map ProjectMsg cmd

                _ ->
                    model => Cmd.none

        ProjectGroupLoaded id (Ok subModel) ->
            { model | pageState = Loaded (ProjectGroup id subModel) } => scrollToTop

        ProjectGroupLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectGroupMsg subMsg ->
            case page of
                ProjectGroup id subModel ->
                    toPage (ProjectGroup id) ProjectGroupMsg (ProjectGroup.update session) subMsg subModel

                _ ->
                    model => Cmd.none

        ProjectGroupsLoaded (Ok subModel) ->
            { model | pageState = Loaded (ProjectGroups subModel) } => scrollToTop

        ProjectGroupsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProjectGroupsMsg subMsg ->
            case page of
                ProjectGroups subModel ->
                    toPage ProjectGroups ProjectGroupsMsg ProjectGroups.update subMsg subModel

                _ ->
                    model => Cmd.none

        SampleLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Sample id subModel) } => scrollToTop

        SampleLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        SampleMsg subMsg ->
            case page of
                Sample id subModel ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                            Sample.update model.session subMsg subModel

                        newModel =
                            case msgFromPage of
                                Sample.NoOp ->
                                    model

                                Sample.SetCart newCart ->
                                    let
                                        newSession =
                                            { session | cart = newCart }
                                    in
                                    { model | session = newSession }

                    in
                    { newModel | pageState = Loaded (Sample id pageModel) }
                        => Cmd.map SampleMsg cmd

                _ ->
                    model => Cmd.none

        SamplesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Samples subModel) } => scrollToTop

        SamplesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SamplesMsg subMsg ->
            case page of
                Samples subModel ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                            Samples.update model.session subMsg subModel

                        newModel =
                            case msgFromPage of
                                Samples.NoOp ->
                                    model

                                Samples.SetCart newCart ->
                                    let
                                        newSession =
                                            { session | cart = newCart }
                                    in
                                    { model | session = newSession }

                    in
                    { newModel | pageState = Loaded (Samples pageModel) }
                        => Cmd.map SamplesMsg cmd

                _ ->
                    model => Cmd.none

        SearchLoaded query (Ok subModel) ->
            { model | pageState = Loaded (Search query subModel) } => scrollToTop

        SearchLoaded query (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SearchMsg subMsg ->
            case page of
                Search query subModel ->
                    toPage (Search query) SearchMsg Search.update subMsg subModel

                _ ->
                    model => Cmd.none

        TaxonomySearchLoaded query (Ok subModel) ->
            { model | pageState = Loaded (TaxonomySearch query subModel) } => scrollToTop

        TaxonomySearchLoaded query (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        TaxonomySearchMsg subMsg ->
            case page of
                TaxonomySearch query subModel ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                            TaxonomySearch.update model.session subMsg subModel

                        newModel =
                            case msgFromPage of
                                TaxonomySearch.NoOp ->
                                    model

                                TaxonomySearch.SetCart newCart ->
                                    let
                                        newSession =
                                            { session | cart = newCart }
                                    in
                                    { model | session = newSession }

                    in
                    { newModel | pageState = Loaded (TaxonomySearch query pageModel) }
                        => Cmd.map TaxonomySearchMsg cmd

                _ ->
                    model => Cmd.none

        ProteinSearchLoaded query (Ok subModel) ->
            { model | pageState = Loaded (ProteinSearch query subModel) } => scrollToTop

        ProteinSearchLoaded query (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ProteinSearchMsg subMsg ->
            case page of
                ProteinSearch query subModel ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                            ProteinSearch.update model.session subMsg subModel

                        newModel =
                            case msgFromPage of
                                ProteinSearch.NoOp ->
                                    model

                                ProteinSearch.SetCart newCart ->
                                    let
                                        newSession =
                                            { session | cart = newCart }
                                    in
                                    { model | session = newSession }

                    in
                    { newModel | pageState = Loaded (ProteinSearch query pageModel) }
                        => Cmd.map ProteinSearchMsg cmd

                _ ->
                    model => Cmd.none

        SetSession newSession ->
            let
                 _ = Debug.log "Main.SetSession" (toString newSession)
            in
            case newSession of
                Just newSession ->
                    case page of
                        Cart subModel ->
                            let
                                ( ( pageModel, cmd ), msgFromPage ) =
                                    Cart.update model.session (Cart.SetSession newSession) subModel
                            in
                            { model | pageState = Loaded (Cart pageModel) } => Cmd.map CartMsg cmd

                        Samples subModel ->
                            let
                                ( ( pageModel, cmd ), msgFromPage ) =
                                    Samples.update model.session (Samples.SetSession newSession) subModel
                            in
                            { model | pageState = Loaded (Samples pageModel) } => Cmd.map SamplesMsg cmd

                        _ ->
                            { model | session = newSession } => Cmd.none

                Nothing ->
                    model => Cmd.none

        SelectFile selection ->
            let
                _ = Debug.log "Main.SelectFile" (toString selection)
            in
            case page of
                App id subModel ->
                    let
                        (pageModel, cmd) =
                            App.update session (App.SetInput selection.source selection.id selection.path) subModel
                    in
                    { model | pageState = Loaded (App id pageModel) } => Cmd.map AppMsg cmd

                _ ->
                    model => Cmd.none

        FileUploadFileSelected file ->
            case page of
                Dashboard subModel ->
                    let
                        (pageModel, cmd) =
                            Dashboard.update session (Dashboard.UploadFileBegin file) subModel
                    in
                    { model | pageState = Loaded (Dashboard pageModel) } => Cmd.map DashboardMsg cmd

                _ ->
                    model => Cmd.none

        FileUploadDone result ->
            case page of
                Dashboard subModel ->
                    let
                        (pageModel, cmd) =
                            let
                                _ = Debug.log (toString result)
                            in
                            case result of
                                Nothing ->
                                    Dashboard.update session Dashboard.UploadFileError subModel

                                _ ->
                                    Dashboard.update session Dashboard.UploadFileEnd subModel
                    in
                    { model | pageState = Loaded (Dashboard pageModel) } => Cmd.map DashboardMsg cmd

                _ ->
                    model => Cmd.none

        PollTimerTick time ->
            case page of
                Job id subModel ->
                    let
                        (pageModel, cmd) =
                                Job.update session (Job.PollJob time) subModel
                    in
                    { model | pageState = Loaded (Job id pageModel) } => Cmd.map JobMsg cmd

                _ ->
                    model => Cmd.none

        InputTimerTick time ->
            case page of
                Samples subModel ->
                    let
                        ( ( pageModel, cmd ), msgFromPage ) =
                                Samples.update session (Samples.DelayedSearch time) subModel
                    in
                    { model | pageState = Loaded (Samples pageModel) } => Cmd.map SamplesMsg cmd

                _ ->
                    model => Cmd.none

        SearchBarInput query ->
            { model | query = query } => Cmd.none

        SearchBarKeyDown key ->
            if key == 13 then -- enter key
                update SearchBarQuery model
            else
                model => Cmd.none

        SearchBarQuery ->
            case model.query of
                "" ->
                    model => Cmd.none

                _ ->
                    model => Route.modifyUrl (Route.Search model.query)

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
            pageLayout isLoading session
    in
    case page of
        NotFound ->
            layout Page.Other NotFound.view

        Blank ->
            -- This is for the very initial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text ""
                |> layout Page.Other

        App id subModel ->
            App.view subModel
                |> Html.map AppMsg
                |> layout Page.App

        Apps subModel ->
            Apps.view subModel
                |> Html.map AppsMsg
                |> layout Page.Apps

        Assembly id subModel ->
            Assembly.view subModel
                |> Html.map AssemblyMsg
                |> layout Page.Assembly

        Assemblies subModel ->
            Assemblies.view subModel
                |> Html.map AssembliesMsg
                |> layout Page.Assemblies

        Cart subModel ->
            Cart.view subModel
                |> Html.map CartMsg
                |> layout Page.Cart

        CombinedAssembly id subModel ->
            CombinedAssembly.view subModel
                |> Html.map CombinedAssemblyMsg
                |> layout Page.CombinedAssembly

        CombinedAssemblies subModel ->
            CombinedAssemblies.view subModel
                |> Html.map CombinedAssembliesMsg
                |> layout Page.CombinedAssemblies

        Contact subModel ->
            Contact.view subModel
                |> Html.map ContactMsg
                |> layout Page.Contact

        Dashboard subModel ->
            Dashboard.view subModel
                |> Html.map DashboardMsg
                |> layout Page.Dashboard

        Domains subModel ->
            Domains.view subModel
                |> Html.map DomainsMsg
                |> layout Page.Domains

        Domain id subModel ->
            Domain.view subModel
                |> Html.map DomainMsg
                |> layout Page.Domain

        Error subModel ->
            Error.view subModel
                |> layout Page.Other

        Files subModel ->
            Files.view subModel
                |> Html.map FilesMsg
                |> layout Page.Files

        Home subModel ->
            Home.view subModel
                |> Html.map HomeMsg
                |> layout Page.Home

        Investigator id subModel ->
            Investigator.view subModel
                |> Html.map InvestigatorMsg
                |> layout Page.Investigator

        Investigators subModel ->
            Investigators.view subModel
                |> Html.map InvestigatorsMsg
                |> layout Page.Investigators

        Job id subModel ->
            Job.view subModel
                |> Html.map JobMsg
                |> layout Page.Job

        Jobs subModel ->
            Jobs.view subModel
                |> Html.map JobsMsg
                |> layout Page.Jobs

        Map lat lng subModel ->
            Map.view subModel
                |> Html.map MapMsg
                |> layout Page.Map

        Publication id subModel ->
            Publication.view subModel
                |> Html.map PublicationMsg
                |> layout Page.Publication

        Publications subModel ->
            Publications.view subModel
                |> Html.map PublicationsMsg
                |> layout Page.Publications

        Profile subModel ->
            Profile.view subModel
                |> Html.map ProfileMsg
                |> layout Page.Profile

        Project id subModel ->
            Project.view subModel
                |> Html.map ProjectMsg
                |> layout Page.Project

        Projects subModel ->
            Projects.view subModel
                |> Html.map ProjectsMsg
                |> layout Page.Projects

        ProjectGroup id subModel ->
            ProjectGroup.view subModel
                |> Html.map ProjectGroupMsg
                |> layout Page.ProjectGroup

        ProjectGroups subModel ->
            ProjectGroups.view subModel
                |> Html.map ProjectGroupsMsg
                |> layout Page.ProjectGroups

        Pubchase subModel ->
            Pubchase.view subModel
                |> Html.map PubchaseMsg
                |> layout Page.Pubchase

        Sample id subModel ->
            Sample.view subModel
                |> Html.map SampleMsg
                |> layout Page.Sample

        Samples subModel ->
            Samples.view subModel
                |> Html.map SamplesMsg
                |> layout Page.Samples

        Search query subModel ->
            Search.view subModel
                |> Html.map SearchMsg
                |> layout Page.Search

        TaxonomySearch query subModel ->
            TaxonomySearch.view subModel
                |> Html.map TaxonomySearchMsg
                |> layout Page.TaxonomySearch

        ProteinSearch query subModel ->
            ProteinSearch.view subModel
                |> Html.map ProteinSearchMsg
                |> layout Page.ProteinSearch


{-| Take a page's Html and layout it with a header and footer.
    isLoading can be used to show loading indicator during slow transitions
    MDB 10/30/17: moved from Page.elm in support of header search bar
-}
pageLayout : Bool -> Session -> ActivePage -> Html Msg -> Html Msg
pageLayout isLoading session page content =
    div []
        [ viewHeader page isLoading session
        , div [] [ content ]
--        , viewFooter
        ]


viewHeader : ActivePage -> Bool -> Session -> Html Msg
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
                            [ li [] [ a [ Route.href Route.Dashboard ] [ text "Dashboard" ] ]
                            , li [] [ a [ Route.href Route.Profile ] [ text "Profile" ] ]
                            , li [] [ a [ Route.href Route.Logout ] [ text "Sign out" ] ]
                            ]
                        ]

        numItemsInCart =
            Data.Cart.size session.cart

        cartButton =
            let
                label =
                    if numItemsInCart == 0 then
                        []
                    else
                        [ span [ class "gray absolute" ] [ text (toString numItemsInCart) ] ]
            in
            div [ class "pull-right", style [("padding-top", "21px"), ("margin-left", "2em")], title "Cart" ]
                [ a [ Route.href Route.Cart ]
                    (span [ class "icon-button glyphicon glyphicon-shopping-cart" ] [] :: label)
                ]

        cartLabel =
            if numItemsInCart == 0 then
                "(Empty)"
            else
                "(" ++ (toString numItemsInCart) ++ ")"

        dashboardButton =
            if profile == Nothing then
                text ""
            else
                div [ class "pull-right", style [("padding-top", "21px"), ("margin-left", "2em")], title "Dashboard" ]
                    [ a [ Route.href Route.Dashboard ]
                        [ span [ class "icon-button glyphicon glyphicon-dashboard" ] [] ]
                    ]

        helpButton =
            div [ class "pull-right", style [("padding-top", "21px"), ("margin-left", "2em")], title "Contact Us" ]
                [ a [ Route.href Route.Contact ]
                    [ span [ class "icon-button glyphicon glyphicon-question-sign" ] [] ]
                ]

        searchBar =
            div [ class "pull-right", style [("padding-top", "10px")] ]
                [ div [ class "navbar-form" ]
                    [ div [ class "input-group" ]
                        [ input [ class "form-control", placeholder "Search", onInput SearchBarInput, onKeyDown SearchBarKeyDown ] []
                        , div [ class "input-group-btn" ]
                            [ button [ class "btn btn-default", onClick SearchBarQuery ]
                                [ i [ class "glyphicon glyphicon-search" ] []
                                ]
                            ]
                        ]
                    ]
                ]
    in
    div []
        [ div [ class "hidden message-banner" ] [ text "" ]
        , nav [ class "navbar navbar-default navbar-static-top", style [("padding-top", "10px")] ]
            [ div [ class "container" ]
                [ div [ class "navbar-header" ]
                    [ a [ class "navbar-brand", Html.Attributes.href "/" ] --Route.href Route.Home ] -- fix home page not rendering in some cases
                        [ img [ src "/img/nav-header.png" ] [] ]
                    ]
                , div [ class "navbar-collapse collapse" ]
                    [ ul [ class "nav navbar-nav" ]
                        [ li [ class "dropdown" ]
                            [ a [ class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-expanded" "false" ]
                                [ text "Browse"
                                , span [ class "caret" ] []
                                ]
                            , ul
                                [ class "dropdown-menu", style [ ( "role", "menu" ) ] ]
                                [ li [] [ a [ Route.href Route.Projects ] [ text "Projects" ] ]
--                                , li [] [ a [ Route.href Route.ProjectGroups ] [ text "Project Groups" ] ]
--                                , li [] [ a [ Route.href Route.Domains ] [ text "Domains" ] ]
--                                , li [] [ a [ Route.href Route.Assemblies ] [ text "Assemblies" ] ]
--                                , li [] [ a [ Route.href Route.CombinedAssemblies ] [ text "CombinedAssemblies" ] ]
                                , li [] [ a [ Route.href Route.Samples ] [ text "Samples" ] ]
                                , li [] [ a [ Route.href Route.Investigators ] [ text "Investigators" ] ]
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
--                                , li [] [ a [ Route.href Route.Cart ] [ text ("Cart " ++ cartLabel) ] ]
                                , li [] [ a [ Route.href (Route.TaxonomySearch "") ] [ text "Taxonomy Search" ] ]
                                , li [] [ a [ Route.href (Route.ProteinSearch "") ] [ text "Protein Search" ] ]
                                ]
                            ]
                        , li []
                            [ a [ Html.Attributes.href "ftp://ftp.imicrobe.us" ]
                                [ text "Download" ]
                            ]
                        , loginMenuItem
                        ]
                    , helpButton
                    , dashboardButton
                    , cartButton
                    , searchBar
                    ]
                ]
            ]
        ]


--viewFooter : Html msg
--viewFooter =
--    footer [] []



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [--pageSubscriptions (getPage model.pageState)
          Sub.map SetSession (Ports.onSessionChange (Decode.decodeString Session.decoder >> Result.toMaybe))
        , Ports.onFileSelect SelectFile
        , Sub.map FileUploadFileSelected (Ports.fileUploadFileSelected (Decode.decodeString Ports.fileDecoder >> Result.toMaybe))
        , Sub.map FileUploadDone (Ports.fileUploadDone (Decode.decodeString (Request.Agave.responseDecoder Agave.decoderUploadResult) >> Result.toMaybe))
        , Time.every (10 * Time.second) PollTimerTick
        , Time.every (500 * Time.millisecond) InputTimerTick
        ]


pageSubscriptions : Page -> Sub Msg
pageSubscriptions page =
    case page of
        _ ->
            let
                _ = Debug.log "pageSubscriptions" "_"
            in
            Sub.none



---- PROGRAM ----


initialPage : Page
initialPage =
    Blank


type alias Flags =
    { session : String
    }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        _ = Debug.log "flags" flags

        _ = Debug.log "location" (toString location)

        session = --TODO use Maybe Session instead
            if flags.session == "" then
                Session.empty
            else
                decodeSessionFromJson flags.session

        model =
            { oauth =
                { authEndpoint = Config.oauthUrl
                , clientId = Config.oauthClientId
                , redirectUri = location.origin --location.origin ++ location.pathname
                }
            , session = { session | url = location.href }
            , query = ""
            , error = Nothing
            , pageState = Loaded initialPage
            , currentRoute = Nothing
            }

        -- Kludge for Agave not returning required "token_type=bearer" in OAuth redirect
        location2 =
            { location | hash = location.hash ++ "&token_type=bearer" }

        handleOAuthImplicit token state =
            let
                url =
                    case state of
                        Nothing -> ""
                        Just state -> state

                newSession =
                    { session | token = toString token }

                loadProfile =
                    Request.Agave.getProfile (toString token) |> Http.toTask |> Task.map .result

                handleProfile profile =
                    case profile of
                        Ok profile ->
                            LoadProfile profile

                        Err _ ->
                            let
                                _ = Debug.log "Error" "could not retrieve profile"
                            in
                            SetRoute (Just Route.Home) --FIXME go to Error page with a relevant error message instead
            in
            { model | session = newSession } => Cmd.batch
                [ Navigation.modifyUrl url
                , Task.attempt handleProfile loadProfile
                ]

        handleOAuthAuthorizationCode code =
            let
                authenticate =
                    Request.ORCID.authenticate "orcid" code (Maybe.map .user_id session.user) session.url
            in
                model ! [ Http.send AuthenticateORCID authenticate ]

        handleOAuthError error =
            case error of
                OAuth.Empty ->
                    let
                        _ =
                            Debug.log "OAuth.Empty" ""
                    in
                    setRoute (Route.fromLocation location) model

                (OAuth.OAuthErr err) ->
                    let
                        _ =
                            Debug.log "OAuth.OAuthErr" err
                    in
                    { model | error = Just <| OAuth.showErrCode err.error }
                        ! [ Navigation.modifyUrl model.oauth.redirectUri ]

                _ ->
                    let
                        _ =
                            Debug.log "Error" (toString error ++ toString location2)
                    in
                    { model | error = Just "parsing error" } ! []
    in
    case ( OAuth.Implicit.parse location2, OAuth.AuthorizationCode.parse location ) of
        ( Ok { token, state }, _ ) ->
            handleOAuthImplicit token state

        ( _, Ok { code } ) ->
            handleOAuthAuthorizationCode code

        ( Err error, _ ) ->
            handleOAuthError error


decodeSessionFromJson : String -> Session
decodeSessionFromJson json =
    json
        |> Decode.decodeString Session.decoder
        |> Result.withDefault Session.empty


main : Program Flags Model Msg
main =
    Navigation.programWithFlags (Route.fromLocation >> SetRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
