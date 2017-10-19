module Main exposing (..)

import Config as Config
import Data.Session as Session exposing (Session)
import Data.Profile
import Data.App
import Debug exposing (log)
import Json.Decode as Decode exposing (Value)
import Html exposing (..)
import Http
import Navigation exposing (Location)
import OAuth
import OAuth.Implicit
import Page.App as App
import Page.Apps as Apps
import Page.Assembly as Assembly
import Page.Assemblies as Assemblies
import Page.Cart as Cart
import Page.CombinedAssembly as CombinedAssembly
import Page.CombinedAssemblies as CombinedAssemblies
import Page.Contact as Contact
import Page.Domain as Domain
import Page.Domains as Domains
import Page.Files as Files
import Page.Error as Error exposing (PageLoadError, redirectLoadError)
import Page.Home as Home
import Page.Investigator as Investigator
import Page.Investigators as Investigators
import Page.Job as Job
import Page.Jobs as Jobs
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
import Request.Agave
import Request.Login
import Ports
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
    | MetaSearch MetaSearch.Model
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
    | Search Search.Model


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
    | Authorize (Result PageLoadError Home.Model)
    | Deauthorize (Result PageLoadError Home.Model)
    | CartLoaded (Result PageLoadError Cart.Model)
    | CartMsg Cart.Msg
    | CombinedAssemblyLoaded Int (Result PageLoadError CombinedAssembly.Model)
    | CombinedAssemblyMsg CombinedAssembly.Msg
    | CombinedAssembliesLoaded (Result PageLoadError CombinedAssemblies.Model)
    | CombinedAssembliesMsg CombinedAssemblies.Msg
    | ContactLoaded (Result PageLoadError Contact.Model)
    | ContactMsg Contact.Msg
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
    | MetaSearchLoaded (Result PageLoadError MetaSearch.Model)
    | MetaSearchMsg MetaSearch.Msg
    | LoadProfile Data.Profile.Profile
    | LoginRecorded Request.Login.Login
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
    | SearchLoaded (Result PageLoadError Search.Model)
    | SearchMsg Search.Msg
    | SetRoute (Maybe Route)
    | SetSession (Maybe Session)
    | SelectFile Data.App.FileBrowser


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    let
        routeString =
            case maybeRoute of
                Nothing -> "Nothing"

                Just route -> Route.routeToString route

        transition toMsg task =
            { model | pageState = TransitioningFrom (getPage model.pageState) }
                => Cmd.batch [ Ports.updateAnalytics routeString, Task.attempt toMsg task ]

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

        Just Route.Home ->
            transition HomeLoaded (Home.init model.session)

        Just Route.Domains ->
            transition DomainsLoaded Domains.init

        Just (Route.Domain id) ->
            transition (DomainLoaded id) (Domain.init id)

        Just Route.Files ->
            transition FilesLoaded (Files.init model.session)

        Just (Route.Investigator id) ->
            transition (InvestigatorLoaded id) (Investigator.init id)

        Just Route.Investigators ->
            transition InvestigatorsLoaded Investigators.init

        Just Route.Jobs ->
            transition JobsLoaded (Jobs.init model.session)

        Just (Route.Job id) ->
            transition (JobLoaded id) (Job.init model.session id)

        Just Route.Login ->
            transition Authorize (Home.init model.session)

        Just Route.Logout ->
            transition Deauthorize (Home.init model.session)

        Just Route.Pubchase ->
            transition PubchaseLoaded Pubchase.init

        Just Route.Publications ->
            transition PublicationsLoaded Publications.init

        Just (Route.Publication id) ->
            transition (PublicationLoaded id) (Publication.init id)

        Just Route.Profile ->
            transition ProfileLoaded (Profile.init model.session)

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
            transition SamplesLoaded (Samples.init model.session)

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

    in
    case msg of
        SetRoute route ->
            setRoute route model

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

        Deauthorize (Ok subModel) ->
            let
                newSession =
                    { session | token = "", profile = Nothing }
            in
            { model | session = newSession } => Cmd.batch [ Session.store newSession, Route.modifyUrl Route.Home ]

        AppLoaded id (Ok subModel) ->
            { model | pageState = Loaded (App id subModel) } => Cmd.none

        AppLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        AppMsg subMsg ->
            case page of
                App id subModel ->
                    toPage (App id) AppMsg (App.update session) subMsg subModel

                _ ->
                    model => Cmd.none

        AppsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Apps subModel) } => Cmd.none

        AppsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        AppsMsg subMsg ->
            case page of
                Apps subModel ->
                    toPage Apps AppsMsg Apps.update subMsg subModel

                _ ->
                    model => Cmd.none

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

        CartLoaded (Ok subModel) ->
            { model | pageState = Loaded (Cart subModel) } => Cmd.none

        CartLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        CartMsg subMsg ->
            case page of
                Cart subModel ->
                    let
                        _ = Debug.log "Main.CartMsg" (toString subMsg)

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

        ContactLoaded (Ok subModel) ->
            { model | pageState = Loaded (Contact subModel) } => Cmd.none

        ContactLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        ContactMsg subMsg ->
            case page of
                Contact subModel ->
                    toPage Contact ContactMsg Contact.update subMsg subModel

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

        FilesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Files subModel) } => Cmd.none

        FilesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        FilesMsg subMsg ->
            case page of
                Files subModel ->
                    toPage Files FilesMsg Files.update subMsg subModel

                _ ->
                    model => Cmd.none

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

        JobsLoaded (Ok subModel) ->
            { model | pageState = Loaded (Jobs subModel) } => Cmd.none

        JobsLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

        JobsMsg subMsg ->
            case page of
                Jobs subModel ->
                    ( toPage Jobs JobsMsg Jobs.update subMsg subModel )

                _ ->
                    model => Cmd.none

        JobLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Job id subModel) } => Cmd.none

        JobLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

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

        LoadProfile profile ->
            let
                session = model.session

                newSession =
                    { session | profile = Just profile }

                recordLogin username =
                    Request.Login.record username |> Http.toTask |> Task.attempt handleRecordLogin

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
                _ = Debug.log "login" login

                session = model.session

                newSession =
                    { session | user_id = Just login.user_id }
            in
            { model | session = newSession } => Cmd.batch [ Session.store newSession, Route.modifyUrl Route.Home ]

        ProfileLoaded (Ok subModel) ->
            { model | pageState = Loaded (Profile subModel) } => Cmd.none

        ProfileLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => redirectLoadError error

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

        SampleLoaded id (Ok subModel) ->
            { model | pageState = Loaded (Sample id subModel) } => Cmd.none

        SampleLoaded id (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SampleMsg subMsg ->
            case page of
                Sample id subModel ->
                    toPage (Sample id) SampleMsg Sample.update subMsg subModel

                _ ->
                    model => Cmd.none

        SamplesLoaded (Ok subModel) ->
            { model | pageState = Loaded (Samples subModel) } => Cmd.none

        SamplesLoaded (Err error) ->
            { model | pageState = Loaded (Error error) } => Cmd.none

        SamplesMsg subMsg ->
            case page of
                Samples subModel ->
                    let
                        _ = Debug.log "Main.SamplesMsg" (toString subMsg)

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

        HomeMsg subMsg ->
            let
                x = Debug.log (toString subMsg)
            in
            case page of
                Home subModel ->
                    toPage Home HomeMsg Home.update subMsg subModel

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

        SetSession newSession ->
            let
                 _ = Debug.log "Main.SetSession" (toString newSession)
            in
            case newSession of
                Just newSession ->
                    case page of
                        Cart subModel ->
--                            toPage Cart CartMsg (Cart.update newSession) (Cart.SetSession newSession) subModel
                            let
                                ( ( pageModel, cmd ), msgFromPage ) =
                                    Cart.update model.session (Cart.SetSession newSession) subModel
                            in
                            { model | pageState = Loaded (Cart pageModel) }
                                => Cmd.map CartMsg cmd

                        Samples subModel ->
--                            toPage Samples SamplesMsg (Samples.update newSession) (Samples.SetSession newSession) subModel
                            let
                                ( ( pageModel, cmd ), msgFromPage ) =
                                    Samples.update model.session (Samples.SetSession newSession) subModel
                            in
                            { model | pageState = Loaded (Samples pageModel) }
                                => Cmd.map SamplesMsg cmd

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
                            App.update session (App.SetInput selection.id selection.path) subModel
                    in
                    { model | pageState = Loaded (App id pageModel) } => Cmd.map AppMsg cmd

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
            Page.layout isLoading session
    in
    case page of
        NotFound ->
            layout Page.Other NotFound.view

        Blank ->
            -- This is for the very intial page load, while we are loading
            -- data via HTTP. We could also render a spinner here.
            Html.text ""
                |> layout Page.Other

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

        Cart subModel ->
            Cart.view subModel
                |> layout Page.Cart
                |> Html.map CartMsg

        CombinedAssembly id subModel ->
            CombinedAssembly.view subModel
                |> layout Page.CombinedAssembly
                |> Html.map CombinedAssemblyMsg

        CombinedAssemblies subModel ->
            CombinedAssemblies.view subModel
                |> layout Page.CombinedAssemblies
                |> Html.map CombinedAssembliesMsg

        Contact subModel ->
            Contact.view subModel
                |> layout Page.Contact
                |> Html.map ContactMsg

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

        Files subModel ->
            Files.view subModel
                |> layout Page.Files
                |> Html.map FilesMsg

        Home subModel ->
            Home.view subModel -- session subModel
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

        Job id subModel ->
            Job.view subModel
                |> layout Page.Job
                |> Html.map JobMsg

        Jobs subModel ->
            Jobs.view subModel
                |> layout Page.Jobs
                |> Html.map JobsMsg

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

        Profile subModel ->
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
    Sub.batch
        [--pageSubscriptions (getPage model.pageState)
          Sub.map SetSession sessionChange
        , Ports.onFileSelect SelectFile
        ]


sessionChange : Sub (Maybe Session)
sessionChange =
    Ports.onSessionChange (Decode.decodeString Session.decoder >> Result.toMaybe)


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

        session = --TODO use Maybe Session instead
            case flags.session of
                "" -> Session.empty

                _ -> decodeSessionFromJson flags.session

        _ = Debug.log "location" (toString location)

        model =
            { oauth =
                { authEndpoint = Config.oauthUrl
                , clientId = Config.oauthClientId
                , redirectUri = location.origin --location.origin ++ location.pathname
                }
            , session = session
            , error = Nothing
            , pageState = Loaded initialPage
            }

        -- Kludge for Agave not returning required "token_type=bearer" in redirect
        location2 =
            { location | hash = location.hash ++ "&token_type=bearer" }

        _ = Debug.log "init" model
    in
    case OAuth.Implicit.parse location2 of
        Ok { token } ->
            let
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
            { model | session = newSession } => Task.attempt handleProfile loadProfile

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
