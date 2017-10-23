module Page.Project exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Project
import Data.Session as Session exposing (Session)
import Data.Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError)
import Request.Project
import Route
import Task exposing (Task)
import View.Cart as Cart
import Util exposing ((=>))



---- MODEL ----


type alias Model =
    { pageTitle : String
    , project_id : Int
    , project : Data.Project.Project
    , cart : Cart.Model
    }


init : Session -> Int -> Task PageLoadError Model
init session id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Project"

        loadProject =
            Request.Project.get id |> Http.toTask

        cart =
            Task.succeed (Cart.init session.cart Cart.Editable)
    in
    Task.map4 Model title (Task.succeed id) loadProject cart
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = CartMsg Cart.Msg


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        CartMsg subMsg ->
            let
                _ = Debug.log "Samples.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "page-header" ]
                [ h1 []
                    [ text (model.pageTitle ++ " ")
                    , small []
                        [ text model.project.project_name ]
                    ]
                ]
            , viewProject model.project
            , viewInvestigators model.project.investigators
            , viewPubs model.project.publications
            , viewSamples model.cart model.project.samples
            ]
        ]


viewProject : Data.Project.Project -> Html msg
viewProject project =
    let
        numDomains =
            List.length project.domains

        domainText =
            case numDomains of
                1 ->
                    "Domain (1)"

                _ ->
                    "Domains (" ++ toString numDomains ++ ")"
    in
    table [ class "table" ]
        [ tr []
            [ th [] [ text "Name" ]
            , td [] [ text project.project_name ]
            ]
        , tr []
            [ th [] [ text "Code" ]
            , td [] [ text project.project_code ]
            ]
        , tr []
            [ th [] [ text "Project Type" ]
            , td [] [ text project.project_type ]
            ]
        , tr []
            [ th [] [ text domainText ]
            , td [] (viewDomains project.domains)
            ]
        , tr []
            [ th [] [ text "Groups" ]
            , td [] (viewProjectGroups project.project_groups)
            ]
        ]

viewInvestigator : Data.Project.Investigator -> Html msg
viewInvestigator investigator =
    tr []
        [ td []
            [ a [ Route.href (Route.Investigator investigator.investigator_id) ]
                [ text investigator.investigator_name ]
            ]
        ]


viewInvestigators : List Data.Project.Investigator -> Html msg
viewInvestigators investigators =
    let
        numInvs =
            List.length investigators

        label =
            case numInvs of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numInvs)
                        ]

        body =
            case numInvs of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ]
                        [ tbody [] (List.map viewInvestigator investigators) ]
    in
    div []
        [ h2 []
            [ text "Investigators "
            , label
            ]
        , body
        ]


viewDomain : Data.Project.Domain -> Html msg
viewDomain domain =
    text domain.domain_name


viewDomains : List Data.Project.Domain -> List (Html msg)
viewDomains domains =
    case List.length domains of
        0 ->
            [ text "None" ]

        _ ->
            List.intersperse (text ", ") (List.map viewDomain domains)


viewPublication : Data.Project.Publication -> Html msg
viewPublication pub =
    let
        authorList =
            case pub.author of
                "" ->
                    ""

                "NA" ->
                    ""

                _ ->
                    " (" ++ pub.author ++ ")"
    in
    text (pub.title ++ authorList)


viewSamples : Cart.Model -> List Data.Project.Sample -> Html Msg
viewSamples cart samples =
    let
        numSamples =
            List.length samples

        label =
            case numSamples of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numSamples)
                        ]
        cols =
            tr []
                (List.map (\s -> th [] [ text s ]) [ "Name", "Type", "Cart" ])

        rows =
            List.map (viewSample cart) samples

        body =
            case numSamples of
                0 ->
                    text "NA"

                _ ->
                    table [ class "table table-condensed" ] [ tbody [] (cols :: rows) ]
    in
    div []
        [ h2 []
            [ text "Samples "
            , label
            ]
        , body
        ]


viewSample : Cart.Model -> Data.Project.Sample -> Html Msg
viewSample cart sample =
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        , td [ class "col-md-1" ] [ Cart.addToCartButton cart sample.sample_id |> Html.map CartMsg ]
        ]


viewPubs : List Data.Project.Publication -> Html msg
viewPubs pubs =
    let
        numPubs =
            List.length pubs

        label =
            case numPubs of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text (toString numPubs)
                        ]

        body =
            case numPubs of
                0 ->
                    text "None"

                _ ->
                    table [ class "table table-condensed" ] [ tbody [] (List.map pubRow pubs) ]
    in
    div []
        [ h2 []
            [ text "Publications "
            , label
            ]
        , body
        ]


pubRow : Data.Project.Publication -> Html msg
pubRow pub =
    tr []
        [ td []
            [ a [ Route.href (Route.Publication pub.publication_id) ]
                [ text pub.title ]
            ]
        ]


viewProjectGroup : Data.Project.ProjectGroup -> Html msg
viewProjectGroup group =
    a [ Route.href (Route.ProjectGroup group.project_group_id) ]
        [ text group.group_name ]


viewProjectGroups : List Data.Project.ProjectGroup -> List (Html msg)
viewProjectGroups groups =
    case List.length groups of
        0 ->
            [ text "None" ]

        _ ->
            List.intersperse (text ", ") (List.map viewProjectGroup groups)