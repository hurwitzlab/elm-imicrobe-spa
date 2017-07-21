module Page.Project exposing (Model, Msg, init, update, view)

import Data.Project
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Project
import Route
import Task exposing (Task)
import View.Page as Page


---- MODEL ----


type alias Model =
    { pageTitle : String
    , project_id : Int
    , project : Data.Project.Project
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Project"

        loadProject =
            Request.Project.get id |> Http.toTask

        handleLoadError err =
            -- If a resource task fail load error page
            let
                errMsg =
                    case err of
                        Http.BadStatus response ->
                            case String.length response.body of
                                0 ->
                                    "Bad status"

                                _ ->
                                    response.body

                        _ ->
                            toString err
            in
            Error.pageLoadError Page.Home errMsg
    in
    Task.map3 Model title (Task.succeed id) loadProject
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = Todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Todo ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , viewProject model.project
            ]
        ]



{--
    , project_name : String
    , project_code : String
    , project_type : String
    , description : String
    , read_file : String
    , meta_file : String
    , assembly_file : String
    , peptide_file : String
    , num_samples : String
    , domains : List Domain
    , investigators : List Investigator
    --}


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

        numInvs =
            List.length project.investigators

        invText =
            case numInvs of
                1 ->
                    "Investigator (1)"

                _ ->
                    "Investigators (" ++ toString numInvs ++ ")"

        numPubs =
            List.length project.publications

        pubsText =
            case numPubs of
                1 ->
                    "Publication (1)"

                _ ->
                    "Publications (" ++ toString numPubs ++ ")"
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
            [ th [] [ text invText ]
            , td [] (viewInvestigators project.investigators)
            ]
        , tr []
            [ th [] [ text domainText ]
            , td [] (viewDomains project.domains)
            ]
        , tr []
            [ th [] [ text pubsText ]
            , td [] (viewPublications project.publications)
            ]
        ]


viewInvestigator : Data.Project.Investigator -> Html msg
viewInvestigator investigator =
    a [ Route.href (Route.Investigator investigator.investigator_id) ]
        [ text investigator.investigator_name ]


viewInvestigators : List Data.Project.Investigator -> List (Html msg)
viewInvestigators investigators =
    case List.length investigators of
        0 ->
            [ text "None" ]

        _ ->
            List.intersperse
                (text ", ")
                (List.map viewInvestigator investigators)


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


viewPublications : List Data.Project.Publication -> List (Html msg)
viewPublications pubs =
    case List.length pubs of
        0 ->
            [ text "None" ]

        _ ->
            List.intersperse
                (text ", ")
                (List.map viewPublication pubs)
