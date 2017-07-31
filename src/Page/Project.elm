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
            , viewInvestigators model.project.investigators
            , viewPubs model.project.publications
            , viewSamples model.project.samples
            , viewProjectGroups model.project.project_groups
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

        title =
            "Investigators (" ++ toString numInvs ++ ")"

        body =
            case numInvs of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ]
                        (List.map viewInvestigator investigators)
    in
    div []
        [ h2 [] [ text title ]
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


viewSamples : List Data.Project.Sample -> Html msg
viewSamples samples =
    let
        numSamples =
            List.length samples

        title =
            "Samples (" ++ toString numSamples ++ ")"

        cols =
            tr []
                (List.map (\s -> th [] [ text s ]) [ "Name", "Type" ])

        rows =
            List.map viewSample samples

        body =
            case numSamples of
                0 ->
                    text "NA"

                _ ->
                    table [] (cols :: rows)
    in
    div []
        [ h2 [] [ text title ]
        , body
        ]


viewSample : Data.Project.Sample -> Html msg
viewSample sample =
    tr []
        [ td []
            [ a [ Route.href (Route.Sample sample.sample_id) ]
                [ text sample.sample_name ]
            ]
        , td [] [ text sample.sample_type ]
        ]


viewPubs : List Data.Project.Publication -> Html msg
viewPubs pubs =
    let
        numPubs =
            List.length pubs

        title =
            "Publications (" ++ toString numPubs ++ ")"

        body =
            case numPubs of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ] (List.map pubRow pubs)
    in
    div []
        [ h2 [] [ text title ]
        , body
        ]


pubRow : Data.Project.Publication -> Html msg
pubRow pub =
    tr []
        [ td [] [ text pub.title ]
        ]


viewProjectGroups : List Data.Project.ProjectGroup -> Html msg
viewProjectGroups groups =
    let
        numGroups =
            List.length groups

        title =
            "Project Groups (" ++ toString numGroups ++ ")"

        body =
            case numGroups of
                0 ->
                    text "None"

                _ ->
                    table [ class "table" ] (List.map viewGroup groups)
    in
    div []
        [ h2 [] [ text title ]
        , body
        ]


viewGroup : Data.Project.ProjectGroup -> Html msg
viewGroup group =
    tr []
        [ td []
            [ a [ Route.href (Route.ProjectGroup group.project_group_id) ]
                [ text group.group_name ]
            ]
        ]
