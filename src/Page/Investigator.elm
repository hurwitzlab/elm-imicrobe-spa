module Page.Investigator exposing (Model, Msg, init, update, view)

import Data.Investigator
import Dict
import Exts.Dict as EDict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Request.Investigator
import Task exposing (Task)
import View.Page as Page


---- MODEL ----
-- , investigator : Dict.Dict String String


type alias Model =
    { pageTitle : String
    , investigator_id : Int
    , investigator : Data.Investigator.Investigator
    }


init : Int -> Task PageLoadError Model
init id =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Investigator"

        loadInvestigator =
            Request.Investigator.get id |> Http.toTask

        handleLoadError err =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home (toString err)
    in
    Task.map3 Model title (Task.succeed id) loadInvestigator
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
    let
        inv =
            model.investigator
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , table [ class "table" ]
                [ tr []
                    [ th [] [ text "Name" ]
                    , td [] [ text inv.investigator_name ]
                    ]
                , tr []
                    [ th [] [ text "Institution" ]
                    , td [] [ text inv.institution ]
                    ]
                ]
            ]
        ]



{--
view : Model -> Html Msg
view model =
    let
        inv =
            model.investigator

        name =
            EDict.getWithDefault "NA" "investigator_name" inv

        inst =
            EDict.getWithDefault "NA" "institution" inv
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , table [ class "table" ]
                [ tr []
                    [ th [] [ text "Name" ]
                    , td [] [ text name ]
                    ]
                , tr []
                    [ th [] [ text "Institution" ]
                    , td [] [ text inst ]
                    ]
                ]
            ]
        ]
        --}
