module Page.Cart exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Sample
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Request.Sample
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import View.Page as Page
import View.Cart as Cart
import Set
import Debug
import Util exposing ((=>))


---- MODEL ----


type alias Model =
    { pageTitle : String
    , cart : Cart.Model
    , samples : List Data.Sample.Sample
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Cart"

        cart =
            Task.succeed (Cart.init session.cart)

        id_list =
            session.cart.contents |> Set.toList

        loadSamples =
            Request.Sample.getSome id_list |> Http.toTask

        handleLoadError _ =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home "The about page is currently unavailable."
    in
    Task.map3 Model title cart loadSamples
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | SetSession Session


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        CartMsg subMsg ->
            let
                toPage toModel toMsg subUpdate subMsg subModel =
                    let
                        ( newModel, newCmd ) =
                            subUpdate subMsg subModel
                    in
                    --( model, Cmd.map toMsg newCmd )
                    ( { model | cart = newModel }, Cmd.map toMsg newCmd )
            in
                toPage model.cart CartMsg (Cart.update session) subMsg model.cart

        SetSession newSession ->
            model => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    let
        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                count =
                    List.length model.samples

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]
    in
    div [ class "container" ]
        [ div [ class "row" ]
            [ h1 []
                [ text (model.pageTitle ++ " ")
                , numShowing
                ]
            , div [] [ viewCart model ]
            ]
        ]


viewCart : Model -> Html Msg
viewCart model =
    div [] [ Cart.viewCart model.cart model.samples |> Html.map CartMsg ]