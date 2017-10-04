module Page.Cart exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Sample
import Data.Cart
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (usLocale)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Request.Sample
import Page.Error as Error exposing (PageLoadError)
import Route
import Task exposing (Task)
import View.Cart as Cart
import Set
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
            Task.succeed (Cart.init session.cart Cart.Editable)

        id_list =
            session.cart.contents |> Set.toList

        loadSamples =
            Request.Sample.getSome id_list |> Http.toTask
    in
    Task.map3 Model title cart loadSamples
        |> Task.mapError Error.handleLoadError



-- UPDATE --


type Msg
    = CartMsg Cart.Msg
    | Files
    | EmptyCart
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
                    ( { model | cart = newModel }, Cmd.map toMsg newCmd )
            in
                toPage model.cart CartMsg (Cart.update session) subMsg model.cart

        Files ->
            model => Route.modifyUrl Route.Files

        EmptyCart ->
            let
                newCart =
                    Data.Cart.Cart Set.empty

                newSession =
                    { session | cart = newCart }
            in
            { model | samples = [] } => Session.store newSession

        SetSession newSession ->
            model => Cmd.none



-- VIEW --


view : Model -> Html Msg
view model =
    let
        isEmpty =
            case model.samples of
                [] ->  True

                _ -> False

        buttonAttr =
            case isEmpty of
                True -> [ attribute "disabled" "" ]

                False -> []

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
                , div [ class "right" ]
                    [ button ([ class "margin-right btn btn-primary btn-sm", onClick Files ] ++ buttonAttr) [ span [ class "glyphicon glyphicon-file"] [], text " Files" ]
--                    , button [ class "margin-right btn btn-primary btn-sm", attribute "type" "submit" ] [ text "Download" ]
                    , button ([ class "btn btn-primary btn-sm", onClick EmptyCart ] ++ buttonAttr)
                        [ span [ class "glyphicon glyphicon-trash"] []
                        , text " Empty"
                        ]
                    ]
                ]
            , div []
                [ viewCart model
                ]
            ]
        ]


viewCart : Model -> Html Msg
viewCart model =
    case model.samples of
        [] -> text "The cart is empty"

        _ ->
            div [] [ Cart.viewCart model.cart model.samples |> Html.map CartMsg ]