module Page.Cart exposing (Model, Msg(..), ExternalMsg(..), init, update, view)

import Data.Session as Session exposing (Session)
import Data.Sample as Sample exposing (Sample)
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
    , samples : List Sample
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
    | SetSamples (List Sample)


type ExternalMsg
    = NoOp
    | SetCart Data.Cart.Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        CartMsg subMsg ->
            let
                _ = Debug.log "Cart.CartMsg" (toString subMsg)

                ( ( newCart, subCmd ), msgFromPage ) =
                    Cart.update session subMsg model.cart
            in
            { model | cart = newCart } => Cmd.map CartMsg subCmd => SetCart newCart.cart

        Files ->
            model => Route.modifyUrl Route.Files => NoOp

        EmptyCart ->
            let
                newCart =
                    Data.Cart.Cart Set.empty

                newSession =
                    { session | cart = newCart }
            in
            { model | samples = [] } => Session.store newSession => SetCart newCart

        SetSession newSession ->
            let
                _ = Debug.log "Page.Cart.SetSession" (toString newSession)

                newCart =
                    Cart.init newSession.cart Cart.Editable

                id_list =
                    newSession.cart.contents |> Set.toList

                loadSamples =
                    Request.Sample.getSome id_list |> Http.toTask

                handleSamples samples =
                    case samples of
                        Ok samples ->
                            let
                                (subModel, cmd) = Cart.update newSession (Cart.SetSession newSession) model.cart
                            in
                            SetSamples samples

                        Err _ ->
                            let
                                _ = Debug.log "Error" "could not retrieve samples"
                            in
                            SetSamples []
            in
            { model | cart = newCart } => Task.attempt handleSamples loadSamples => NoOp

        SetSamples newSamples ->
            { model | samples = newSamples } => Cmd.none => NoOp



-- VIEW --


view : Model -> Html Msg
view model =
    let
        count = Cart.size model.cart

        numShowing =
            let
                myLocale =
                    { usLocale | decimals = 0 }

                numStr =
                    count |> toFloat |> format myLocale
            in
            case count of
                0 ->
                    span [] []

                _ ->
                    span [ class "badge" ]
                        [ text numStr ]

        isEmpty =
            case count of
                0 -> True

                _ -> False

        buttonAttr =
            case isEmpty of
                True -> [ attribute "disabled" "" ]

                False -> []
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
    case (Cart.size model.cart) of
        0 -> text "The cart is empty"

        _ ->
            div [] [ Cart.viewCart model.cart model.samples |> Html.map CartMsg ]