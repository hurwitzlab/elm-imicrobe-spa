module View.Cart exposing (Model, Msg, init, update, viewCart, addToCartButton)

import Data.Session as Session exposing (Session)
import Data.Cart as Cart exposing (Cart)
import Ports as Ports
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Encode as Encode exposing (Value)
import Set
import Util exposing ((=>))


type Model
    = Model InternalModel


type alias InternalModel =
    { cart : Cart.Cart
    }


init : Cart.Cart -> Model -- Task Http.Error Model
init cart =
    Model (InternalModel cart)



-- UPDATE --


type Msg
    = AddToCart Int


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg (Model internalModel) =
    updateInternal session msg internalModel
        |> Tuple.mapFirst Model


updateInternal : Session -> Msg -> InternalModel -> ( InternalModel, Cmd Msg )
updateInternal session msg model =
    case msg of
        AddToCart id ->
            let
                newCart =
                    Cart (Set.insert id model.cart.contents)

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => storeSession newSession


storeSession : Session -> Cmd msg
storeSession session =
    Session.encode session
        |> Encode.encode 0
        |> Ports.storeSession



-- VIEW --


viewCart : Model -> Html Msg
viewCart model =
    div [] [ text (toString model) ]


addToCartButton : Int -> Html Msg
addToCartButton id =
    button [ onClick (AddToCart id) ] [ text "Add" ]