module Page.Cart exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Data.Cart
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Error as Error exposing (PageLoadError, pageLoadError)
import Task exposing (Task)
import View.Page as Page
import View.Cart as Cart
import Set


---- MODEL ----


type alias Model =
    { pageTitle : String
    , cart : Cart.Model
    }


init : Session -> Task PageLoadError Model
init session =
    let
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Cart"

        cart =
            Task.succeed (Cart.init session.cart)

        handleLoadError _ =
            -- If a resource task fail load error page
            Error.pageLoadError Page.Home "The about page is currently unavailable."
    in
    Task.map2 Model title cart
        |> Task.mapError handleLoadError



-- UPDATE --


type Msg
    = CartMsg Cart.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    case msg of
        CartMsg subMsg ->
            ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ h2 [] [ text model.pageTitle ]
            , div [] [ viewCart model.cart ]
            ]
        ]


viewCart : Cart.Model -> Html Msg
viewCart cart =
    div [] [ Cart.viewCart cart |> Html.map CartMsg ]