module View.Cart exposing (Model, Msg(..), init, update, viewCart, addToCartButton, size, CartType(..))

import Data.Session as Session exposing (Session)
import Data.Cart as Cart exposing (Cart)
import Data.Sample as Sample exposing (Sample)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Table exposing (defaultCustomizations)
import Route
import Util exposing ((=>))
import Set



type CartType
    = Selectable
    | Editable


type alias Model =
    { cart : Cart
    , tableState : Table.State
    , cartType : CartType
    , selected : Cart
    }


init : Cart -> CartType -> Model
init cart cartType =
    Model cart (Table.initialSort "Name") cartType Cart.empty



-- UPDATE --


type Msg
    = AddToCart Int
    | RemoveFromCart Int
    | SelectInCart Int
    | SetTableState Table.State
    | SetSession Session


type ExternalMsg
    = NoOp
    | SetCart Cart


update : Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update session msg model =
    case msg of
        AddToCart id ->
            let
                newCart =
                    Cart.add id model.cart

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession => SetCart newCart

        RemoveFromCart id ->
            let
                newCart =
                    Cart.remove id model.cart

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession => SetCart newCart

        SelectInCart id ->
            let
                selected =
                    Cart.add id model.selected
            in
            { model | selected = selected } => Cmd.none => NoOp

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none => NoOp

        SetSession newSession ->
            let
                newCart =
                    newSession.cart
            in
            { model | cart = newCart } => Cmd.none => NoOp



-- VIEW --


config : Model -> Table.Config Sample Msg
config model =
    let
        columns =
            case model.cartType of
                Editable ->
                    [ projectColumn
                    , nameColumn
                    , removeFromCartColumn
                    ]

                Selectable ->
                    [ selectInCartColumn
                    , projectColumn
                    , nameColumn
                    ]
    in
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetTableState
        , columns = columns
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


viewCart : Model -> List Sample -> Html Msg
viewCart model samples =
    div [] [ Table.view (config model) model.tableState (samplesInCart model.cart samples) ]


nameColumn : Table.Column Sample Msg
nameColumn =
    Table.veryCustomColumn
        { name = "Sample"
        , viewData = nameLink
        , sorter = Table.increasingOrDecreasingBy (String.toLower << .sample_name)
        }


nameLink : Sample -> Table.HtmlDetails Msg
nameLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Sample sample.sample_id) ]
            [ text <| Util.truncate sample.sample_name ]
        ]


projectColumn : Table.Column Sample Msg
projectColumn =
    Table.veryCustomColumn
        { name = "Project"
        , viewData = projectLink
        , sorter = Table.increasingOrDecreasingBy (.project >> .project_name >> String.toLower)
        }


projectLink : Sample -> Table.HtmlDetails Msg
projectLink sample =
    Table.HtmlDetails []
        [ a [ Route.href (Route.Project sample.project_id) ]
            [ text <| Util.truncate sample.project.project_name ]
        ]


removeFromCartColumn : Table.Column Sample Msg
removeFromCartColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = removeFromCartLink
        , sorter = Table.unsortable
        }


removeFromCartLink : Sample -> Table.HtmlDetails Msg
removeFromCartLink sample =
    Table.HtmlDetails []
        [ removeFromCartButton sample.sample_id |> Html.map (\_ -> RemoveFromCart sample.sample_id)
        ]


removeFromCartButton : Int -> Html Msg
removeFromCartButton id =
    button [ class "btn btn-default btn-xs", onClick (RemoveFromCart id) ] [ text "Remove" ]


addToCartButton : Model -> Int -> Html Msg
addToCartButton model id =
    case (Set.member id model.cart.contents) of
        True ->
            button [ class "btn btn-default btn-xs", onClick (RemoveFromCart id) ] [ text "Remove" ]

        False ->
            button [ class "btn btn-default btn-xs", onClick (AddToCart id) ] [ text "Add" ]


selectInCartColumn : Table.Column Sample Msg
selectInCartColumn =
    Table.veryCustomColumn
        { name = ""
        , viewData = selectInCartLink
        , sorter = Table.unsortable
        }


selectInCartLink : Sample -> Table.HtmlDetails Msg
selectInCartLink sample =
    Table.HtmlDetails []
        [ selectInCartCheckbox sample.sample_id |> Html.map (\_ -> SelectInCart sample.sample_id)
        ]


selectInCartCheckbox : Int -> Html Msg
selectInCartCheckbox id =
    input [ type_ "checkbox", onClick (SelectInCart id) ] []


samplesInCart : Cart -> List Sample -> List Sample
samplesInCart cart samples =
    List.filter (\sample -> Set.member sample.sample_id cart.contents) samples

size : Model -> Int
size model =
    Set.size model.cart.contents