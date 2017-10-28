module View.Cart exposing (Model, Msg(..), init, update, viewCart, addToCartButton, addToCartButton2, addAllToCartButton, size, CartType(..))

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
    | AddAllToCart (List Int)
    | RemoveAllFromCart (List Int)
    | ToggleSelectInCart Int
    | SelectAllInCart
    | UnselectAllInCart
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
                    Cart.add model.cart id

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession => SetCart newCart

        RemoveFromCart id ->
            let
                newCart =
                    Cart.remove model.cart id

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession => SetCart newCart

        AddAllToCart ids ->
            let
                newCart =
                    Cart.addList model.cart ids

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession => SetCart newCart

        RemoveAllFromCart ids ->
            let
                newCart =
                    Cart.removeList model.cart ids

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession => SetCart newCart

        ToggleSelectInCart id ->
            let
                selected =
                    case Cart.contains model.selected id of
                        True ->
                            Cart.remove model.selected id

                        False ->
                            Cart.add model.selected id
            in
            { model | selected = selected } => Cmd.none => NoOp

        SelectAllInCart ->
            let
                selected =
                    Cart.addList model.selected (Set.toList model.cart.contents)
            in
            { model | selected = selected } => Cmd.none => NoOp

        UnselectAllInCart ->
            let
                selected = Cart.empty
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
                    [ selectInCartColumn model
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
    div []
        [ div [ style [("overflow-y", "scroll"), ("max-height", "25em")] ]
            [ Table.view (config model) model.tableState (samplesInCart model.cart samples)
            ]
        , button [ class "btn btn-default btn-xs", onClick SelectAllInCart ] [ text "Select All" ]
        , button [ class "btn btn-default btn-xs", onClick UnselectAllInCart ] [ text "Unselect All" ]
        ]


selectInCartColumn : Model -> Table.Column Sample Msg
selectInCartColumn model =
    Table.veryCustomColumn
        { name = ""
        , viewData = (\s -> selectInCartLink model s)
        , sorter = Table.unsortable
        }


selectInCartLink : Model -> Sample -> Table.HtmlDetails Msg
selectInCartLink model sample =
    let
        isChecked =
            Set.member sample.sample_id model.selected.contents
    in
    Table.HtmlDetails []
        [ selectInCartCheckbox sample.sample_id isChecked -- |> Html.map (\_ -> ToggleSelectInCart sample.sample_id)
        ]


selectInCartCheckbox : Int -> Bool -> Html Msg
selectInCartCheckbox id isChecked =
    input [ type_ "checkbox", checked isChecked, onClick (ToggleSelectInCart id) ] []


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


-- Kludge
addToCartButton2 : Model -> Int -> Html Msg
addToCartButton2 model id =
    case (Set.member id model.cart.contents) of
        True ->
            button [ class "btn btn-default", onClick (RemoveFromCart id) ] [ text "Remove from Cart" ]

        False ->
            button [ class "btn btn-default", onClick (AddToCart id) ] [ text "Add to Cart" ]


addAllToCartButton : Model -> List Int -> Html Msg
addAllToCartButton model ids =
    let
        intersection =
            Set.intersect (Set.fromList ids) model.cart.contents |> Set.toList
    in
    case intersection of
        [] ->
            button [ class "btn btn-default btn-xs", onClick (AddAllToCart ids) ] [ text "Add all" ]

        _ ->
            button [ class "btn btn-default btn-xs", onClick (RemoveAllFromCart ids) ] [ text "Remove all" ]


samplesInCart : Cart -> List Sample -> List Sample
samplesInCart cart samples =
    List.filter (\sample -> Set.member sample.sample_id cart.contents) samples

size : Model -> Int
size model =
    Set.size model.cart.contents