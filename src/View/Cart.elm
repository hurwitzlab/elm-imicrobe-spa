module View.Cart exposing (Model, Msg, init, update, viewCart, addToCartButton)

import Data.Session as Session exposing (Session)
import Data.Cart as Cart exposing (Cart)
import Data.Sample as Sample exposing (Sample)
import Ports as Ports
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Encode as Encode exposing (Value)
import Table exposing (defaultCustomizations)
import Route
import Set
import Util exposing ((=>))


type Model
    = Model InternalModel


type alias InternalModel =
    { cart : Cart.Cart
    , tableState : Table.State
    }


init : Cart.Cart -> Model -- Task Http.Error Model
init cart =
    Model (InternalModel cart (Table.initialSort "Name"))



-- UPDATE --


type Msg
    = AddToCart Int
    | RemoveFromCart Int
    | SetTableState Table.State
    | SetSession Session

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
            { model | cart = newCart } => Cmd.batch [ Session.store newSession ]--, Route.modifyUrl Route.Cart ]

        RemoveFromCart id ->
            let
                newCart =
                    Cart (Set.remove id model.cart.contents)

                newSession =
                    { session | cart = newCart }
            in
            { model | cart = newCart } => Session.store newSession

        SetTableState newState ->
            { model | tableState = newState } => Cmd.none

        SetSession newSession ->
            model => Cmd.none



-- VIEW --


config : Table.Config Sample Msg
config =
    Table.customConfig
        { toId = toString << .sample_id
        , toMsg = SetTableState
        , columns =
            [ projectColumn
            , nameColumn
            , removeFromCartColumn
            ]
        , customizations =
            { defaultCustomizations | tableAttrs = toTableAttrs }
        }


toTableAttrs : List (Attribute Msg)
toTableAttrs =
    [ attribute "class" "table"
    ]


viewCart : Model -> List Sample -> Html Msg
viewCart (Model internalModel) samples =
    div [] [ Table.view config internalModel.tableState samples ]


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


addToCartButton : Int -> Html Msg
addToCartButton id =
    button [ class "btn btn-default btn-xs", onClick (AddToCart id) ] [ text "Add" ]