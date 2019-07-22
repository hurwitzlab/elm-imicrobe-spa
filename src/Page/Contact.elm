module Page.Contact exposing (Model, Msg, init, update, view)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Page.Error as Error exposing (PageLoadError)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)
import Json.Encode as Encode
import Task exposing (Task)
import Util exposing ((=>))
import Config exposing (apiBaseUrl)



---- MODEL ----


type alias Model =
    { pageTitle : String
    , name : String
    , email : String
    , message : String
    , sent : Bool
    }


init : Session -> Task PageLoadError Model
init session =
    let
        email =
            session.user |> Maybe.map .email |> Maybe.withDefault ""

        name =
            session.user
                |> Maybe.map (\user -> user.first_name ++ " " ++ user.last_name)
                |> Maybe.withDefault ""
    in
    Task.succeed
        { pageTitle = "Contact Us"
        , name = name
        , email = email
        , message = ""
        , sent = False
        }



-- UPDATE --


type Msg
    = SetName String
    | SetEmail String
    | SetMessage String
    | Submit
    | SubmitDone (Result Http.Error ContactResponse)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetName name ->
            { model | name = name } => Cmd.none

        SetEmail email ->
            { model | email = email } => Cmd.none

        SetMessage message ->
            { model | message = message } => Cmd.none

        Submit ->
            let
                _ = Debug.log "Contact.Submit" model
            in
            { model | sent = True } => submit model

        SubmitDone response ->
            { model | sent = True } => Cmd.none


submit : Model -> Cmd Msg
submit model =
    let
        url =
            apiBaseUrl ++ "/contact"
    in
    Http.send SubmitDone
        (Http.post url (encodeContact model) decoderContact)


encodeContact : Model -> Http.Body
encodeContact model =
    Http.jsonBody <|
        Encode.object
            [ ("name", Encode.string model.name)
            , ("email", Encode.string model.email)
            , ("message", Encode.string model.message)
            ]


type alias ContactResponse =
    { status : String
    }


decoderContact : Decoder ContactResponse
decoderContact =
    decode ContactResponse
        |> Pipeline.required "status" Decode.string



-- VIEW --


view : Model -> Html Msg
view model =
    if model.sent then
        div [ class "container" ]
            [ div [ class "row center gray", style [("font-size","1.5em"), ("padding", "3em")] ]
                [ div [] [ text "We will respond to your message as soon as possible." ]
                , div [] [ text "Thanks for your feedback and for using iMicrobe!" ]
                ]
            ]
    else
        div [ class "container" ]
            [ div [ class "row" ]
                [ h1 []
                    [ text (model.pageTitle) ]
                ]
            , div [ class "row" ]
                [ p []
                    [ text "If you are looking for documentation, see the "
                    , a [ href "https://hurwitzlab.gitbook.io/imicrobe/", target "_blank" ] [ text "User Manual" ]
                    , text "."
                    ]
                , p []
                    [ text "To cite iMicrobe:" ]
                , p [ style [("padding-left", "5em")] ]
                    [ text "Youens-Clark,K., Bomhoff, M; Ponsero, A.; Wood-Charlson, E.; Lynch, J.; Choi, I., Hartman, J.H., Hurwitz, B.L., 2019. iMicrobe: Tools and data-driven discovery platform for the microbiome sciences. GigaScience. In Press." ]
                , br [] []
                , p []
                    [ text "Please complete the form below to send us your bug reports, comments, and suggestions." ]
                ]
            , div [ style [("padding-top", "1em")] ]
                [ div [ class "form-group" ]
                    [ label [ attribute "for" "name" ] [ text "Your name" ]
                    , input [ type_ "text", class "form-control", placeholder "Enter your name", value model.name, onInput SetName ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ attribute "for" "email" ] [ text "Your email" ]
                    , input [ type_ "email", class "form-control", placeholder "Enter your email", value model.email, onInput SetEmail ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ attribute "for" "message" ] [ text "Your message" ]
                    , textarea [ class "form-control", rows 3, onInput SetMessage ] []
                    ]
                , button [ class "btn btn-primary", onClick Submit ] [ text "Submit" ]
                ]
            ]
