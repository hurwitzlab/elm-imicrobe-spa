module Page.Contact exposing (Model, Msg, init, update, view)

import Data.Profile as Profile exposing (Profile)
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
        -- Load page - Perform tasks to load the resources of a page
        title =
            Task.succeed "Contact Us"

        blank =
            Task.succeed ""

        email =
            case session.profile of
                Nothing -> blank

                Just profile -> Task.succeed profile.email

        name =
            case session.profile of
                Nothing -> blank

                Just profile -> Task.succeed (profile.first_name ++ " " ++ profile.last_name)
    in
    Task.map5 Model title name email blank (Task.succeed False)
        |> Task.mapError Error.handleLoadError



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
                _ = Debug.log "submit" model
            in
            model => submit model

        SubmitDone response ->
            { model | sent = True } => Cmd.none


submit : Model -> Cmd Msg
submit model =
    let
        url =
            apiBaseUrl ++ "/contact"
    in
    Http.send SubmitDone
        (Http.post url
            (encodeContact model)
            decoderContact
        )


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
    let
        _ = Debug.log "model" model
    in
    case model.sent of
        True ->
            div [ class "container" ]
                [ div [ class "row center gray", style [("font-size","1.5em"), ("padding", "3em")] ]
                    [ div [] [ text "We will respond to your message as soon as possible." ]
                    , div [] [ text "Thanks for your feedback and for using iMicrobe!" ]
                    ]
                ]

        False ->
            div [ class "container" ]
                [ div [ class "row" ]
                    [ h1 []
                        [ text (model.pageTitle) ]
                    ]
                , div [ class "row" ]
                    [ text "Please complete the form below to send us your bug reports, comments, and suggestions."
                    ]
                , Html.form [ style [("padding-top", "2em")] ]
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
                    , button [ type_ "submit", class "btn btn-primary", onClick Submit ] [ text "Submit" ]
                    ]
                ]