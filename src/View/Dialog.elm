module View.Dialog exposing (confirmationDialogConfig, infoDialogConfig, errorDialogConfig)

import Dialog
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


confirmationDialogConfig : String -> msg -> msg -> Dialog.Config msg
confirmationDialogConfig confirmationText noMsg yesMsg =
    let
        content =
            div []
                [ p [] [ text confirmationText ]
                ]

        footer =
            div []
                [ button [ class "btn btn-default", onClick noMsg ] [ text "Cancel" ]
                , button [ class "btn btn-primary", onClick yesMsg ] [ text "Ok" ]
                ]
    in
    { closeMessage = Just noMsg
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Confirm" ])
    , body = Just content
    , footer = Just footer
    }


infoDialogConfig : String -> msg -> Dialog.Config msg
infoDialogConfig infoText closeMsg =
    let
        content =
            div [ class "alert alert-info" ]
                [ text infoText
                ]

        footer =
            div []
                [ button [ class "btn btn-primary", onClick closeMsg ] [ text "Ok" ]
                ]
    in
    { closeMessage = Just closeMsg
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Information" ])
    , body = Just content
    , footer = Just footer
    }


errorDialogConfig : String -> msg -> Dialog.Config msg
errorDialogConfig errorText closeMsg =
    let
        content =
            div [ class "alert alert-danger" ]
                [ p [] [ text "An error occurred:" ]
                , p [] [ text errorText ]
                ]

        footer =
            div []
                [ button [ class "btn btn-primary", onClick closeMsg ] [ text "Ok" ]
                ]
    in
    { closeMessage = Just closeMsg
    , containerClass = Nothing
    , header = Just (h3 [] [ text "Error" ])
    , body = Just content
    , footer = Just footer
    }