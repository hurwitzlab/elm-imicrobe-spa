port module Ports exposing (onSessionChange, storeSession)

import Json.Encode exposing (Value)


port storeSession : String -> Cmd msg


port onSessionChange : (Value -> msg) -> Sub msg