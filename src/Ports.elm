port module Ports exposing (storeSession, onSessionChange, createFileBrowser, onFileSelect, updateAnalytics)

import Data.App exposing (FileBrowser)



---- Session ----


port storeSession : String -> Cmd msg


port onSessionChange : (String -> msg) -> Sub msg



---- Agave File Browser ----


port createFileBrowser : FileBrowser -> Cmd msg


port onFileSelect : (FileBrowser -> msg) -> Sub msg



---- Google Analytics ----


port updateAnalytics : String -> Cmd msg
