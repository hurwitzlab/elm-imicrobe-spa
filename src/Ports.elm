port module Ports exposing (..)

import Data.App exposing (FileBrowser)



---- Session ----


port storeSession : String -> Cmd msg


port onSessionChange : (String -> msg) -> Sub msg



---- Agave File Browser ----


port createFileBrowser : FileBrowser -> Cmd msg


port onFileSelect : (FileBrowser -> msg) -> Sub msg



---- Google Analytics ----


port updateAnalytics : String -> Cmd msg



---- Sequence Similarity Plots ----


port createSimPlot : (String, List (String, String)) -> Cmd msg



---- Scroll To Top ----


port scrollToTop : String -> Cmd msg