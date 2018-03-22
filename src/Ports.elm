port module Ports exposing (..)

import Data.App exposing (FileBrowser)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, optional, required)




---- Session ----


port storeSession : String -> Cmd msg


port onSessionChange : (String -> msg) -> Sub msg



---- Agave File Browser ----


port createFileBrowser : FileBrowser -> Cmd msg


port onFileSelect : (FileBrowser -> msg) -> Sub msg



---- Google Analytics ----


port updateAnalytics : String -> Cmd msg



---- Sequence Similarity Plots ----


-- TODO change to accept record instead of list of tuples
port createSimPlot : (String, List (String, String, String)) -> Cmd msg



---- Scroll To Top ----


port scrollToTop : String -> Cmd msg



---- File Upload ----


type alias FileToUpload =
    { name : String
    , size : Int
    , type_ : String
    }


fileDecoder : Decoder FileToUpload
fileDecoder =
    decode FileToUpload
        |> required "name" Decode.string
        |> required "size" Decode.int
        |> required "type" Decode.string


port fileUploadOpenBrowser : (String, String) -> Cmd msg


port fileUploadFileSelected : (String -> msg) -> Sub msg


port fileUploadDone : (String -> msg) -> Sub msg
