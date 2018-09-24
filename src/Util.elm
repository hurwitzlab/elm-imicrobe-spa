module Util exposing (..)

import Char
import String


maxColumnWidth : Int
maxColumnWidth =
    40


truncate : String -> String
truncate string =
    if String.length string <= maxColumnWidth then
        string
    else
        String.left (maxColumnWidth - 3) string ++ "..."


capitalize : String -> String
capitalize string =
    case String.uncons string of
        Nothing -> ""
        Just (head, tail) ->
            String.cons (Char.toUpper head) tail


pluralize : String -> Int -> String
pluralize str count =
    if count == 1 then
        str
    else
        str ++ "s"


removeTrailingSlash : String -> String
removeTrailingSlash path =
    case String.startsWith "/" path of
        True ->
            String.dropLeft 1 path

        False ->
            path


isUrl : String -> Bool
isUrl s =
    String.startsWith "http://" s
        || String.startsWith "https://" s
        || String.startsWith "ftp://" s


-- pretty inefficient
dropFileName : String -> String
dropFileName s =
    String.split "/" s |> List.reverse |> List.drop 1 |> List.reverse |> String.join "/"


-- toTuple
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>
