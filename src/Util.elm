module Util exposing ((=>), truncate, capitalize)

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


-- toTuple
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>
