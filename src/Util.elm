module Util exposing ((=>), apiHost, truncate)


-- TODO move into config.json
apiHost : String
apiHost =
    "http://localhost:3006"
    --"http://spa.imicrobe.us/api/v1"


maxColumnWidth : Int
maxColumnWidth =
    40


truncate : String -> String
truncate string =
    if String.length string <= maxColumnWidth then
        string
    else
        String.left (maxColumnWidth - 3) string ++ "..."


-- toTuple
(=>) : a -> b -> ( a, b )
(=>) =
    (,)


{-| infixl 0 means the (=>) operator has the same precedence as (<|) and (|>),
meaning you can use it at the end of a pipeline and have the precedence work out.
-}
infixl 0 =>
