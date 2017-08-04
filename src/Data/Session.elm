module Data.Session exposing (Session)

import Set


type alias Session =
    { cart : Set.Set Int
    }
