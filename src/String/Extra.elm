module String.Extra exposing (..)


emptyToNothing : String -> Maybe String
emptyToNothing string =
    if String.isEmpty string then
        Nothing
    else
        Just string
