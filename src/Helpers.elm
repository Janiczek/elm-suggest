module Helpers exposing (..)


debugIfErr : String -> Result a b -> Result a b
debugIfErr input result =
    case result of
        Ok _ ->
            result

        Err err ->
            let
                _ =
                    Debug.log input err
            in
                result
