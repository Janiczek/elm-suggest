module Types exposing (..)

import Array.Hamt exposing (Array)


type alias Model =
    { inputs : Array (Maybe String)
    , output : Maybe String

    -- losing some data structures here but ¯\_(ツ)_/¯
    , suggestions : List String
    }


type Msg
    = SetInput Int String
    | AddInput
    | RemoveInput Int
    | SetOutput String
