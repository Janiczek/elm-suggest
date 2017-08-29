module Main exposing (..)

import Html
import Array.Hamt as Array exposing (Array)
import View exposing (view)
import Types exposing (Model, Msg)
import Update exposing (update)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , update = update
        , view = view
        }



-- MODEL


model : Model
model =
    { inputs = Array.fromList [ Nothing, Nothing ]
    , output = Nothing
    , suggestions = []
    }
