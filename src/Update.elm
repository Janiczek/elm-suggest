module Update exposing (update)

import String.Extra as String
import Types exposing (Model, Msg(..))
import Array.Hamt as Array


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetInput index value ->
            model
                |> setInput index value
                |> updateSuggestions

        AddInput ->
            model
                |> addInput

        RemoveInput index ->
            model
                |> removeInput index
                |> updateSuggestions

        SetOutput value ->
            model
                |> setOutput value
                |> updateSuggestions



-- UPDATE SUBFUNCTIONS


setInput : Int -> String -> Model -> Model
setInput index value model =
    { model | inputs = model.inputs |> Array.set index (value |> String.emptyToNothing) }


addInput : Model -> Model
addInput model =
    { model | inputs = model.inputs |> Array.push Nothing }


removeInput : Int -> Model -> Model
removeInput index model =
    { model
        | inputs =
            Array.append
                (Array.slice 0 index model.inputs)
                (Array.slice (index + 1) (Array.length model.inputs) model.inputs)
    }


setOutput : String -> Model -> Model
setOutput value model =
    { model | output = value |> String.emptyToNothing }


updateSuggestions : Model -> Model
updateSuggestions model =
    model
