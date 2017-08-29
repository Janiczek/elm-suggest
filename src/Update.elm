module Update exposing (update)

import String.Extra as String
import Types exposing (Model, Msg(..))
import Array.Hamt as Array
import Functions exposing (Type(..))
import Parse exposing (parse)


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



-- HELPERS


updateSuggestions : Model -> Model
updateSuggestions model =
    let
        parsedInputs =
            model.inputs
                |> Array.toList
                |> List.filterMap identity
                |> List.map parse
                |> List.filterMap identity

        parsedOutput =
            model.output
                |> Maybe.andThen parse

        satisfactoryFns =
            case parsedOutput of
                Nothing ->
                    []

                Just output ->
                    case parsedInputs of
                        -- TODO make the order not matter, possibly with List.Extra.permutations
                        [ a ] ->
                            satisfactoryFns1 a output

                        [ a, b ] ->
                            satisfactoryFns2 a b output

                        _ ->
                            []
    in
        { model | suggestions = satisfactoryFns }


satisfactoryFns1 : Type -> Type -> List String
satisfactoryFns1 a output =
    Functions.functions1
        |> List.filter (\fn -> fn.checkFn a output)
        |> List.map .name


satisfactoryFns2 : Type -> Type -> Type -> List String
satisfactoryFns2 a b output =
    Functions.functions2
        |> List.filter (\fn -> fn.checkFn a b output)
        |> List.map .name
