module Main exposing (..)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Array.Hamt as Array exposing (Array)


main : Program Never Model Msg
main =
    H.beginnerProgram
        { model = model
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { inputs : Array (Maybe String)
    , output : Maybe String

    -- losing some data structures here but ¯\_(ツ)_/¯
    , suggestions : List String
    }


model : Model
model =
    { inputs = Array.fromList [ Nothing, Nothing ]
    , output = Nothing
    , suggestions = []
    }



-- UPDATE


type Msg
    = SetInput Int String
    | AddInput
    | RemoveInput Int
    | SetOutput String


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
    { model | inputs = model.inputs |> Array.set index (value |> emptyToNothing) }


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
    { model | output = value |> emptyToNothing }


updateSuggestions : Model -> Model
updateSuggestions model =
    model



-- HELPERS


emptyToNothing : String -> Maybe String
emptyToNothing string =
    if String.isEmpty string then
        Nothing
    else
        Just string



-- VIEW


view : Model -> Html Msg
view model =
    H.div [ HA.style [ ( "padding", "20px" ) ] ]
        [ H.h1 [] [ H.text "elm-suggest" ]
        , H.p []
            [ H.text "(inspired by "
            , H.a [ HA.href "https://guillaumesalles.github.io/resuggest/" ] [ H.text "resuggest" ]
            , H.text ")"
            ]
        , viewInputs model.inputs
        , viewOutput model.output
        , viewSuggestions model.suggestions
        ]


viewInputs : Array (Maybe String) -> Html Msg
viewInputs inputs =
    H.div []
        [ H.h2 [] [ H.text "Inputs" ]
        , H.p [] [ H.text "(the order doesn't matter)" ]
        , H.ul []
            ((inputs
                |> Array.toList
                |> List.indexedMap
                    (\index input ->
                        H.li []
                            [ H.input
                                [ HE.onInput (SetInput index)
                                , HA.value (input |> Maybe.withDefault "")
                                ]
                                []
                            , H.button [ HE.onClick (RemoveInput index) ] [ H.text "-" ]
                            ]
                    )
             )
                ++ [ H.li []
                        [ H.button
                            [ HE.onClick AddInput ]
                            [ H.text "+" ]
                        ]
                   ]
            )
        ]


viewOutput : Maybe String -> Html Msg
viewOutput value =
    H.div []
        [ H.h2 [] [ H.text "Output" ]
        , H.input
            [ HE.onInput SetOutput
            , HA.value (value |> Maybe.withDefault "")
            ]
            []
        ]


viewSuggestions : List String -> Html Msg
viewSuggestions functions =
    let
        suggestions =
            functions
                |> List.map (\fn -> H.li [] [ H.pre [] [ H.text fn ] ])
    in
        H.div []
            [ H.h2 [] [ H.text "Suggestions" ]
            , if List.isEmpty suggestions then
                H.p [] [ H.text "none" ]
              else
                H.ul [] suggestions
            ]
