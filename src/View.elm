module View exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Types exposing (Model, Msg(..))
import Array.Hamt as Array exposing (Array)


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
