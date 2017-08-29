module View exposing (view)

import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Types exposing (Model, Msg(..))
import Array.Hamt as Array exposing (Array)
import Parse exposing (parse)
import Functions
import List.Extra as List


view : Model -> Html Msg
view model =
    H.div [ HA.style [ ( "padding", "20px" ) ] ]
        [ viewHeader
        , viewCaveats
        , viewInputs model.inputs
        , viewOutput model.output
        , viewSuggestions model.suggestions
        , viewDatabase
        ]


viewHeader : Html Msg
viewHeader =
    H.div []
        [ H.h1 [] [ H.text "elm-suggest" ]
        , H.p []
            [ H.text "(inspired by "
            , H.a [ HA.href "https://guillaumesalles.github.io/resuggest/" ] [ H.text "resuggest" ]
            , H.text ")"
            ]
        ]


viewCaveats : Html Msg
viewCaveats =
    H.div []
        [ H.h4 [] [ H.text "Caveats:" ]
        , H.ul []
            [ H.li [] [ H.text "Strings with escaped double quotes don't work" ]
            , H.li [] [ H.text "Order doesn't work" ]
            , H.li [] [ H.text "Lists don't work" ]
            , H.li [] [ H.text "Tuples don't work" ]
            , H.li [] [ H.text "Maybes don't work" ]
            , H.li [] [ H.text "Results don't work" ]
            ]
        ]


viewInputs : Array (Maybe String) -> Html Msg
viewInputs inputs =
    H.div []
        [ H.h2 [] [ H.text "Inputs" ]
        , H.p [] [ H.text "(TODO in the future the order won't matter :) )" ]
        , H.ul []
            ((inputs
                |> Array.toList
                |> List.indexedMap
                    (\index input ->
                        let
                            value =
                                input
                                    |> Maybe.withDefault ""

                            parsedValue =
                                parse value
                        in
                            H.li []
                                [ H.input
                                    [ HE.onInput (SetInput index)
                                    , HA.value value
                                    , parseResultBgColor parsedValue
                                    ]
                                    []
                                , H.button
                                    [ HE.onClick (RemoveInput index) ]
                                    [ H.text "-" ]
                                , H.pre
                                    [ HA.style
                                        [ ( "display", "inline-block" )
                                        , ( "padding-left", "10px" )
                                        , ( "margin", "0" )
                                        ]
                                    ]
                                    [ parsedValue
                                        |> Maybe.map toString
                                        |> Maybe.withDefault ""
                                        |> H.text
                                    ]
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
viewOutput output =
    let
        value =
            output |> Maybe.withDefault ""

        parsedValue =
            parse value
    in
        H.div []
            [ H.h2 [] [ H.text "Output" ]
            , H.input
                [ HE.onInput SetOutput
                , HA.value value
                , parseResultBgColor parsedValue
                ]
                []
            , H.pre
                [ HA.style
                    [ ( "display", "inline-block" )
                    , ( "padding-left", "10px" )
                    ]
                ]
                [ parsedValue
                    |> Maybe.map toString
                    |> Maybe.withDefault ""
                    |> H.text
                ]
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


viewDatabase : Html Msg
viewDatabase =
    let
        allNames =
            (Functions.functions1 |> List.map .name)
                ++ (Functions.functions2 |> List.map .name)
                |> List.unique
    in
        H.div []
            [ H.h2 [] [ H.text "Chooses from:" ]
            , H.ul []
                (allNames
                    |> List.map (\name -> H.li [] [ H.text name ])
                )
            ]


parseResultBgColor : Maybe a -> H.Attribute Msg
parseResultBgColor parsedValue =
    HA.style
        [ ( "background-color"
          , if parsedValue == Nothing then
                "#FDD9D7"
            else
                "#EEF3BD"
          )
        ]
