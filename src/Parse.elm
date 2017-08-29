module Parse exposing (parse)

import Json.Decode as JD exposing (Decoder)
import Functions exposing (Type(..))


parse : String -> Maybe Type
parse string =
    JD.decodeString any string
        |> Result.toMaybe


any : Decoder Type
any =
    JD.oneOf allDecoders


allDecoders : List (Decoder Type)
allDecoders =
    (nonrecursive ++ recursive)


nonrecursive : List (Decoder Type)
nonrecursive =
    [ JD.string |> JD.map TString
    , bool
    , JD.int |> JD.map TInt
    , JD.float |> JD.map TFloat
    ]


recursive : List (Decoder Type)
recursive =
    (nonrecursive
        |> List.map JD.list
        |> List.map (JD.map TList)
     -- TODO ++ other things than lists
    )


bool : Decoder Type
bool =
    JD.string
        |> JD.andThen
            (\string ->
                case string of
                    "True" ->
                        JD.succeed (TBool True)

                    "False" ->
                        JD.succeed (TBool False)

                    _ ->
                        JD.fail ""
            )


maybe : Decoder Type
maybe =
    JD.string
        |> JD.andThen
            (\string ->
                if string |> String.startsWith "Just " then
                    case parse (string |> String.dropLeft 5) of
                        Nothing ->
                            JD.fail ""

                        Just type_ ->
                            JD.succeed (TMaybe (Just type_))
                else if string == "Nothing" then
                    JD.succeed (TMaybe Nothing)
                else
                    JD.fail ""
            )
