module Parse exposing (parse)

import Functions exposing (Type(..))
import Parser as P exposing (Parser, (|.), (|=))
import Char


parse : String -> Maybe Type
parse string =
    P.run any string
        |> (\x ->
                case x of
                    Ok _ ->
                        x

                    Err err ->
                        let
                            _ =
                                Debug.log string err
                        in
                            x
           )
        |> Result.toMaybe


any : Parser Type
any =
    P.oneOf allParsers


allParsers : List (Parser Type)
allParsers =
    [ only string |> P.map TString
    , only bool
    , only number
    ]


spaces : Parser ()
spaces =
    P.fail ""


only : Parser a -> Parser a
only parser =
    P.succeed identity
        |= parser
        |. P.end


string : Parser String
string =
    P.succeed identity
        |. P.symbol "\""
        |= P.keep P.zeroOrMore (\c -> c /= '"')
        |. P.symbol "\""


bool : Parser Type
bool =
    P.oneOf
        [ P.keyword "True" |> P.map (always (TBool True))
        , P.keyword "False" |> P.map (always (TBool False))
        ]


number : Parser Type
number =
    P.keep P.oneOrMore
        (\c ->
            Char.isHexDigit c
                || List.member c [ '.', 'x', '-' ]
        )
        |> P.andThen
            (\string ->
                case String.toInt string of
                    Err _ ->
                        case String.toFloat string of
                            Err err2 ->
                                P.fail err2

                            Ok float ->
                                if isNaN float then
                                    P.fail "Float NaN"
                                else if isInfinite float then
                                    P.fail "Float infinite"
                                else
                                    P.succeed (TFloat float)

                    Ok int ->
                        if isNaN (toFloat int) then
                            P.fail "Int NaN"
                        else if isInfinite (toFloat int) then
                            P.fail "Int infinite"
                        else
                            P.succeed (TInt int)
            )


maybe : Parser Type
maybe =
    string
        |> P.andThen
            (\string ->
                if string |> String.startsWith "Just " then
                    case parse (string |> String.dropLeft 5) of
                        Nothing ->
                            P.fail ""

                        Just type_ ->
                            P.succeed (TMaybe (Just type_))
                else if string == "Nothing" then
                    P.succeed (TMaybe Nothing)
                else
                    P.fail ""
            )
