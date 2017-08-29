module Functions exposing (..)


type alias Named a =
    { a | name : String }


type alias Function1 =
    Named { checkFn : Type -> Type -> Bool }


type alias Function2 =
    Named { checkFn : Type -> Type -> Type -> Bool }


type Type
    = TInt Int
    | TFloat Float
    | TBool Bool
    | TChar Char
    | TString String
    | TOrder Order
      -- TODO
    | TList (List Type)
    | TMaybe (Maybe Type)



--| TTuple (List Type)
--| TAny String
--| TFunction (List Type) Type
--| TRecord (List Field)
--| TExtensibleRecord String (List Field)
--type alias Field =
--    ( String, Type )
-- FN DATABASE


functions1 : List Function1
functions1 =
    [ listLength
    , listIsEmpty
    , listHead
    ]


functions2 : List Function2
functions2 =
    [ plus
    , minus
    , equal
    , notEqual
    , lessThan
    , greaterThan
    ]



-- DEFINITIONS


listLength : Function1
listLength =
    { name = "List.length"
    , checkFn =
        \a output ->
            case a of
                TList vals ->
                    case output of
                        TInt intOutput ->
                            List.length vals == intOutput

                        _ ->
                            False

                _ ->
                    False
    }


listIsEmpty : Function1
listIsEmpty =
    { name = "List.isEmpty"
    , checkFn =
        \a output ->
            case a of
                TList vals ->
                    case output of
                        TBool boolOutput ->
                            List.isEmpty vals == boolOutput

                        _ ->
                            False

                _ ->
                    False
    }


listHead : Function1
listHead =
    { name = "List.head"
    , checkFn =
        \a output ->
            case a of
                TList vals ->
                    case output of
                        TMaybe maybeOutput ->
                            List.head vals == maybeOutput

                        _ ->
                            False

                _ ->
                    False
    }


plus : Function2
plus =
    { name = "(+)"
    , checkFn =
        \a b output ->
            case output of
                TInt intOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    toFloat (intA + intB) == toFloat intOutput

                                TFloat floatB ->
                                    (toFloat intA + floatB) == toFloat intOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA + toFloat intB) == toFloat intOutput

                                TFloat floatB ->
                                    (floatA + floatB) == toFloat intOutput

                                _ ->
                                    False

                        _ ->
                            False

                TFloat floatOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    toFloat (intA + intB) == floatOutput

                                TFloat floatB ->
                                    (toFloat intA + floatB) == floatOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA + toFloat intB) == floatOutput

                                TFloat floatB ->
                                    (floatA + floatB) == floatOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }


minus : Function2
minus =
    { name = "(-)"
    , checkFn =
        \a b output ->
            case output of
                TInt intOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    toFloat (intA - intB) == toFloat intOutput

                                TFloat floatB ->
                                    (toFloat intA - floatB) == toFloat intOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA - toFloat intB) == toFloat intOutput

                                TFloat floatB ->
                                    (floatA - floatB) == toFloat intOutput

                                _ ->
                                    False

                        _ ->
                            False

                TFloat floatOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    toFloat (intA - intB) == floatOutput

                                TFloat floatB ->
                                    (toFloat intA - floatB) == floatOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA - toFloat intB) == floatOutput

                                TFloat floatB ->
                                    (floatA - floatB) == floatOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }


equal : Function2
equal =
    { name = "(==)"
    , checkFn =
        \a b output ->
            case output of
                TBool boolOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    (intA == intB) == boolOutput

                                TFloat floatB ->
                                    (toFloat intA == floatB) == boolOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA == toFloat intB) == boolOutput

                                TFloat floatB ->
                                    (floatA == floatB) == boolOutput

                                _ ->
                                    False

                        -- TODO remaining types (think about all of them)
                        TBool boolA ->
                            case b of
                                TBool boolB ->
                                    (boolA == boolB) == boolOutput

                                _ ->
                                    False

                        TString stringA ->
                            case b of
                                TString stringB ->
                                    (stringA == stringB) == boolOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }


notEqual : Function2
notEqual =
    { name = "(/=)"
    , checkFn =
        \a b output ->
            case output of
                TBool boolOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    (intA /= intB) == boolOutput

                                TFloat floatB ->
                                    (toFloat intA /= floatB) == boolOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA /= toFloat intB) == boolOutput

                                TFloat floatB ->
                                    (floatA /= floatB) == boolOutput

                                _ ->
                                    False

                        TBool boolA ->
                            case b of
                                TBool boolB ->
                                    (boolA /= boolB) == boolOutput

                                _ ->
                                    False

                        TString stringA ->
                            case b of
                                TString stringB ->
                                    (stringA /= stringB) == boolOutput

                                _ ->
                                    False

                        -- TODO remaining types (think about all of them)
                        _ ->
                            False

                _ ->
                    False
    }


lessThan : Function2
lessThan =
    { name = "(<)"
    , checkFn =
        \a b output ->
            case output of
                TBool boolOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    (intA < intB) == boolOutput

                                TFloat floatB ->
                                    (toFloat intA < floatB) == boolOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA < toFloat intB) == boolOutput

                                TFloat floatB ->
                                    (floatA < floatB) == boolOutput

                                _ ->
                                    False

                        TString stringA ->
                            case b of
                                TString stringB ->
                                    (stringA < stringB) == boolOutput

                                _ ->
                                    False

                        -- TODO remaining types (chars, lists, tuples)
                        _ ->
                            False

                _ ->
                    False
    }


greaterThan : Function2
greaterThan =
    { name = "(>)"
    , checkFn =
        \a b output ->
            case output of
                TBool boolOutput ->
                    case a of
                        TInt intA ->
                            case b of
                                TInt intB ->
                                    (intA > intB) == boolOutput

                                TFloat floatB ->
                                    (toFloat intA > floatB) == boolOutput

                                _ ->
                                    False

                        TFloat floatA ->
                            case b of
                                TInt intB ->
                                    (floatA > toFloat intB) == boolOutput

                                TFloat floatB ->
                                    (floatA > floatB) == boolOutput

                                _ ->
                                    False

                        TString stringA ->
                            case b of
                                TString stringB ->
                                    (stringA > stringB) == boolOutput

                                _ ->
                                    False

                        -- TODO remaining types (chars, lists, tuples)
                        _ ->
                            False

                _ ->
                    False
    }
