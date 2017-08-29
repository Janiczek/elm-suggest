module Functions exposing (..)


type alias Function1 =
    { name : String
    , checkFn : Type -> Type -> Bool
    }


type alias Function2 =
    { name : String
    , checkFn : Type -> Type -> Type -> Bool
    }


type Type
    = TInt Int
    | TFloat Float
    | TBool Bool
    | TChar Char
    | TString String
    | TList Type (List Type)



--| TTuple (List Type)
--| TAny String
--| TFunction (List Type) Type
--| TRecord (List Field)
--| TExtensibleRecord String (List Field)
--| TUserType String (List Type)


type alias Field =
    ( String, Type )



-- FN DATABASE


functions1 : List Function1
functions1 =
    [ listLength
    , listIsEmpty
    ]


functions2 : List Function2
functions2 =
    [ intPlus
    , intMinus
    , floatPlus
    , floatMinus
    ]



-- DEFINITIONS


listLength : Function1
listLength =
    { name = "List.length"
    , checkFn =
        \a output ->
            case a of
                TList type_ vals ->
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
                TList type_ vals ->
                    case output of
                        TBool boolOutput ->
                            List.isEmpty vals == boolOutput

                        _ ->
                            False

                _ ->
                    False
    }


intPlus : Function2
intPlus =
    { name = "(+)"
    , checkFn =
        \a b output ->
            case a of
                TInt intA ->
                    case b of
                        TInt intB ->
                            case output of
                                TInt intOutput ->
                                    (intA + intB) == intOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }


intMinus : Function2
intMinus =
    { name = "(-)"
    , checkFn =
        \a b output ->
            case a of
                TInt intA ->
                    case b of
                        TInt intB ->
                            case output of
                                TInt intOutput ->
                                    (intA - intB) == intOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }


floatPlus : Function2
floatPlus =
    { name = "(-)"
    , checkFn =
        \a b output ->
            case a of
                TFloat floatA ->
                    case b of
                        TFloat floatB ->
                            case output of
                                TFloat floatOutput ->
                                    (floatA + floatB) == floatOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }


floatMinus : Function2
floatMinus =
    { name = "(-)"
    , checkFn =
        \a b output ->
            case a of
                TFloat floatA ->
                    case b of
                        TFloat floatB ->
                            case output of
                                TFloat floatOutput ->
                                    (floatA - floatB) == floatOutput

                                _ ->
                                    False

                        _ ->
                            False

                _ ->
                    False
    }
