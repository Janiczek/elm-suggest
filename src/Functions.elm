module Functions exposing (..)


type alias Function1 =
    { name : String
    , checkFn : Type -> Maybe Type
    }


type alias Function2 =
    { name : String
    , checkFn : Type -> Type -> Maybe Type
    }


type Type
    = TInt Int
    | TFloat Float
    | TBool Bool
    | TChar Char
    | TString String



--| TTuple (List Type)
--| TList Type (List Type)
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
        \a ->
            case a of
                TList type_ vals ->
                    Just <| TInt <| List.length vals

                _ ->
                    Nothing
    }


listIsEmpty : Function1
listIsEmpty =
    { name = "List.isEmpty"
    , checkFn =
        \a ->
            case a of
                TList type_ vals ->
                    Just <| TBool <| List.isEmpty vals

                _ ->
                    Nothing
    }


intPlus : Function2
intPlus =
    { name = "(+)"
    , checkFn =
        \a b ->
            case a of
                TInt intA ->
                    case b of
                        TInt intB ->
                            Just <| TInt <| intA + intB

                        _ ->
                            Nothing

                _ ->
                    Nothing
    }


intMinus : Function2
intMinus =
    { name = "(-)"
    , checkFn =
        \a b ->
            case a of
                TInt intA ->
                    case b of
                        TInt intB ->
                            Just <| TInt <| intA - intB

                        _ ->
                            Nothing

                _ ->
                    Nothing
    }


floatPlus : Function2
floatPlus =
    { name = "(-)"
    , checkFn =
        \a b ->
            case a of
                TFloat floatA ->
                    case b of
                        TFloat floatB ->
                            Just <| TFloat <| floatA + floatB

                        _ ->
                            Nothing

                _ ->
                    Nothing
    }


floatMinus : Function2
floatMinus =
    { name = "(-)"
    , checkFn =
        \a b ->
            case a of
                TFloat floatA ->
                    case b of
                        TFloat floatB ->
                            Just <| TFloat <| floatA - floatB

                        _ ->
                            Nothing

                _ ->
                    Nothing
    }
