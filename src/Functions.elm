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
      -- TODO parsing
    | TList (List Type)
    | TTuple (List Type)
    | TMaybe (Maybe Type)
    | TResult (Result Type)



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
    , not_
    , negate_
    , abs_
    , sqrt_
    ]


functions2 : List Function2
functions2 =
    [ plus
    , minus
    , multiply
    , divide
    , power
    , integerDivision
    , remainder
    , modulo
    , equal
    , notEqual
    , lessThan
    , greaterThan
    , lessOrEqualThan
    , greaterOrEqualThan
    , max_
    , min_
    , compare_
    , and
    , or
    , xor_
    ]



-- DEFINITIONS


listLength : Function1
listLength =
    { name = "List.length"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TList list, TInt out ) ->
                    List.length list == out

                _ ->
                    False
    }


listIsEmpty : Function1
listIsEmpty =
    { name = "List.isEmpty"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TList list, TBool out ) ->
                    List.isEmpty list == out

                _ ->
                    False
    }


listHead : Function1
listHead =
    { name = "List.head"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TList list, TMaybe out ) ->
                    List.head list == out

                _ ->
                    False
    }


not_ : Function1
not_ =
    { name = "not"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TBool a, TBool out ) ->
                    not a == out

                _ ->
                    False
    }


negate_ : Function1
negate_ =
    { name = "negate"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TInt a, TInt out ) ->
                    negate a == out

                ( TInt a, TFloat out ) ->
                    toFloat (negate a) == out

                ( TFloat a, TInt out ) ->
                    negate a == toFloat out

                ( TFloat a, TFloat out ) ->
                    negate a == out

                _ ->
                    False
    }


abs_ : Function1
abs_ =
    { name = "abs"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TInt a, TInt out ) ->
                    abs a == out

                ( TInt a, TFloat out ) ->
                    toFloat (abs a) == out

                ( TFloat a, TInt out ) ->
                    abs a == toFloat out

                ( TFloat a, TFloat out ) ->
                    abs a == out

                _ ->
                    False
    }


sqrt_ : Function1
sqrt_ =
    { name = "sqrt"
    , checkFn =
        \a output ->
            case ( a, output ) of
                ( TFloat a, TFloat out ) ->
                    sqrt a == out

                _ ->
                    False
    }


plus : Function2
plus =
    { name = "(+)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    a + b == out

                ( TInt a, TFloat b, TInt out ) ->
                    toFloat a + b == toFloat out

                ( TFloat a, TInt b, TInt out ) ->
                    a + toFloat b == toFloat out

                ( TFloat a, TFloat b, TInt out ) ->
                    a + b == toFloat out

                ( TInt a, TInt b, TFloat out ) ->
                    toFloat (a + b) == out

                ( TInt a, TFloat b, TFloat out ) ->
                    toFloat a + b == out

                ( TFloat a, TInt b, TFloat out ) ->
                    a + toFloat b == out

                ( TFloat a, TFloat b, TFloat out ) ->
                    a + b == out

                _ ->
                    False
    }


minus : Function2
minus =
    { name = "(-)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    a - b == out

                ( TInt a, TFloat b, TInt out ) ->
                    toFloat a - b == toFloat out

                ( TFloat a, TInt b, TInt out ) ->
                    a - toFloat b == toFloat out

                ( TFloat a, TFloat b, TInt out ) ->
                    a - b == toFloat out

                ( TInt a, TInt b, TFloat out ) ->
                    toFloat (a - b) == out

                ( TInt a, TFloat b, TFloat out ) ->
                    toFloat a - b == out

                ( TFloat a, TInt b, TFloat out ) ->
                    a - toFloat b == out

                ( TFloat a, TFloat b, TFloat out ) ->
                    a - b == out

                _ ->
                    False
    }


multiply : Function2
multiply =
    { name = "(*)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    a * b == out

                ( TInt a, TFloat b, TInt out ) ->
                    toFloat a * b == toFloat out

                ( TFloat a, TInt b, TInt out ) ->
                    a * toFloat b == toFloat out

                ( TFloat a, TFloat b, TInt out ) ->
                    a * b == toFloat out

                ( TInt a, TInt b, TFloat out ) ->
                    toFloat (a * b) == out

                ( TInt a, TFloat b, TFloat out ) ->
                    toFloat a * b == out

                ( TFloat a, TInt b, TFloat out ) ->
                    a * toFloat b == out

                ( TFloat a, TFloat b, TFloat out ) ->
                    a * b == out

                _ ->
                    False
    }


divide : Function2
divide =
    { name = "(/)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TFloat a, TFloat b, TFloat out ) ->
                    a / b == out

                _ ->
                    False
    }


power : Function2
power =
    { name = "(^)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    a ^ b == out

                ( TInt a, TFloat b, TInt out ) ->
                    toFloat a ^ b == toFloat out

                ( TFloat a, TInt b, TInt out ) ->
                    a ^ toFloat b == toFloat out

                ( TFloat a, TFloat b, TInt out ) ->
                    a ^ b == toFloat out

                ( TInt a, TInt b, TFloat out ) ->
                    toFloat (a ^ b) == out

                ( TInt a, TFloat b, TFloat out ) ->
                    toFloat a ^ b == out

                ( TFloat a, TInt b, TFloat out ) ->
                    a ^ toFloat b == out

                ( TFloat a, TFloat b, TFloat out ) ->
                    a ^ b == out

                _ ->
                    False
    }


integerDisivion : Function2
integerDisivion =
    { name = "(//)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    a // b == out

                _ ->
                    False
    }


remainder : Function2
remainder =
    { name = "rem"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    rem a b == out

                _ ->
                    False
    }


modulo : Function2
modulo =
    { name = "(%)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    a % b == out

                _ ->
                    False
    }


equal : Function2
equal =
    { name = "(==)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TBool out ) ->
                    (a == b) == out

                ( TInt a, TFloat b, TBool out ) ->
                    (toFloat a == b) == out

                ( TFloat a, TInt b, TBool out ) ->
                    (a == toFloat b) == out

                ( TFloat a, TFloat b, TBool out ) ->
                    (a == b) == out

                ( TBool a, TBool b, TBool out ) ->
                    (a == b) == out

                ( TString a, TString b, TBool out ) ->
                    (a == b) == out

                ( TChar a, TChar b, TBool out ) ->
                    (a == b) == out

                ( TOrder a, TOrder b, TBool out ) ->
                    (a == b) == out

                ( TTuple a, TTuple b, TBool out ) ->
                    (a == b) == out

                ( TList a, TList b, TBool out ) ->
                    (a == b) == out

                ( TMaybe a, TMaybe b, TBool out ) ->
                    (a == b) == out

                ( TResult a, TResult b, TBool out ) ->
                    (a == b) == out

                -- TODO some more types?
                _ ->
                    False
    }


notEqual : Function2
notEqual =
    { name = "(/=)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TBool out ) ->
                    (a /= b) == out

                ( TInt a, TFloat b, TBool out ) ->
                    (toFloat a /= b) == out

                ( TFloat a, TInt b, TBool out ) ->
                    (a /= toFloat b) == out

                ( TFloat a, TFloat b, TBool out ) ->
                    (a /= b) == out

                ( TBool a, TBool b, TBool out ) ->
                    (a /= b) == out

                ( TString a, TString b, TBool out ) ->
                    (a /= b) == out

                ( TChar a, TChar b, TBool out ) ->
                    (a /= b) == out

                ( TOrder a, TOrder b, TBool out ) ->
                    (a /= b) == out

                ( TTuple a, TTuple b, TBool out ) ->
                    (a /= b) == out

                ( TList a, TList b, TBool out ) ->
                    (a /= b) == out

                ( TMaybe a, TMaybe b, TBool out ) ->
                    (a /= b) == out

                ( TResult a, TResult b, TBool out ) ->
                    (a /= b) == out

                -- TODO some more types?
                _ ->
                    False
    }


lessThan : Function2
lessThan =
    { name = "(<)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TBool out ) ->
                    (a < b) == out

                ( TInt a, TFloat b, TBool out ) ->
                    (toFloat a < b) == out

                ( TFloat a, TInt b, TBool out ) ->
                    (a < toFloat b) == out

                ( TFloat a, TFloat b, TBool out ) ->
                    (a < b) == out

                ( TString a, TString b, TBool out ) ->
                    (a < b) == out

                ( TChar a, TChar b, TBool out ) ->
                    (a < b) == out

                --( TTuple a, TTuple b, TBool out ) ->
                --    (a < b) == out
                --( TList a, TList b, TBool out ) ->
                --    (a < b) == out
                _ ->
                    False
    }


greaterThan : Function2
greaterThan =
    { name = "(>)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TBool out ) ->
                    (a > b) == out

                ( TInt a, TFloat b, TBool out ) ->
                    (toFloat a > b) == out

                ( TFloat a, TInt b, TBool out ) ->
                    (a > toFloat b) == out

                ( TFloat a, TFloat b, TBool out ) ->
                    (a > b) == out

                ( TString a, TString b, TBool out ) ->
                    (a > b) == out

                ( TChar a, TChar b, TBool out ) ->
                    (a > b) == out

                --( TTuple a, TTuple b, TBool out ) ->
                --    (a > b) == out
                --( TList a, TList b, TBool out ) ->
                --    (a > b) == out
                _ ->
                    False
    }


lessOrEqualThan : Function2
lessOrEqualThan =
    { name = "(<=)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TBool out ) ->
                    (a <= b) == out

                ( TInt a, TFloat b, TBool out ) ->
                    (toFloat a <= b) == out

                ( TFloat a, TInt b, TBool out ) ->
                    (a <= toFloat b) == out

                ( TFloat a, TFloat b, TBool out ) ->
                    (a <= b) == out

                ( TString a, TString b, TBool out ) ->
                    (a <= b) == out

                ( TChar a, TChar b, TBool out ) ->
                    (a <= b) == out

                --( TTuple a, TTuple b, TBool out ) ->
                --    (a <= b) == out
                --( TList a, TList b, TBool out ) ->
                --    (a <= b) == out
                _ ->
                    False
    }


greaterOrEqualThan : Function2
greaterOrEqualThan =
    { name = "(>=)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TBool out ) ->
                    (a >= b) == out

                ( TInt a, TFloat b, TBool out ) ->
                    (toFloat a >= b) == out

                ( TFloat a, TInt b, TBool out ) ->
                    (a >= toFloat b) == out

                ( TFloat a, TFloat b, TBool out ) ->
                    (a >= b) == out

                ( TString a, TString b, TBool out ) ->
                    (a >= b) == out

                ( TChar a, TChar b, TBool out ) ->
                    (a >= b) == out

                --( TTuple a, TTuple b, TBool out ) ->
                --    (a >= b) == out
                --( TList a, TList b, TBool out ) ->
                --    (a >= b) == out
                _ ->
                    False
    }


max_ : Function2
max_ =
    { name = "max"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    max a b == out

                ( TInt a, TInt b, TFloat out ) ->
                    toFloat (max a b) == out

                ( TInt a, TFloat b, TInt out ) ->
                    max (toFloat a) b == toFloat out

                ( TInt a, TFloat b, TFloat out ) ->
                    max (toFloat a) b == out

                ( TFloat a, TInt b, TInt out ) ->
                    max a (toFloat b) == toFloat out

                ( TFloat a, TInt b, TFloat out ) ->
                    max a (toFloat b) == out

                ( TFloat a, TFloat b, TInt out ) ->
                    max a b == toFloat out

                ( TFloat a, TFloat b, TFloat out ) ->
                    max a b == out

                ( TString a, TString b, TString out ) ->
                    max a b == out

                ( TChar a, TChar b, TChar out ) ->
                    max a b == out

                --( TTuple a, TTuple b, TTuple out ) ->
                --    max a b == out
                --( TList a, TList b, TList out ) ->
                --    max a b == out
                _ ->
                    False
    }


min_ : Function2
min_ =
    { name = "min"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TInt out ) ->
                    min a b == out

                ( TInt a, TInt b, TFloat out ) ->
                    toFloat (min a b) == out

                ( TInt a, TFloat b, TInt out ) ->
                    min (toFloat a) b == toFloat out

                ( TInt a, TFloat b, TFloat out ) ->
                    min (toFloat a) b == out

                ( TFloat a, TInt b, TInt out ) ->
                    min a (toFloat b) == toFloat out

                ( TFloat a, TInt b, TFloat out ) ->
                    min a (toFloat b) == out

                ( TFloat a, TFloat b, TInt out ) ->
                    min a b == toFloat out

                ( TFloat a, TFloat b, TFloat out ) ->
                    min a b == out

                ( TString a, TString b, TString out ) ->
                    min a b == out

                ( TChar a, TChar b, TChar out ) ->
                    min a b == out

                --( TTuple a, TTuple b, TTuple out ) ->
                --    min a b == out
                --( TList a, TList b, TList out ) ->
                --    min a b == out
                _ ->
                    False
    }


compare_ : Function2
compare_ =
    { name = "compare"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TInt a, TInt b, TOrder out ) ->
                    compare a b == out

                ( TInt a, TFloat b, TOrder out ) ->
                    compare (toFloat a) b == out

                ( TFloat a, TInt b, TOrder out ) ->
                    compare a (toFloat b) == out

                ( TFloat a, TFloat b, TOrder out ) ->
                    compare a b == out

                ( TString a, TString b, TOrder out ) ->
                    compare a b == out

                ( TChar a, TChar b, TOrder out ) ->
                    compare a b == out

                --( TTuple a, TTuple b, TOrder out ) ->
                --    compare a b == out
                --( TList a, TList b, TOrder out ) ->
                --    compare a b == out
                _ ->
                    False
    }


and : Function2
and =
    { name = "(&&)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TBool a, TBool b, TBool out ) ->
                    a && b == out

                _ ->
                    False
    }


or : Function2
or =
    { name = "(||)"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TBool a, TBool b, TBool out ) ->
                    a || b == out

                _ ->
                    False
    }


xor_ : Function2
xor_ =
    { name = "xor"
    , checkFn =
        \a b output ->
            case ( a, b, output ) of
                ( TBool a, TBool b, TBool out ) ->
                    xor a b == out

                _ ->
                    False
    }
