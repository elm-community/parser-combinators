module Combine.Num exposing (sign, digit, int, float)

{-| This module contains Parsers specific to parsing numbers.


# Parsers

@docs sign, digit, int, float

-}

import Char
import Combine exposing (Parser, andThen, fail, map, onerror, onsuccess, optional, or, regex, string, succeed)
import Combine.Char
import String


{-| Parse a numeric sign, returning `1` for positive numbers and `-1`
for negative numbers.
-}
sign : Parser s Int
sign =
    optional 1
        (or
            (string "+" |> onsuccess 1)
            (string "-" |> onsuccess -1)
        )


{-| Parse a digit.
-}
digit : Parser s Int
digit =
    let
        toDigit c =
            Char.toCode c - Char.toCode '0'
    in
    map toDigit Combine.Char.digit |> onerror "expected a digit"


{-| Parse an integer.
-}
int : Parser s Int
int =
    regex "-?(?:0|[1-9]\\d*)"
        |> map String.toInt
        |> andThen unwrap
        |> onerror "expected an float"


{-| Parse a float.
-}
float : Parser s Float
float =
    regex "-?(?:0|[1-9]\\d*)\\.\\d+"
        |> map String.toFloat
        |> andThen unwrap
        |> onerror "expected an float"


unwrap : Maybe v -> Parser s v
unwrap value =
    case value of
        Just v ->
            succeed v

        Nothing ->
            fail "impossible state in Combine.Num.unwrap"
