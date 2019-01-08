module Combine.Num exposing (sign, digit, int, float)

{-| This module contains Parsers specific to parsing numbers.


# Parsers

@docs sign, digit, int, float

-}

import Char
import Combine exposing (..)
import Combine.Char
import String



{-
   unwrap : (String -> Maybe res) -> String -> res
   unwrap f s =
       case f s of
           Just res ->
               onsuccess  res

           Nothing ->
               onerror "impossible state in Combine.Num.unwrap: "

-}


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
int : Parser s (Maybe Int)
int =
    regex "-?(?:0|[1-9]\\d*)"
        |> map String.toInt



--|> onerror "expected an integer"


{-| Parse a float.
-}
float : Parser s (Maybe Float)
float =
    regex "-?(?:0|[1-9]\\d*)\\.\\d+"
        |> map String.toFloat
