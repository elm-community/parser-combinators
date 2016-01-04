module Combine.Num ( sign, digit, int, float ) where

{-| This module contains Parsers specific to parsing numbers.

# Parsers
@docs sign, digit, int, float
-}

import Char
import Combine exposing (..)
import Combine.Char
import Combine.Infix exposing (..)
import Debug exposing (crash)
import String


unwrap : (String -> Result x res) -> String -> res
unwrap f s =
  case f s of
    Ok res ->
      res

    Err m ->
      crash ("impossible state in Combine.Num.unwrap: " ++ (toString m))


toInt : String -> Int
toInt = unwrap String.toInt


toFloat : String -> Float
toFloat = unwrap String.toFloat


{-| Parse a numeric sign, returning `1` for positive numbers and `-1`
for negative numbers. -}
sign : Parser Int
sign = optional 1 (choice [  1 <$ string "+"
                          , -1 <$ string "-" ])


{-| Parse a digit. -}
digit : Parser Int
digit =
  let
    toDigit c = Char.toCode c - Char.toCode '0'
  in
    toDigit <$> Combine.Char.digit <?> "expected a digit"


{-| Parse an integer. -}
int : Parser Int
int =
  (*)
    `map` sign
    `andMap` (toInt <$> regex "(0|[1-9][0-9]*)") <?> "expected an integer"


{-| Parse a float. -}
float : Parser Float
float =
  ((*) << Basics.toFloat)
    `map` sign
    `andMap` (toFloat <$> regex "(0|[1-9][0-9]*)(\\.[0-9]+)") <?> "expected a float"
