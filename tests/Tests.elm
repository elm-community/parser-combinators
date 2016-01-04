module Tests where

import Calc exposing (calc)
import ElmTest exposing (..)
import String


calcSuite : Test
calcSuite =
  let
    equiv s x = calc s `assertEqual` (Ok x)
  in
    suite
      "Calc example tests"
      [ test "Atoms" (equiv "1" 1)
      , test "Atoms 2" (equiv "-1" -1)
      , test "Parenthesized atoms" (equiv "(1)" 1)
      , test "Addition" (equiv "1 + 1" 2)
      , test "Subtraction" (equiv "1 - 1" 0)
      , test "Multiplication" (equiv "1 * 1" 1)
      , test "Division" (equiv "1 / 1" 1)
      , test "Precedence 1" (equiv "1 + 2 * 3" 7)
      , test "Precedence 2" (equiv "1 + 2 * 3 * 2" 13)
      , test "Parenthesized precedence" (equiv "(1 + 2) * 3 * 2" 18)
      ]


all : Test
all =
  suite
    "Combine test suite"
    [ calcSuite
    ]
