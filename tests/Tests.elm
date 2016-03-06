module Tests where

import ElmTest exposing (..)

import Calc exposing (calc)
import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)


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


manyTillSuite : Test
manyTillSuite =
  suite
    "manyTill tests"
    [ test "Example"
        <| assertEqual
             (parse (string "<!--" *> manyTill anyChar (string "-->")) "<!-- test -->")
             (Ok [' ', 't', 'e', 's', 't', ' '], { input = "", position = 13 })
    , test "Backtracking"
        <| assertEqual
             (parse (manyTill anyChar ((many space) *> eol)) "a b c\n")
             (Ok [ 'a', ' ', 'b', ' ', 'c' ], { input = "", position = 6 })
    , test "Backtracking 2"
        <| assertEqual
             (parse (manyTill anyChar ((many space) *> eol)) "a b c  \n")
             (Ok [ 'a', ' ', 'b', ' ', 'c' ], { input = "", position = 8 })
    ]


sepEndBySuite : Test
sepEndBySuite =
  suite
    "sepEndBy tests"
    [ test "sepEndBy 1"
        <| assertEqual
             (parse (sepEndBy (string ",") (string "a")) "b")
             (Ok [], { input = "b", position = 0 })
    , test "sepEndBy 2"
        <| assertEqual
             (parse (sepEndBy (string ",") (string "a")) "a,a,a")
             (Ok ["a", "a", "a"], { input = "", position = 5 })
    , test "sepEndBy 3"
        <| assertEqual
             (parse (sepEndBy (string ",") (string "a")) "a,a,a,")
             (Ok ["a", "a", "a"], { input = "", position = 6 })
    , test "sepEndBy 4"
        <| assertEqual
             (parse (sepEndBy (string ",") (string "a")) "a,a,b")
             (Ok ["a", "a"], { input = "b", position = 4 })
    ]


sepEndBy1Suite : Test
sepEndBy1Suite =
  suite
    "sepEndBy1 tests"
    [ test "sepEndBy1 1"
        <| assertEqual
             (parse (sepEndBy1 (string ",") (string "a")) "b")
             (Err (["expected \"a\""]),{ input = "b", position = 0 })
    , test "sepEndBy1 2"
        <| assertEqual
             (parse (sepEndBy1 (string ",") (string "a")) "a,a,a")
             (Ok ["a", "a", "a"], { input = "", position = 5 })
    , test "sepEndBy1 3"
        <| assertEqual
             (parse (sepEndBy1 (string ",") (string "a")) "a,a,a,")
             (Ok ["a", "a", "a"], { input = "", position = 6 })
    , test "sepEndBy1 4"
        <| assertEqual
             (parse (sepEndBy1 (string ",") (string "a")) "a,a,b")
             (Ok ["a", "a"], { input = "b", position = 4 })
    ]


sequenceSuite : Test
sequenceSuite =
  suite
    "sequence tests"
    [ test "empty sequence"
        <| assertEqual
             (parse (sequence []) "a")
             (Ok [], { input = "a", position = 0 })
    , test "one parser"
        <| assertEqual
             (parse (sequence [many <| string "a"]) "aaaab")
             (Ok [["a", "a", "a", "a"]], { input = "b", position = 5 })
    , test "many parsers"
        <| assertEqual
             (parse (sequence [string "a", string "b", string "c"]) "abc")
             (Ok ["a", "b", "c"], { input = "", position = 3 })
    , test "many parsers failure"
        <| assertEqual
             (parse (sequence [string "a", string "b", string "c"]) "abd")
             (Err ["expected \"c\""], { input = "d", position = 2 })
    ]


all : Test
all =
  suite
    "Combine test suite"
    [ calcSuite
    , manyTillSuite
    , sepEndBySuite
    , sepEndBy1Suite
    ]
