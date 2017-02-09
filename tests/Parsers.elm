module Parsers exposing (all)

import Calc exposing (calc)
import Combine exposing (..)
import Combine.Char exposing (..)
import Expect
import String
import Test exposing (Test, describe, test)


successful : String -> Parser () a -> String -> a -> Test
successful desc p s r =
    test desc <|
        \() ->
            case parse p s of
                Ok ( _, _, res ) ->
                    Expect.equal res r

                Err ( _, _, ms ) ->
                    Expect.fail <| String.join ", " ms


calcSuite : Test
calcSuite =
    let
        equiv s x () =
            Expect.equal (calc s) (Ok x)
    in
        describe "calc example tests"
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
    let
        comment =
            string "<!--" *> manyTill anyChar (string "-->")

        line =
            manyTill anyChar ((many space) *> eol)
    in
        describe "manyTill tests"
            [ successful "Example" comment "<!-- test -->" [ ' ', 't', 'e', 's', 't', ' ' ]
            , successful "Backtracking" line "a b c\n" [ 'a', ' ', 'b', ' ', 'c' ]
            , successful "Backtracking 2" line "a b c  \n" [ 'a', ' ', 'b', ' ', 'c' ]
            ]


sepEndBySuite : Test
sepEndBySuite =
    let
        commaSep =
            sepEndBy (string ",") (string "a")
    in
        describe "sepEndBy tests"
            [ successful "sepEndBy 1" commaSep "b" []
            , successful "sepEndBy 2" commaSep "a,a,a" [ "a", "a", "a" ]
            , successful "sepEndBy 3" commaSep "a,a,a," [ "a", "a", "a" ]
            , successful "sepEndBy 4" commaSep "a,a,b" [ "a", "a" ]
            ]


sepEndBy1Suite : Test
sepEndBy1Suite =
    let
        commaSep =
            sepEndBy1 (string ",") (string "a")
    in
        describe "sepEndBy1 tests"
            [ test "sepEndBy1 1" <|
                \() ->
                    Expect.equal
                        (parse commaSep "a,a,a")
                        (Ok ( (), { data = "a,a,a", input = "", position = 5 }, [ "a", "a", "a" ] ))
            , test "sepEndBy1 2" <|
                \() ->
                    Expect.equal
                        (parse commaSep "b")
                        (Err ( (), { data = "b", input = "b", position = 0 }, [ "expected \"a\"" ] ))
            , test "sepEndBy1 3" <|
                \() ->
                    Expect.equal
                        (parse commaSep "a,a,a,")
                        (Ok ( (), { data = "a,a,a,", input = "", position = 6 }, [ "a", "a", "a" ] ))
            , test "sepEndBy1 4" <|
                \() ->
                    Expect.equal
                        (parse commaSep "a,a,b")
                        (Ok ( (), { data = "a,a,b", input = "b", position = 4 }, [ "a", "a" ] ))
            ]


sequenceSuite : Test
sequenceSuite =
    describe "sequence tests"
        [ test "empty sequence" <|
            \() ->
                Expect.equal
                    (parse (sequence []) "a")
                    (Ok ( (), { data = "a", input = "a", position = 0 }, [] ))
        , test "one parser" <|
            \() ->
                Expect.equal
                    (parse (sequence [ many <| string "a" ]) "aaaab")
                    (Ok ( (), { data = "aaaab", input = "b", position = 4 }, [ [ "a", "a", "a", "a" ] ] ))
        , test "many parsers" <|
            \() ->
                Expect.equal
                    (parse (sequence [ string "a", string "b", string "c" ]) "abc")
                    (Ok ( (), { data = "abc", input = "", position = 3 }, [ "a", "b", "c" ] ))
        , test "many parsers failure" <|
            \() ->
                Expect.equal
                    (parse (sequence [ string "a", string "b", string "c" ]) "abd")
                    (Err ( (), { data = "abd", input = "d", position = 2 }, [ "expected \"c\"" ] ))
        ]


all : Test
all =
    describe "parsers suite"
        [ calcSuite
        , manyTillSuite
        , sepEndBySuite
        , sepEndBy1Suite
        , sequenceSuite
        ]
