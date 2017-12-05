module CurrentLocationTests exposing (..)

import Combine exposing (..)
import Combine.Char
import Expect
import Fuzz
import Test exposing (..)


entryPoint : Test
entryPoint =
    describe "entry point"
        [ test "column should be zero with empty input" <|
            \() ->
                { data = "", input = "", position = 0 }
                    |> Combine.currentColumn
                    |> Expect.equal 0
        , test "column should be zero with two lines" <|
            \() ->
                { data = "\n", input = "\n", position = 0 }
                    |> Combine.currentColumn
                    |> Expect.equal 0
        , fuzz Fuzz.string "column should be zero" <|
            \s ->
                { data = s, input = s, position = 0 }
                    |> Combine.currentColumn
                    |> Expect.equal 0
        , fuzz Fuzz.string "line should be zero" <|
            \s ->
                { data = s, input = s, position = 0 }
                    |> Combine.currentLine
                    |> Expect.equal 0
        ]


specificLocationTests : Test
specificLocationTests =
    describe "specific locations"
        [ test "should not skip to next line on eol" <|
            \() ->
                { data = "x\ny", input = "x\ny", position = 1 }
                    |> Combine.currentLocation
                    |> Expect.equal { source = "x", line = 0, column = 1 }
        , test "should skip to next line on eol + 1" <|
            \() ->
                { data = "x\ny", input = "x\ny", position = 2 }
                    |> Combine.currentLocation
                    |> Expect.equal { source = "y", line = 1, column = 0 }
        ]


noNegativeValuesForColumn : Test
noNegativeValuesForColumn =
    fuzz2 Fuzz.string Fuzz.int "withColumn should never return a value below zero" <|
        \s i ->
            let
                c =
                    if String.length s == 0 then
                        0
                    else
                        (i % String.length s)
            in
                case
                    Combine.parse
                        (Combine.count c Combine.Char.anyChar
                            *> (Combine.withColumn Combine.succeed)
                        )
                        s
                of
                    Err _ ->
                        Expect.fail "Should always parse"

                    Ok ( _, _, v ) ->
                        Expect.greaterThan -1 v


noNegativeValuesForLine : Test
noNegativeValuesForLine =
    fuzz2 Fuzz.string Fuzz.int "withLine should never return a value below zero" <|
        \s i ->
            let
                c =
                    if String.length s == 0 then
                        0
                    else
                        (i % String.length s)
            in
                case
                    Combine.parse
                        (Combine.count c Combine.Char.anyChar
                            *> (Combine.withLine Combine.succeed)
                        )
                        s
                of
                    Err _ ->
                        Expect.fail "Should always parse"

                    Ok ( _, _, v ) ->
                        Expect.greaterThan -1 v
