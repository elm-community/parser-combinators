port module Main exposing (..)

import Json.Encode exposing (Value)
import Parsers
import Test exposing (..)
import Test.Runner.Node exposing (TestProgram, run)


main : TestProgram
main =
    run emit all


all : Test
all =
    describe "Combine suite" [ Parsers.all ]


port emit : ( String, Value ) -> Cmd msg
