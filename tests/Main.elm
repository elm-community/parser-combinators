port module Main exposing (..)

import Json.Encode exposing (Value)
import Parsers
import Test exposing (..)
import Test.Runner.Node exposing (run)

main =
    run emit all

port emit : ( String, Value ) -> Cmd msg

all : Test
all =
   describe "Combine suite" [ Parsers.all
                            ]
