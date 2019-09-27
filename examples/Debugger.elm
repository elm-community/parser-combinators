module Debugger exposing (runDebug)

{-| An example parser defines a simple debugger, so that you are aware of the
current rule and location information ... state can be added accordingly...

To run this example, simply enter the examples and:

1.  run `elm repl`
2.  type in `import Debugger exposing (runDebug)`
3.  try out some expressions like `runDebug "1234\nabcdef\nABCD"`

@docs calc

-}

import Combine exposing (..)


debug : String -> Parser s a -> Parser s a
debug log p =
    withLine
        (\x ->
            withColumn
                (\y ->
                    withSourceLine
                        (\s ->
                            let
                                output =
                                    Debug.log log ( x, y, s )
                            in
                            p
                        )
                )
        )


runDebug : String -> Result String String
runDebug s =
    case
        parse
            ((regex "(.|\n)"
                |> debug "regex"
             )
                |> many
            )
            s
    of
        Ok ( _, _, n ) ->
            Ok <| String.concat n

        Err ( _, stream, ms ) ->
            Err
                ("parse error: "
                    ++ "[ \""
                    ++ (ms |> List.intersperse "\", \"" |> String.concat)
                    ++ "\" ] , "
                    ++ "{ data: "
                    ++ stream.data
                    ++ ", input: "
                    ++ stream.input
                    ++ ", position: "
                    ++ String.fromInt stream.position
                    ++ " }"
                )
