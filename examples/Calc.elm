module Calc exposing (calc)

{-| An example parser that computes arithmetic expressions.

@docs calc

-}

import Combine exposing (..)
import Combine.Num exposing (int)


addop : Parser s (Int -> Int -> Int)
addop =
    choice
        [ string "+" |> onsuccess (+)
        , string "-" |> onsuccess (-)
        ]


mulop : Parser s (Int -> Int -> Int)
mulop =
    choice
        [ string "*" |> onsuccess (*)
        , string "/" |> onsuccess (//)
        ]


expr : Parser s Int
expr =
    let
        go () =
            chainl addop term
    in
    lazy go


term : Parser s Int
term =
    let
        go () =
            chainl mulop factor
    in
    lazy go


factor : Parser s Int
factor =
    whitespace
        |> keep (or (parens expr) int)
        |> ignore whitespace


{-| Compute the result of an expression.
-}
calc : String -> Result String Int
calc s =
    case parse (expr |> ignore end) s of
        Ok ( _, _, n ) ->
            Ok n

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
