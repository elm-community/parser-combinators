module Calc (calc) where

{-| An example parser that computes arithmetic expressions.

@docs calc
-}

import Combine exposing (..)
import Combine.Infix exposing (..)
import Combine.Num exposing (int)

ws : Parser String
ws = regex "[ \t\r\n]*"

addop : Parser (Int -> Int -> Int)
addop = choice [ (+) <$ string "+", (-) <$ string "-" ]

mulop : Parser (Int -> Int -> Int)
mulop = choice [ (*) <$ string "*", (//) <$ string "/" ]

expr : Parser Int
expr = rec <| \() -> term `chainl` addop

term : Parser Int
term = rec <| \() -> factor `chainl` mulop

factor : Parser Int
factor = rec <| \() -> between ws ws (parens expr <|> int)

{-| Compute the result of an expression. -}
calc : String -> Result String Int
calc s =
  case parse (expr <* end) s of
    (Ok n, _) ->
      Ok n

    (Err ms, cx) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))
