module Calc exposing ( calc )

{-| An example parser that computes arithmetic expressions.

@docs calc
-}

import Combine exposing (..)
import Combine.Num exposing (int)

addop : Parser (Int -> Int -> Int)
addop = choice [ (+) <$ string "+"
               , (-) <$ string "-"
               ]

mulop : Parser (Int -> Int -> Int)
mulop = choice [ (*)  <$ string "*"
               , (//) <$ string "/"
               ]

expr : Parser Int
expr =
  let
    go () =
      term |> chainl addop
  in
    rec go

term : Parser Int
term =
  let
    go () =
      factor |> chainl mulop
  in
    rec go

factor : Parser Int
factor =
  whitespace *> (parens expr <|> int) <* whitespace

{-| Compute the result of an expression. -}
calc : String -> Result String Int
calc s =
  case parse (expr <* end) s of
    (Ok n, _) ->
      Ok n

    (Err ms, cx) ->
      Err ("parse error: " ++ (toString ms) ++ ", " ++ (toString cx))
