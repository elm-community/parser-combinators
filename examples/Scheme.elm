module Scheme where

import Combine exposing (..)
import String

type E
  = EInt Int
  | EFloat Float
  | EString String
  | EName String
  | EApp E (List E)
  | EQuote (List E)

undefined : a
undefined = undefined

forcing : (String -> Result.Result x res) -> String -> res
forcing f s =
  case f s of
    Ok res ->
      res

    Err _ ->
      undefined

toInt : String -> Int
toInt = forcing String.toInt

toFloat : String -> Float
toFloat = forcing String.toFloat

whitespace : Parser String
whitespace = regex "[ \t\r\n]*"

sign : Parser Int
sign = optional 1 (choice [  1 <$ string "+"
                          , -1 <$ string "-" ])

int : Parser E
int = (EInt << toInt) <$> regex "[1-9][0-9]*"

float : Parser E
float = (EFloat << toFloat) <$> regex "(0|[1-9][0-9]*)(\\.[0-9]+)"

num : Parser E
num =
  sign
    `andThen` \x -> float `or` int
    `andThen` \n ->
      case n of
        EInt n ->
          succeed (EInt (x * n))

        EFloat n ->
          succeed (EFloat ((Basics.toFloat x) * n))

str : Parser E
str = EString <$> regex "\"(\\\"|[^\"])+\""

name : Parser E
name = EName <$> regex "[a-zA-Z-_+][a-zA-Z0-9-_+]*"

app : Parser E
app = char '(' *> (EApp <$> expr `andMap` many expr) <* char ')'

quote : Parser E
quote = EQuote <$> (string "'(" *> many expr <* char ')')

expr : Parser E
expr = rec (\() -> whitespace *> choice [num, str, name, app, quote])

parse : String -> Result.Result String E
parse s =
  case Combine.parse (expr <* end) s of
    (Done e, _) ->
      Ok e

    (Fail ms, c) ->
      Err (toString ms)
