module Scheme where

import Combine exposing (..)
import Combine.Infix exposing (..)
import String

type E
  = EBool Bool
  | EInt Int
  | EFloat Float
  | EChar Char
  | EString String
  | EIdentifier String
  | EList (List E)
  | EVector (List E)
  | EQuote E
  | EQuasiquote E
  | EUnquote E
  | EUnquoteSplice E
  | EComment String

undefined : a
undefined = undefined

unwrap : (String -> Result.Result x res) -> String -> res
unwrap f s =
  case f s of
    Ok res ->
      res

    Err _ ->
      undefined

toInt : String -> Int
toInt = unwrap String.toInt

toFloat : String -> Float
toFloat = unwrap String.toFloat

toChar : String -> Char
toChar s =
  case String.uncons s of
    Just (c, _) ->
      c

    Nothing ->
      undefined

whitespace : Parser String
whitespace = regex "[ \t\r\n]*"

comment : Parser E
comment = EComment <$> regex ";[^\n]+"

bool : Parser E
bool = EBool <$> choice [ True  <$ string "#t"
                        , False <$ string "#f" ]

sign : Parser Int
sign = optional 1 (choice [  1 <$ string "+"
                          , -1 <$ string "-" ])

int : Parser E
int = (EInt << toInt) <$> regex "(0|[1-9][0-9]*)"

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

char : Parser E
char = EChar <$> (string "#\\" *> choice [ ' '  <$ string "space"
                                         , '\n' <$ string "newline"
                                         , toChar <$> regex "." ])

str : Parser E
str = EString <$> regex "\"(\\\"|[^\"])+\""

identifier : Parser E
identifier =
  let
    letter = "a-zA-Z"
    specialInitial = "!$%&*/:<=>?^_~+\\-"
    initial = letter ++ specialInitial
    initialRe = "[" ++ initial ++ "]"

    digit = "0-9"
    specialSubsequent = ".@+\\-"
    subsequent = initial ++ digit ++ specialSubsequent
    subsequentRe = "[" ++ subsequent ++ "]*"

    identifierRe = initialRe ++ subsequentRe
  in
  EIdentifier <$> regex identifierRe

list : Parser E
list = EList <$> (string "(" *> many expr <* string ")")

vector : Parser E
vector = EVector <$> (string "#(" *> many expr <* string ")")

quote : Parser E
quote = EQuote <$> (string "'" *> expr)

quasiquote : Parser E
quasiquote = EQuasiquote <$> (string "`" *> expr)

unquote : Parser E
unquote = EUnquote <$> (string "," *> expr)

unquoteSplice : Parser E
unquoteSplice = EUnquoteSplice <$> (string ",@" *> expr)

expr : Parser E
expr =
  rec <| \() ->
    let parsers = [ bool, num , char, str, identifier, list, vector
                  , quote, quasiquote, unquote, unquoteSplice, comment ]
    in whitespace *> choice parsers <* whitespace

parse : String -> Result.Result String (List E)
parse s =
  case Combine.parse (many expr <* end) s of
    (Done e, _) ->
      Ok e

    (Fail ms, c) ->
      Err ("Parse error on input: " ++ (toString c.input) ++ ". Errors: " ++ (toString ms))
