module Scheme where

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num
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
int = EInt <$> Combine.Num.int

float : Parser E
float = EFloat <$> Combine.Num.float

num : Parser E
num = float <|> int

char : Parser E
char = EChar <$> (string "#\\" *> choice [ ' '  <$ string "space"
                                         , '\n' <$ string "newline"
                                         , anyChar ])

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
list = EList <$> parens (many expr)

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
