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
whitespace = regex "[ \t\r\n]*" <?> "whitespace"

comment : Parser E
comment =
  EComment
    <$> regex ";[^\n]+"
    <?> "comment"

bool : Parser E
bool =
  let
    boolLiteral = choice [ True  <$ string "#t"
                         , False <$ string "#f"
                         ]
  in
  EBool
    <$> boolLiteral
    <?> "boolean literal"

int : Parser E
int =
  EInt
    <$> Combine.Num.int
    <?> "integer literal"

float : Parser E
float =
  EFloat
    <$> Combine.Num.float
    <?> "float literal"

char : Parser E
char =
  let
    charLiteral =
      string "#\\" *> choice [ ' '  <$ string "space"
                             , '\n' <$ string "newline"
                             , anyChar
                             ]
  in
  EChar
    <$> charLiteral
    <?> "character literal"

str : Parser E
str =
  EString
    <$> regex "\"(\\\"|[^\"])+\""
    <?> "string literal"

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
  EIdentifier <$> regex identifierRe <?> "identifier"

list : Parser E
list =
  EList
    <$> parens (many expr)
    <?> "list"

vector : Parser E
vector =
  EVector
    <$> (string "#(" *> many expr <* string ")")
    <?> "vector"

quote : Parser E
quote =
  EQuote
    <$> (string "'" *> expr)
    <?> "quoted expression"

quasiquote : Parser E
quasiquote =
  EQuasiquote
    <$> (string "`" *> expr)
    <?> "quasiquoted expression"

unquote : Parser E
unquote =
  EUnquote
    <$> (string "," *> expr)
    <?> "unquoted expression"

unquoteSplice : Parser E
unquoteSplice =
  EUnquoteSplice
    <$> (string ",@" *> expr)
    <?> "spliced expression"

expr : Parser E
expr =
  rec <| \() ->
    let parsers = [ bool, float, int, char, str, identifier, list, vector
                  , quote, quasiquote, unquote, unquoteSplice, comment ]
    in whitespace *> choice parsers <* whitespace

program : Parser (List E)
program =
  let
    all acc cx =
      if cx.input == ""
      then (Ok (List.reverse acc), cx)
      else
        case app expr cx of
          (Ok res', cx') ->
            all (res' :: acc) cx'

          (Err ms, cx') ->
            (Err ms, cx')
  in
    primitive <| all []

formatError : String -> List String -> Context -> String
formatError input ms cx =
  let
    lines = String.lines input
    lineCount = List.length lines
    (line, lineNumber, lineOffset, _) =
      List.foldl
            (\line (line', n, o, pos) ->
               if pos < 0
               then (line', n, o, pos)
               else (line, n + 1, pos, pos - 1 - String.length line'))
            ("", 0, 0, cx.position) lines

    separator = "|> "
    expectationSeparator = "\n  * "
    lineNumberOffset = floor (logBase 10 lineNumber) + 1
    separatorOffset = String.length separator
    padding = lineNumberOffset + separatorOffset + lineOffset + 1
  in
  "Parse error around line:\n\n"
    ++ (toString lineNumber) ++ separator ++ line ++ "\n"
    ++ String.padLeft padding ' ' "^"
    ++ "\nI expected one of the following:\n"
    ++ expectationSeparator
    ++ String.join expectationSeparator ms

parse : String -> Result String (List E)
parse s =
  case Combine.parse program s of
    (Ok e, _) ->
      Ok e

    (Err ms, cx) ->
      Err <| formatError s ms cx
