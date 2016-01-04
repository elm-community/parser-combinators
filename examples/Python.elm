module Python where

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Infix exposing (..)
import Combine.Num
import String

type E
  = EBool Bool
  | EInt Int
  | EFloat Float
  | EString String
  | EOr E E
  | EAnd E E
  | ENot E
  | ECmp String E E
  | EAdd E E
  | ESub E E
  | EMul E E
  | EDiv E E
  | EList (List E)
  | ETuple (List E)
  | EDict (List (E, E))
  | ESet (List E)
  | EIdentifier String
  | EAttribute E E
  | EApp E (List E)
  | EAssign E E

type S
  = SExpr E
  | SPrint (List E)
  | SDel (List E)
  | SPass
  | SBreak
  | SContinue
  | SReturn (Maybe E)
  | SRaise (Maybe (List E))
  | SImport (List (E, Maybe E))
  | SImportFrom E (List (E, Maybe E))
  | SGlobal (List E)
  | SAssert E (Maybe E)
  | SAssign E

type C
  = CSimple (List S)
  | CWhile E (List C)
  | CFor E E (List C)
  | CWith E (Maybe E) (List C)
  | CFunc E (List E) (List C)

type alias Ctx = List Int

dropWhile : (a -> Bool) -> List a -> List a
dropWhile p xs =
  case xs of
    []     -> []
    x::xs' ->
     if p x
     then dropWhile p xs'
     else xs

comment : Parser String
comment = regex "#[^\n]*"

spaces : Parser String
spaces = regex " *"

whitespace : Parser String
whitespace = comment <|> spaces <?> "whitespace"

ws : Parser res -> Parser res
ws = between whitespace whitespace

keyword : String -> Parser String
keyword s = string s <* spaces

bool : Parser E
bool = EBool <$> choice [ False <$ string "False"
                        , True  <$ string "True"
                        ] <?> "boolean"

int : Parser E
int = EInt <$> Combine.Num.int <?> "integer"

float : Parser E
float = EFloat <$> Combine.Num.float <?> "float"

str : Parser E
str = EString <$> choice [ string "'" *> regex "(\\\\'|[^'\n])*" <* string "'"
                         , string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\""
                         ] <?> "string"

identifier : Parser E
identifier = EIdentifier <$> regex "[_a-zA-Z][_a-zA-Z0-9]*" <?> "identifier"

attribute : Parser E
attribute =
  rec <| \() ->
    EAttribute
      <$> identifier <* string "."
      <*> choice [ attribute, identifier ]
      <?> "attribute"

app : Parser E
app =
  rec <| \() -> EApp <$> choice [ attribute, identifier ] <*> parens exprList

commaSep : Parser String
commaSep = regex ", *"

dictSep : Parser String
dictSep = regex ":[ \t\r\n]*"

listSep : Parser String
listSep = regex ",[ \t\r\n]*"

list : Parser E
list =
  rec <| \() ->
      EList <$> brackets (sepBy listSep expr) <?> "list"

tuple : Parser E
tuple =
  rec <| \() ->
      ETuple <$> parens (sepBy listSep expr) <?> "tuple"

dict : Parser E
dict =
  rec <| \() ->
      EDict <$> brackets (sepBy listSep ((,) <$> expr <* dictSep <*> expr))
            <?> "dictionary"

set : Parser E
set =
  rec <| \() ->
      ESet <$> brackets (sepBy listSep expr) <?> "set"

atom : Parser E
atom =
  rec <| \() ->
      choice [ bool, float, int, str, attribute, identifier, list, tuple, dict, set ]

expr : Parser E
expr =
  rec <| \() ->
      andExpr `chainl` orop

andExpr : Parser E
andExpr =
  rec <| \() ->
      notExpr `chainl` andop

notExpr : Parser E
notExpr =
  rec <| \() ->
        (ws <| ENot <$> (string "not" *> notExpr)) `or` cmpExpr

cmpExpr : Parser E
cmpExpr =
  rec <| \() ->
      arithExpr `chainl` cmpop

arithExpr : Parser E
arithExpr =
  rec <| \() ->
      term `chainl` addop

term : Parser E
term =
  rec <| \() ->
      factor `chainl` mulop

factor : Parser E
factor =
  rec <| \() ->
      ws (parens expr <|> app <|> atom)

orop : Parser (E -> E -> E)
orop = EOr <$ string "or"

andop : Parser (E -> E -> E)
andop = EAnd <$ string "and"

cmpop : Parser (E -> E -> E)
cmpop = ECmp <$> choice [ string "<",  string ">"
                        , string "==", string "!="
                        , string ">=", string "<="
                        , string "in", (++) <$> keyword "not" <*> string "in"
                        , string "is", (++) <$> keyword "is" <*> string "not"
                        ]

addop : Parser (E -> E -> E)
addop = choice [ EAdd <$ string "+"
               , ESub <$ string "-"
               ]

mulop : Parser (E -> E -> E)
mulop = choice [ EMul <$ string "*"
               , EDiv <$ string "/"
               ]

exprList : Parser (List E)
exprList = sepBy commaSep expr

exprStmt : Parser S
exprStmt = SExpr <$> expr <?> "expression"

printStmt : Parser S
printStmt = SPrint <$> (keyword "print" *> exprList) <?> "print statement"

delStmt : Parser S
delStmt = SDel <$> (keyword "del" *> exprList) <?> "del statement"

passStmt : Parser S
passStmt = SPass <$ keyword "pass" <?> "pass statement"

breakStmt : Parser S
breakStmt = SBreak <$ keyword "break" <?> "break statement"

continueStmt : Parser S
continueStmt = SContinue <$ keyword "continue" <?> "continue statement"

returnStmt : Parser S
returnStmt = SReturn <$> (keyword "return" *> maybe expr) <?> "return statement"

raiseStmt : Parser S
raiseStmt = SRaise <$> (keyword "raise" *> maybe exprList) <?> "raise statement"

importAs : Parser (List (E, Maybe E))
importAs =
  sepBy commaSep <| (,)
    <$> choice [ attribute, identifier ]
    <*> maybe (whitespace *> keyword "as" *> identifier)

importStmt : Parser S
importStmt =
  SImport <$> (keyword "import" *> importAs)
          <?> "import statement"

importFromStmt : Parser S
importFromStmt =
  SImportFrom <$> (keyword "from" *> choice [ attribute, identifier ] <* spaces)
              <*> (keyword "import" *> importAs)
              <?> "from statement"

globalStmt : Parser S
globalStmt = SGlobal <$> (keyword "global" *> sepBy commaSep identifier)

assertStmt : Parser S
assertStmt =
  SAssert <$> (keyword "assert" *> expr)
          <*> maybe (commaSep *> expr)

assignop : Parser (E -> E -> E)
assignop = EAssign <$ ws (string "=")

assignStmt : Parser S
assignStmt = SAssign <$> (expr `chainr` assignop)

indentation : Ctx -> Parser res -> Parser (Ctx, res)
indentation cx p =
  let
    level = Maybe.withDefault 0 (List.head cx)
    indentationp =
      skip (count level (string " ")) <?> "expected indentation"
  in
    indentationp *> map ((,) cx) p

indent : Ctx -> Parser (Ctx, ())
indent cx =
  primitive <| \pcx ->
    case Combine.app spaces pcx of
      (Ok s, _) ->
        let level = String.length s in
        case cx of
          [] ->
            (Err ["negative indentation"], pcx)

          x::_ ->
            if level > x
            then (Ok (level::cx, ()), pcx)
            else (Err ["expected indentation"], pcx)

      (Err ms, pcx) ->
        (Err ms, pcx)

dedent : Ctx -> Parser (Ctx, ())
dedent cx =
  primitive <| \pcx ->
    case Combine.app spaces pcx of
      (Ok s, _) ->
        let cx' = dropWhile ((/=) (String.length s)) cx in
        case cx' of
          _::_ ->
            (Ok (cx', ()), pcx)

          _ ->
            (Err ["unindent does not match any outer indentation level"], pcx)

      (Err ms, pcx) ->
        (Err ms, pcx)

block : Ctx -> Parser (Ctx, List C)
block cx =
  string ":" *> whitespace *> eol *> indent cx
    `andThen` \(cx', _) -> many1 (stmt cx')
    `andThen` \ss -> dedent cx'
    `andThen` \(cx'', _) -> succeed (cx'', List.map snd ss)

blockStmt : Ctx -> Parser (List C -> C) -> Parser (Ctx, C)
blockStmt cx p =
  indentation cx p
    `andThen` \(_, f) -> block cx
    `andThen` \(cx, ss) -> succeed (cx, f ss)

simpleStmt : Ctx -> Parser (Ctx, C)
simpleStmt cx =
  let
    stmt = choice [ assertStmt, globalStmt, importFromStmt, importStmt
                  , raiseStmt, returnStmt, continueStmt, breakStmt
                  , passStmt, delStmt, printStmt, assignStmt, exprStmt
                  ]
  in
    indentation cx (CSimple <$> sepBy (string ";" <* whitespace) stmt <* eol)

whileStmt : Parser (List C -> C)
whileStmt =
  CWhile <$> (keyword "while" *> expr)

forStmt : Parser (List C -> C)
forStmt =
  CFor <$> (keyword "for" *> identifier)
       <*> (spaces *> keyword "in" *> expr)

withStmt : Parser (List C -> C)
withStmt =
  CWith
    <$> (keyword "with" *> expr)
    <*> maybe (keyword "as" *> identifier)

funcStmt : Parser (List C -> C)
funcStmt =
  CFunc
    <$> (keyword "def" *> identifier)
    <*> parens (sepBy commaSep identifier)

compoundStmt : Ctx -> Parser (Ctx, C)
compoundStmt cx =
  let
    parsers = [ whileStmt, forStmt, withStmt, funcStmt ]
  in choice <| List.map (\p -> blockStmt cx p) parsers

stmt : Ctx -> Parser (Ctx, C)
stmt cx = compoundStmt cx `or` simpleStmt cx

program : Parser (List C)
program =
  let
    all acc cx pcx =
      if pcx.input == ""
      then (Ok (List.reverse acc), pcx)
      else
        case Combine.app (stmt cx) pcx of
          (Ok (cx', res'), pcx') ->
            all (res' :: acc) cx' pcx'

          (Err ms, pcx') ->
            (Err ms, pcx')
  in
    primitive <| all [] [0]


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

parse : String -> Result String (List C)
parse s =
  case Combine.parse program (s ++ "\n") of
    (Ok es, _) ->
      Ok es

    (Err ms, cx) ->
      Err <| formatError s ms cx

test : Result String (List C)
test =
  parse """import os

a = b = 1

def rel(p):
  return os.path.join(os.path.dirname(__file__), p)

def f(a, b):
  return a + b

with open(rel('Python.elm')) as f:
  for line in f:
    print f
"""
