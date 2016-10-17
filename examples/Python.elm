module Python exposing ( .. )

import Combine exposing (..)
import Combine.Char exposing (..)
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
    []    -> []
    x::ys ->
     if p x
     then dropWhile p ys
     else xs

comment : Parser s String
comment = regex "#[^\n]*"

spaces : Parser s String
spaces = regex " *"

whitespace : Parser s String
whitespace = comment <|> spaces <?> "whitespace"

ws : Parser s res -> Parser s res
ws = between whitespace whitespace

keyword : String -> Parser s String
keyword s = string s <* spaces

bool : Parser s E
bool = EBool <$> choice [ False <$ string "False"
                        , True  <$ string "True"
                        ] <?> "boolean"

int : Parser s E
int = EInt <$> Combine.Num.int <?> "integer"

float : Parser s E
float = EFloat <$> Combine.Num.float <?> "float"

str : Parser s E
str = EString <$> choice [ string "'" *> regex "(\\\\'|[^'\n])*" <* string "'"
                         , string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\""
                         ] <?> "string"

identifier : Parser s E
identifier = EIdentifier <$> regex "[_a-zA-Z][_a-zA-Z0-9]*" <?> "identifier"

attribute : Parser s E
attribute =
  rec <| \() ->
    EAttribute
      <$> identifier <* string "."
      <*> choice [ attribute, identifier ]
      <?> "attribute"

app : Parser s E
app =
  rec <| \() -> EApp <$> choice [ attribute, identifier ] <*> parens exprList

commaSep : Parser s String
commaSep = regex ", *"

dictSep : Parser s String
dictSep = regex ":[ \t\r\n]*"

listSep : Parser s String
listSep = regex ",[ \t\r\n]*"

list : Parser s E
list =
  rec <| \() ->
      EList <$> brackets (sepBy listSep expr) <?> "list"

tuple : Parser s E
tuple =
  rec <| \() ->
      ETuple <$> parens (sepBy listSep expr) <?> "tuple"

dict : Parser s E
dict =
  rec <| \() ->
      EDict <$> brackets (sepBy listSep ((,) <$> expr <* dictSep <*> expr))
            <?> "dictionary"

set : Parser s E
set =
  rec <| \() ->
      ESet <$> brackets (sepBy listSep expr) <?> "set"

atom : Parser s E
atom =
  rec <| \() ->
      choice [ bool, float, int, str, attribute, identifier, list, tuple, dict, set ]

expr : Parser s E
expr =
  rec (\() -> chainl orop andExpr)

andExpr : Parser s E
andExpr =
  rec (\() -> chainl andop notExpr)

notExpr : Parser s E
notExpr =
  rec <| \() ->
        (ws <| ENot <$> (string "not" *> notExpr)) <|> cmpExpr

cmpExpr : Parser s E
cmpExpr =
  rec (\() -> chainl cmpop arithExpr)

arithExpr : Parser s E
arithExpr =
  rec (\() -> chainl addop term)

term : Parser s E
term =
  rec (\() -> chainl mulop factor)

factor : Parser s E
factor =
  rec (\() -> ws (parens expr <|> app <|> atom))

orop : Parser s (E -> E -> E)
orop = EOr <$ string "or"

andop : Parser s (E -> E -> E)
andop = EAnd <$ string "and"

cmpop : Parser s (E -> E -> E)
cmpop = ECmp <$> choice [ string "<",  string ">"
                        , string "==", string "!="
                        , string ">=", string "<="
                        , string "in", (++) <$> keyword "not" <*> string "in"
                        , string "is", (++) <$> keyword "is" <*> string "not"
                        ]

addop : Parser s (E -> E -> E)
addop = choice [ EAdd <$ string "+"
               , ESub <$ string "-"
               ]

mulop : Parser s (E -> E -> E)
mulop = choice [ EMul <$ string "*"
               , EDiv <$ string "/"
               ]

exprList : Parser s (List E)
exprList = sepBy commaSep expr

exprStmt : Parser s S
exprStmt = SExpr <$> expr <?> "expression"

printStmt : Parser s S
printStmt = SPrint <$> (keyword "print" *> exprList) <?> "print statement"

delStmt : Parser s S
delStmt = SDel <$> (keyword "del" *> exprList) <?> "del statement"

passStmt : Parser s S
passStmt = SPass <$ keyword "pass" <?> "pass statement"

breakStmt : Parser s S
breakStmt = SBreak <$ keyword "break" <?> "break statement"

continueStmt : Parser s S
continueStmt = SContinue <$ keyword "continue" <?> "continue statement"

returnStmt : Parser s S
returnStmt = SReturn <$> (keyword "return" *> maybe expr) <?> "return statement"

raiseStmt : Parser s S
raiseStmt = SRaise <$> (keyword "raise" *> maybe exprList) <?> "raise statement"

importAs : Parser s (List (E, Maybe E))
importAs =
  sepBy commaSep <| (,)
    <$> choice [ attribute, identifier ]
    <*> maybe (whitespace *> keyword "as" *> identifier)

importStmt : Parser s S
importStmt =
  SImport <$> (keyword "import" *> importAs)
          <?> "import statement"

importFromStmt : Parser s S
importFromStmt =
  SImportFrom <$> (keyword "from" *> choice [ attribute, identifier ] <* spaces)
              <*> (keyword "import" *> importAs)
              <?> "from statement"

globalStmt : Parser s S
globalStmt = SGlobal <$> (keyword "global" *> sepBy commaSep identifier)

assertStmt : Parser s S
assertStmt =
  SAssert <$> (keyword "assert" *> expr)
          <*> maybe (commaSep *> expr)

assignop : Parser s (E -> E -> E)
assignop = EAssign <$ ws (string "=")

assignStmt : Parser s S
assignStmt = SAssign <$> (chainr assignop expr)

indentation : Ctx -> Parser s res -> Parser s (Ctx, res)
indentation cx p =
  let
    level = Maybe.withDefault 0 (List.head cx)
    indentationp =
      skip (count level (string " ")) <?> "expected indentation"
  in
    indentationp *> map ((,) cx) p

indent : Ctx -> Parser s (Ctx, ())
indent cx =
  primitive <| \state stream ->
    case Combine.app spaces state stream of
      (_, _, Ok s) ->
        let level = String.length s in
        case cx of
          [] ->
            (state, stream, Err ["negative indentation"])

          x::_ ->
            if level > x
            then (state, stream, Ok (level::cx, ()))
            else (state, stream, Err ["expected indentation"])

      (estate, estream, Err ms) ->
        (estate, estream, Err ms)

dedent : Ctx -> Parser s (Ctx, ())
dedent cx =
  primitive <| \state stream ->
    case Combine.app spaces state stream of
      (_, _, Ok s) ->
        let rcx = dropWhile ((/=) (String.length s)) cx in
        case rcx of
          _::_ ->
            (state, stream, Ok (rcx, ()))

          _ ->
            (state, stream, Err ["unindent does not match any outer indentation level"])

      (estate, estream, Err ms) ->
        (estate, estream, Err ms)

block : Ctx -> Parser s (Ctx, List C)
block cx =
  string ":" *> whitespace *> eol *> indent cx
    |> andThen (\(scx, _) -> many1 (stmt scx)
    |> andThen (\ss -> dedent scx
    |> andThen (\(rcx, _) -> succeed (rcx, List.map snd ss))))

blockStmt : Ctx -> Parser s (List C -> C) -> Parser s (Ctx, C)
blockStmt cx p =
  indentation cx p
    |> andThen (\(_, f) -> block cx
    |> andThen (\(rcx, ss) -> succeed (rcx, f ss)))

simpleStmt : Ctx -> Parser s (Ctx, C)
simpleStmt cx =
  let
    stmt = choice [ assertStmt, globalStmt, importFromStmt, importStmt
                  , raiseStmt, returnStmt, continueStmt, breakStmt
                  , passStmt, delStmt, printStmt, assignStmt, exprStmt
                  ]
  in
    indentation cx (CSimple <$> sepBy (string ";" <* whitespace) stmt <* eol)

whileStmt : Parser s (List C -> C)
whileStmt =
  CWhile <$> (keyword "while" *> expr)

forStmt : Parser s (List C -> C)
forStmt =
  CFor <$> (keyword "for" *> identifier)
       <*> (spaces *> keyword "in" *> expr)

withStmt : Parser s (List C -> C)
withStmt =
  CWith
    <$> (keyword "with" *> expr)
    <*> maybe (keyword "as" *> identifier)

funcStmt : Parser s (List C -> C)
funcStmt =
  CFunc
    <$> (keyword "def" *> identifier)
    <*> parens (sepBy commaSep identifier)

compoundStmt : Ctx -> Parser s (Ctx, C)
compoundStmt cx =
  let
    parsers = [ whileStmt, forStmt, withStmt, funcStmt ]
  in choice <| List.map (\p -> blockStmt cx p) parsers

stmt : Ctx -> Parser s (Ctx, C)
stmt cx = compoundStmt cx <|> simpleStmt cx

program : Parser s (List C)
program =
  let
    all acc cx state stream =
      if stream.input == ""
      then (state, stream, Ok (List.reverse acc))
      else
        case Combine.app (stmt cx) state stream of
          (rstate, rstream, Ok (rcx, res)) ->
            all (res :: acc) rcx state stream

          (estate, estream, Err ms) ->
            (estate, estream, Err ms)
  in
    primitive <| all [] [0]


formatError : List String -> InputStream -> String
formatError ms stream =
  let
    location = currentLocation stream
    separator = "|> "
    expectationSeparator = "\n  * "
    lineNumberOffset = floor (logBase 10 (toFloat location.line)) + 1
    separatorOffset = String.length separator
    padding = location.column + separatorOffset + 2
  in
  "Parse error around line:\n\n"
    ++ toString location.line ++ separator ++ location.sourceLine ++ "\n"
    ++ String.padLeft padding ' ' "^"
    ++ "\nI expected one of the following:\n"
    ++ expectationSeparator
    ++ String.join expectationSeparator ms


parse : String -> Result String (List C)
parse s =
  case Combine.parse program (s ++ "\n") of
    (_, _, Ok es) ->
      Ok es

    (_, stream, Err ms) ->
      Err <| formatError ms stream

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
