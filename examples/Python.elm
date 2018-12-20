module Python exposing (CompoundStatement(..), Expression(..), Indentation, Statement(..), addop, andExpr, andop, app, arithExpr, assertStmt, assignStmt, assignop, atom, attribute, block, blockStmt, bool, breakStmt, cmpExpr, cmpop, commaSep, comment, compoundStmt, continueStmt, dedent, delStmt, dict, dictSep, dropWhile, expr, exprList, exprStmt, factor, float, forStmt, formatError, funcStmt, globalStmt, identifier, importAs, importFromStmt, importStmt, indent, indentation, initIndentation, int, keyword, list, listSep, mulop, notExpr, orop, parse, passStmt, printStmt, program, raiseStmt, returnStmt, set, simpleStmt, spaces, stmt, str, term, test, token, tuple, whileStmt, whitespace, withStmt)

import Combine exposing (..)
import Combine.Char exposing (..)
import Combine.Num
import String


type Expression
    = EBool Bool
    | EInt Int
    | EFloat Float
    | EString String
    | EOr Expression Expression
    | EAnd Expression Expression
    | ENot Expression
    | ECmp String Expression Expression
    | EAdd Expression Expression
    | ESub Expression Expression
    | EMul Expression Expression
    | EDiv Expression Expression
    | EList (List Expression)
    | ETuple (List Expression)
    | EDict (List ( Expression, Expression ))
    | ESet (List Expression)
    | EIdentifier String
    | EAttribute Expression Expression
    | EApp Expression (List Expression)
    | EAssign Expression Expression


type Statement
    = SExpr Expression
    | SPrint (List Expression)
    | SDel (List Expression)
    | SPass
    | SBreak
    | SContinue
    | SReturn (Maybe Expression)
    | SRaise (Maybe (List Expression))
    | SImport (List ( Expression, Maybe Expression ))
    | SImportFrom Expression (List ( Expression, Maybe Expression ))
    | SGlobal (List Expression)
    | SAssert Expression (Maybe Expression)
    | SAssign Expression


type CompoundStatement
    = CSimple (List Statement)
    | CWhile Expression (List CompoundStatement)
    | CFor Expression Expression (List CompoundStatement)
    | CWith Expression (Maybe Expression) (List CompoundStatement)
    | CFunc Expression (List Expression) (List CompoundStatement)


type alias Indentation =
    List Int


initIndentation : Indentation
initIndentation =
    [ 0 ]


dropWhile : (a -> Bool) -> List a -> List a
dropWhile p xs =
    case xs of
        [] ->
            []

        x :: ys ->
            if p x then
                dropWhile p ys

            else
                xs


comment : Parser s String
comment =
    regex "#[^\n]*"


spaces : Parser s String
spaces =
    regex " *"


whitespace : Parser s String
whitespace =
    or comment spaces |> onerror "whitespace"


token : Parser s res -> Parser s res
token =
    between whitespace whitespace


keyword : String -> Parser s String
keyword s =
    string s |> ignore spaces


bool : Parser s Expression
bool =
    or (string "False" |> onsuccess False) (string "True" |> onsuccess True)
        |> map EBool
        |> onerror "boolean"


int : Parser s Expression
int =
    map EInt Combine.Num.int |> onerror "integer"


float : Parser s Expression
float =
    map EFloat Combine.Num.float |> onerror "float"


str : Parser s Expression
str =
    or
        (string "'"
            |> keep (regex "(\\\\'|[^'\n])*")
            |> ignore (string "'")
        )
        (string "\""
            |> keep (regex "(\\\\\"|[^\"\n])*")
            |> ignore (string "\"")
        )
        |> map EString
        |> onerror "string"


identifier : Parser s Expression
identifier =
    regex "[_a-zA-Z][_a-zA-Z0-9]*"
        |> map EIdentifier
        |> onerror "identifier"


attribute : Parser s Expression
attribute =
    lazy <|
        \() ->
            map EAttribute identifier
                |> ignore (string ".")
                |> andMap (or attribute identifier)
                |> onerror "attribute"


app : Parser s Expression
app =
    lazy <|
        \() ->
            or attribute identifier
                |> map EApp
                |> andMap (parens exprList)
                |> onerror "function call"


commaSep : Parser s String
commaSep =
    regex ", *"


dictSep : Parser s String
dictSep =
    regex ":[ \t\n]*"


listSep : Parser s String
listSep =
    regex ",[ \t\n]*"


list : Parser s Expression
list =
    lazy <|
        \() ->
            brackets (sepBy listSep expr)
                |> map EList
                |> onerror "error list"


tuple : Parser s Expression
tuple =
    lazy <|
        \() ->
            sepBy listSep expr
                |> parens
                |> map ETuple
                |> onerror "tuple"


dict : Parser s Expression
dict =
    lazy <|
        \() ->
            expr
                |> ignore dictSep
                |> map (,)
                |> andMap expr
                |> sepBy listSep
                |> brackets
                |> map EDict
                |> onerror "dictionary"


set : Parser s Expression
set =
    lazy <|
        \() ->
            sepBy listSep expr
                |> brackets
                |> map ESet
                |> onerror "set"


atom : Parser s Expression
atom =
    lazy <|
        \() ->
            choice [ bool, float, int, str, attribute, identifier, list, tuple, dict, set ]


expr : Parser s Expression
expr =
    lazy (\() -> chainl orop andExpr)


andExpr : Parser s Expression
andExpr =
    lazy (\() -> chainl andop notExpr)


notExpr : Parser s Expression
notExpr =
    lazy <|
        \() ->
            or (string "not" |> keep notExpr |> map ENot |> token) cmpExpr


cmpExpr : Parser s Expression
cmpExpr =
    lazy (\() -> chainl cmpop arithExpr)


arithExpr : Parser s Expression
arithExpr =
    lazy (\() -> chainl addop term)


term : Parser s Expression
term =
    lazy (\() -> chainl mulop factor)


factor : Parser s Expression
factor =
    lazy (\() -> token (choice [ parens expr, app, atom ]))


orop : Parser s (Expression -> Expression -> Expression)
orop =
    string "or" |> onsuccess EOr


andop : Parser s (Expression -> Expression -> Expression)
andop =
    string "and" |> onsuccess EAnd


cmpop : Parser s (Expression -> Expression -> Expression)
cmpop =
    [ string "<"
    , string ">"
    , string "=="
    , string "!="
    , string ">="
    , string "<="
    , string "in"
    , keyword "not" |> map (++) |> andMap (string "in")
    , string "is"
    , keyword "is" |> map (++) |> andMap (string "not")
    ]
        |> choice
        |> map ECmp


addop : Parser s (Expression -> Expression -> Expression)
addop =
    or
        (string "+" |> onsuccess EAdd)
        (string "-" |> onsuccess ESub)


mulop : Parser s (Expression -> Expression -> Expression)
mulop =
    or
        (string "*" |> onsuccess EMul)
        (string "/" |> onsuccess EDiv)


exprList : Parser s (List Expression)
exprList =
    sepBy commaSep expr


exprStmt : Parser s Statement
exprStmt =
    map SExpr expr |> onerror "expression"


printStmt : Parser s Statement
printStmt =
    keyword "print"
        |> keep exprList
        |> map SPrint
        |> onerror "print statement"


delStmt : Parser s Statement
delStmt =
    keyword "del"
        |> keep exprList
        |> map SDel
        |> onerror "del statement"


passStmt : Parser s Statement
passStmt =
    keyword "pass"
        |> onsuccess SPass
        |> onerror "pass statement"


breakStmt : Parser s Statement
breakStmt =
    keyword "break"
        |> onsuccess SBreak
        |> onerror "break statement"


continueStmt : Parser s Statement
continueStmt =
    keyword "continue"
        |> onsuccess SContinue
        |> onerror "continue statement"


returnStmt : Parser s Statement
returnStmt =
    keyword "return"
        |> keep (maybe expr)
        |> map SReturn
        |> onerror "return statement"


raiseStmt : Parser s Statement
raiseStmt =
    keyword "raise"
        |> keep (maybe exprList)
        |> map SRaise
        |> onerror "raise statement"


importAs : Parser s (List ( Expression, Maybe Expression ))
importAs =
    or attribute identifier
        |> map (,)
        |> andMap (whitespace |> ignore (keyword "as") |> keep identifier |> maybe)
        |> sepBy commaSep


importStmt : Parser s Statement
importStmt =
    keyword "import"
        |> keep importAs
        |> map SImport
        |> onerror "import statement"


importFromStmt : Parser s Statement
importFromStmt =
    keyword "from"
        |> keep (or attribute identifier)
        |> ignore spaces
        |> map SImportFrom
        |> ignore (keyword "import")
        |> andMap importAs
        |> onerror "from statement"


globalStmt : Parser s Statement
globalStmt =
    keyword "global"
        |> keep (sepBy commaSep identifier)
        |> map SGlobal


assertStmt : Parser s Statement
assertStmt =
    keyword "assert"
        |> keep expr
        |> map SAssert
        |> andMap (commaSep |> keep expr |> maybe)


assignop : Parser s (Expression -> Expression -> Expression)
assignop =
    token (string "=") |> onsuccess EAssign


assignStmt : Parser s Statement
assignStmt =
    chainr assignop expr |> map SAssign


indentation : Parser Indentation res -> Parser Indentation res
indentation p =
    let
        skipIndent stack =
            let
                current =
                    List.head stack
                        |> Maybe.withDefault 0

                validate s =
                    let
                        indent =
                            String.length s
                    in
                    if indent == current then
                        succeed ()

                    else
                        fail ("expected " ++ toString current ++ " spaces of indentation")
            in
            spaces |> andThen validate
    in
    withState skipIndent |> keep p


indent : Parser Indentation ()
indent =
    lazy <|
        \() ->
            let
                push s =
                    withState <|
                        \stack ->
                            let
                                indent =
                                    String.length s
                            in
                            case stack of
                                [] ->
                                    fail "negative indentation"

                                current :: _ ->
                                    if indent > current then
                                        putState (indent :: stack)

                                    else
                                        fail "expected indentation"
            in
            spaces |> andThen push |> lookAhead


dedent : Parser Indentation ()
dedent =
    lazy <|
        \() ->
            let
                pop s =
                    withState <|
                        \stack ->
                            let
                                rem =
                                    dropWhile ((/=) (String.length s)) stack
                            in
                            case rem of
                                _ :: _ ->
                                    putState rem

                                _ ->
                                    fail "unindent does not match any outer indentation level"
            in
            spaces |> andThen pop


block : Parser Indentation (List CompoundStatement)
block =
    lazy <|
        \() ->
            string ":"
                |> ignore whitespace
                |> ignore eol
                |> ignore indent
                |> keep (many1 stmt)
                |> ignore dedent


blockStmt : Parser Indentation (List CompoundStatement -> CompoundStatement) -> Parser Indentation CompoundStatement
blockStmt p =
    indentation p
        |> andThen
            (\f ->
                block
                    |> andThen (\ss -> succeed (f ss))
            )


simpleStmt : Parser Indentation CompoundStatement
simpleStmt =
    lazy <|
        \() ->
            let
                stmt =
                    choice
                        [ assertStmt
                        , globalStmt
                        , importFromStmt
                        , importStmt
                        , raiseStmt
                        , returnStmt
                        , continueStmt
                        , breakStmt
                        , passStmt
                        , delStmt
                        , printStmt
                        , assignStmt
                        , exprStmt
                        ]
            in
            sepBy (string ";" |> ignore whitespace) stmt
                |> ignore (or (skip eol) end)
                |> map CSimple
                |> indentation


whileStmt : Parser s (List CompoundStatement -> CompoundStatement)
whileStmt =
    keyword "while"
        |> keep expr
        |> map CWhile


forStmt : Parser s (List CompoundStatement -> CompoundStatement)
forStmt =
    keyword "for"
        |> keep identifier
        |> map CFor
        |> andMap (spaces |> ignore (keyword "in") |> keep expr)


withStmt : Parser s (List CompoundStatement -> CompoundStatement)
withStmt =
    keyword "with"
        |> keep expr
        |> map CWith
        |> andMap (keyword "as" |> keep identifier |> maybe)


funcStmt : Parser s (List CompoundStatement -> CompoundStatement)
funcStmt =
    keyword "def"
        |> keep identifier
        |> map CFunc
        |> andMap (parens <| sepBy commaSep identifier)


compoundStmt : Parser Indentation CompoundStatement
compoundStmt =
    lazy <|
        \() ->
            let
                parsers =
                    List.map blockStmt
                        [ whileStmt
                        , forStmt
                        , withStmt
                        , funcStmt
                        ]
            in
            choice parsers


stmt : Parser Indentation CompoundStatement
stmt =
    lazy <|
        \() ->
            or compoundStmt simpleStmt


program : Parser Indentation (List CompoundStatement)
program =
    manyTill stmt end


formatError : List String -> InputStream -> String
formatError ms stream =
    let
        location =
            currentLocation stream

        separator =
            "| "

        expectationSeparator =
            "\n  * "

        lineNumberOffset =
            floor (logBase 10 (toFloat location.line)) + 1

        separatorOffset =
            String.length separator

        padding =
            location.column + separatorOffset + 2
    in
    "Parse error around line:\n\n"
        ++ toString location.line
        ++ separator
        ++ location.source
        ++ "\n"
        ++ String.padLeft padding ' ' "^"
        ++ "\nI expected one of the following:\n"
        ++ expectationSeparator
        ++ String.join expectationSeparator ms


parse : String -> Result String (List CompoundStatement)
parse s =
    case Combine.runParser program initIndentation s of
        Ok ( _, _, es ) ->
            Ok es

        Err ( _, stream, ms ) ->
            Err <| formatError ms stream


test : Result String (List CompoundStatement)
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
