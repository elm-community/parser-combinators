module Python exposing (..)

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
    comment <|> spaces <?> "whitespace"


token : Parser s res -> Parser s res
token =
    between whitespace whitespace


keyword : String -> Parser s String
keyword s =
    string s <* spaces


bool : Parser s Expression
bool =
    EBool
        <$> choice
                [ False <$ string "False"
                , True <$ string "True"
                ]
        <?> "boolean"


int : Parser s Expression
int =
    EInt <$> Combine.Num.int <?> "integer"


float : Parser s Expression
float =
    EFloat <$> Combine.Num.float <?> "float"


str : Parser s Expression
str =
    EString
        <$> choice
                [ string "'" *> regex "(\\\\'|[^'\n])*" <* string "'"
                , string "\"" *> regex "(\\\\\"|[^\"\n])*" <* string "\""
                ]
        <?> "string"


identifier : Parser s Expression
identifier =
    EIdentifier <$> regex "[_a-zA-Z][_a-zA-Z0-9]*" <?> "identifier"


attribute : Parser s Expression
attribute =
    lazy <|
        \() ->
            EAttribute
                <$> identifier
                <* string "."
                <*> choice [ attribute, identifier ]
                <?> "attribute"


app : Parser s Expression
app =
    lazy <|
        \() ->
            EApp
                <$> choice [ attribute, identifier ]
                <*> parens exprList
                <?> "function call"


commaSep : Parser s String
commaSep =
    regex ", *"


dictSep : Parser s String
dictSep =
    regex ":[ \t\x0D\n]*"


listSep : Parser s String
listSep =
    regex ",[ \t\x0D\n]*"


list : Parser s Expression
list =
    lazy <|
        \() ->
            EList
                <$> brackets (sepBy listSep expr)
                <?> "list"


tuple : Parser s Expression
tuple =
    lazy <|
        \() ->
            ETuple
                <$> parens (sepBy listSep expr)
                <?> "tuple"


dict : Parser s Expression
dict =
    lazy <|
        \() ->
            EDict
                <$> brackets (sepBy listSep ((,) <$> expr <* dictSep <*> expr))
                <?> "dictionary"


set : Parser s Expression
set =
    lazy <|
        \() ->
            ESet
                <$> brackets (sepBy listSep expr)
                <?> "set"


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
            (token <| ENot <$> (string "not" *> notExpr)) <|> cmpExpr


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
    lazy (\() -> token (parens expr <|> app <|> atom))


orop : Parser s (Expression -> Expression -> Expression)
orop =
    EOr <$ string "or"


andop : Parser s (Expression -> Expression -> Expression)
andop =
    EAnd <$ string "and"


cmpop : Parser s (Expression -> Expression -> Expression)
cmpop =
    ECmp
        <$> choice
                [ string "<"
                , string ">"
                , string "=="
                , string "!="
                , string ">="
                , string "<="
                , string "in"
                , (++) <$> keyword "not" <*> string "in"
                , string "is"
                , (++) <$> keyword "is" <*> string "not"
                ]


addop : Parser s (Expression -> Expression -> Expression)
addop =
    choice
        [ EAdd <$ string "+"
        , ESub <$ string "-"
        ]


mulop : Parser s (Expression -> Expression -> Expression)
mulop =
    choice
        [ EMul <$ string "*"
        , EDiv <$ string "/"
        ]


exprList : Parser s (List Expression)
exprList =
    sepBy commaSep expr


exprStmt : Parser s Statement
exprStmt =
    SExpr <$> expr <?> "expression"


printStmt : Parser s Statement
printStmt =
    SPrint <$> (keyword "print" *> exprList) <?> "print statement"


delStmt : Parser s Statement
delStmt =
    SDel <$> (keyword "del" *> exprList) <?> "del statement"


passStmt : Parser s Statement
passStmt =
    SPass <$ keyword "pass" <?> "pass statement"


breakStmt : Parser s Statement
breakStmt =
    SBreak <$ keyword "break" <?> "break statement"


continueStmt : Parser s Statement
continueStmt =
    SContinue <$ keyword "continue" <?> "continue statement"


returnStmt : Parser s Statement
returnStmt =
    SReturn <$> (keyword "return" *> maybe expr) <?> "return statement"


raiseStmt : Parser s Statement
raiseStmt =
    SRaise <$> (keyword "raise" *> maybe exprList) <?> "raise statement"


importAs : Parser s (List ( Expression, Maybe Expression ))
importAs =
    sepBy commaSep <|
        (,)
            <$> choice [ attribute, identifier ]
            <*> maybe (whitespace *> keyword "as" *> identifier)


importStmt : Parser s Statement
importStmt =
    SImport
        <$> (keyword "import" *> importAs)
        <?> "import statement"


importFromStmt : Parser s Statement
importFromStmt =
    SImportFrom
        <$> (keyword "from" *> choice [ attribute, identifier ] <* spaces)
        <*> (keyword "import" *> importAs)
        <?> "from statement"


globalStmt : Parser s Statement
globalStmt =
    SGlobal <$> (keyword "global" *> sepBy commaSep identifier)


assertStmt : Parser s Statement
assertStmt =
    SAssert
        <$> (keyword "assert" *> expr)
        <*> maybe (commaSep *> expr)


assignop : Parser s (Expression -> Expression -> Expression)
assignop =
    EAssign <$ token (string "=")


assignStmt : Parser s Statement
assignStmt =
    SAssign <$> chainr assignop expr


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
                spaces >>= validate
    in
        withState skipIndent *> p


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
                lookAhead <| spaces >>= push


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
                spaces >>= pop


block : Parser Indentation (List CompoundStatement)
block =
    lazy <|
        \() ->
            string ":"
                *> whitespace
                *> eol
                *> indent
                *> many1 stmt
                <* dedent


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
                indentation (CSimple <$> sepBy (string ";" <* whitespace) stmt <* (() <$ eol <|> end))


whileStmt : Parser s (List CompoundStatement -> CompoundStatement)
whileStmt =
    CWhile <$> (keyword "while" *> expr)


forStmt : Parser s (List CompoundStatement -> CompoundStatement)
forStmt =
    CFor
        <$> (keyword "for" *> identifier)
        <*> (spaces *> keyword "in" *> expr)


withStmt : Parser s (List CompoundStatement -> CompoundStatement)
withStmt =
    CWith
        <$> (keyword "with" *> expr)
        <*> maybe (keyword "as" *> identifier)


funcStmt : Parser s (List CompoundStatement -> CompoundStatement)
funcStmt =
    CFunc
        <$> (keyword "def" *> identifier)
        <*> parens (sepBy commaSep identifier)


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
            compoundStmt <|> simpleStmt


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
