module Scheme exposing (..)

import Combine exposing (..)
import Combine.Char exposing (anyChar)
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


comment : Parser s E
comment =
    EComment
        <$> regex ";[^\n]+"
        <?> "comment"


bool : Parser s E
bool =
    let
        boolLiteral =
            choice
                [ True <$ string "#t"
                , False <$ string "#f"
                ]
    in
        EBool
            <$> boolLiteral
            <?> "boolean literal"


int : Parser s E
int =
    EInt
        <$> Combine.Num.int
        <?> "integer literal"


float : Parser s E
float =
    EFloat
        <$> Combine.Num.float
        <?> "float literal"


char : Parser s E
char =
    let
        charLiteral =
            string "#\\"
                *> choice
                    [ ' ' <$ string "space"
                    , '\n' <$ string "newline"
                    , anyChar
                    ]
    in
        EChar
            <$> charLiteral
            <?> "character literal"


str : Parser s E
str =
    EString
        <$> regex "\"(\\\"|[^\"])+\""
        <?> "string literal"


identifier : Parser s E
identifier =
    let
        letter =
            "a-zA-Z"

        specialInitial =
            "!$%&*/:<=>?^_~+\\-"

        initial =
            letter ++ specialInitial

        initialRe =
            "[" ++ initial ++ "]"

        digit =
            "0-9"

        specialSubsequent =
            ".@+\\-"

        subsequent =
            initial ++ digit ++ specialSubsequent

        subsequentRe =
            "[" ++ subsequent ++ "]*"

        identifierRe =
            initialRe ++ subsequentRe
    in
        EIdentifier <$> regex identifierRe <?> "identifier"


list : Parser s E
list =
    EList
        <$> parens (many expr)
        <?> "list"


vector : Parser s E
vector =
    EVector
        <$> (string "#(" *> many expr <* string ")")
        <?> "vector"


quote : Parser s E
quote =
    EQuote
        <$> (string "'" *> expr)
        <?> "quoted expression"


quasiquote : Parser s E
quasiquote =
    EQuasiquote
        <$> (string "`" *> expr)
        <?> "quasiquoted expression"


unquote : Parser s E
unquote =
    EUnquote
        <$> (string "," *> expr)
        <?> "unquoted expression"


unquoteSplice : Parser s E
unquoteSplice =
    EUnquoteSplice
        <$> (string ",@" *> expr)
        <?> "spliced expression"


expr : Parser s E
expr =
    lazy <|
        \() ->
            let
                parsers =
                    [ bool
                    , float
                    , int
                    , char
                    , str
                    , identifier
                    , list
                    , vector
                    , quote
                    , quasiquote
                    , unquote
                    , unquoteSplice
                    , comment
                    ]
            in
                whitespace *> choice parsers <* whitespace


program : Parser s (List E)
program =
    manyTill expr end


formatError : List String -> InputStream -> String
formatError ms stream =
    let
        location =
            currentLocation stream

        separator =
            "|> "

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


parse : String -> Result String (List E)
parse s =
    case Combine.parse program s of
        Ok ( _, _, e ) ->
            Ok e

        Err ( _, stream, ms ) ->
            Err <| formatError ms stream
