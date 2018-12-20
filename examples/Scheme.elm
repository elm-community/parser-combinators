module Scheme exposing (E(..), bool, char, comment, expr, float, formatError, identifier, int, list, parse, program, quasiquote, quote, str, unquote, unquoteSplice, vector)

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
    regex ";[^\n]+" |> map EComment |> onerror "comment"


bool : Parser s E
bool =
    let
        boolLiteral =
            or
                (string "#t" |> onsuccess True)
                (string "#f" |> onsuccess False)
    in
    map EBool boolLiteral |> onerror "boolean literal"


int : Parser s E
int =
    map EInt Combine.Num.int |> onerror "integer literal"


float : Parser s E
float =
    map EFloat Combine.Num.float |> onerror "float literal"


char : Parser s E
char =
    let
        charLiteral =
            string "#\\"
                |> keep
                    (choice
                        [ string "space" |> onsuccess ' '
                        , string "newline" |> onsuccess '\n'
                        , anyChar
                        ]
                    )
    in
    map EChar charLiteral |> onerror "character literal"


str : Parser s E
str =
    regex "\"(\\\"|[^\"])+\""
        |> map EString
        |> onerror "string literal"


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
    regex identifierRe |> map EIdentifier |> onerror "identifier"


list : Parser s E
list =
    many expr |> parens |> map EList |> onerror "list"


vector : Parser s E
vector =
    string "#("
        |> keep (many expr)
        |> ignore (string ")")
        |> map EVector
        |> onerror "vector"


quote : Parser s E
quote =
    string "'"
        |> keep expr
        |> map EQuote
        |> onerror "quoted expression"


quasiquote : Parser s E
quasiquote =
    string "`"
        |> keep expr
        |> map EQuasiquote
        |> onerror "quasiquoted expression"


unquote : Parser s E
unquote =
    string ","
        |> keep expr
        |> map EUnquote
        |> onerror "unquoted expression"


unquoteSplice : Parser s E
unquoteSplice =
    string ",@"
        |> keep expr
        |> map EUnquoteSplice
        |> onerror "spliced expression"


expr : Parser s E
expr =
    lazy <|
        \() ->
            let
                parsers =
                    choice
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
            whitespace
                |> keep parsers
                |> ignore whitespace


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
