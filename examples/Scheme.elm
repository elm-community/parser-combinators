module Scheme exposing
    ( parse, test
    , E(..), bool, char, comment, expr, float, formatError, identifier, int, list, program, quasiquote, quote, str, unquote, unquoteSplice, vector
    )

{-| An example parser for the Scheme programming language.

To run this example, simply enter the examples and:

1.  run `elm repl`
2.  type in `import Scheme`
3.  try it out with `Scheme.test` or `Scheme.parse "(* 12 12 13)"`

@docs parse, test

-}

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
        ++ String.fromInt location.line
        ++ separator
        ++ location.source
        ++ "\n"
        ++ String.padLeft padding ' ' "^"
        ++ "\nI expected one of the following:\n"
        ++ expectationSeparator
        ++ String.join expectationSeparator ms


{-| Parse simple Scheme-code ...

    import Scheme exposing (parse)

    parse "(* 12 12 13)"
    -- Ok [EList [EIdentifier "*",EInt 12,EInt 12,EInt 13]]


    parse "(* 12 12 13"
    -- Err ("Parse error around line:\n\n0|> (* 12 12 13\n    ^\nI expected one of the following:\n\n  * expected end of input")

-}
parse : String -> Result String (List E)
parse s =
    case Combine.parse program s of
        Ok ( _, _, e ) ->
            Ok e

        Err ( _, stream, ms ) ->
            Err <| formatError ms stream


{-| Run the following example ...

    import Scheme exposing (test)

    test
    -- Ok [EList [EIdentifier "define",EList [EIdentifier "derivative",EIdentifier "f",EIdentifier "dx"],EList [EIdentifier "lambda",EList [EIdentifier "x"],EList [EIdentifier "/",EList [EIdentifier "-",EList [EIdentifier "f",EList [EIdentifier "+",EIdentifier "x",EIdentifier "dx"]],EList [EIdentifier "f",EIdentifier "x"]],EIdentifier "dx"]]]]

-}
test : Result String (List E)
test =
    parse """
  (define (derivative f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))
  """
