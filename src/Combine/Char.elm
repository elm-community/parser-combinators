module Combine.Char exposing (satisfy, char, anyChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit)

{-| This module contains `Char`-specific Parsers.

Avoid using this module if performance is a concern. You can achieve
everything that you can do with this module by using `Combine.regex`,
`Combine.string` or `Combine.primitive` and, in general, those will be
much faster.


# Parsers

@docs satisfy, char, anyChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit

-}

import Char
import Combine exposing (Parser, onerror, onsuccess, or, primitive, regex, string)
import String


{-| Parse a character matching the predicate.

    parse (satisfy ((==) 'a')) "a" ==
    -- Ok 'a'

    parse (satisfy ((==) 'a')) "b" ==
    -- Err ["could not satisfy predicate"]

-}
satisfy : (Char -> Bool) -> Parser s Char
satisfy pred =
    primitive <|
        \state stream ->
            let
                message =
                    "could not satisfy predicate"
            in
            case String.uncons stream.input of
                Just ( h, rest ) ->
                    if pred h then
                        ( state, { stream | input = rest, position = stream.position + 1 }, Ok h )

                    else
                        ( state, stream, Err [ message ] )

                Nothing ->
                    ( state, stream, Err [ message ] )


{-| Parse an exact character match.

    parse (char 'a') "a" ==
    -- Ok 'a'

    parse (char 'a') "b" ==
    -- Err ["expected 'a'"]

-}
char : Char -> Parser s Char
char c =
    satisfy ((==) c) |> onerror ("expected " ++ toString c)


{-| Parse any character.

    parse anyChar "a" ==
    -- Ok 'a'

    parse anyChar "" ==
    -- Err ["expected any character"]

-}
anyChar : Parser s Char
anyChar =
    satisfy (always True) |> onerror "expected any character"


{-| Parse a character from the given list.

    parse (oneOf ['a', 'b']) "a" ==
    -- Ok 'a'

    parse (oneOf ['a', 'b']) "c" ==
    -- Err ["expected one of ['a','b']"]

-}
oneOf : List Char -> Parser s Char
oneOf cs =
    satisfy (flip List.member cs) |> onerror ("expected one of " ++ toString cs)


{-| Parse a character that is not in the given list.

    parse (noneOf ['a', 'b']) "c" ==
    -- Ok 'c'

    parse (noneOf ['a', 'b']) "a" ==
    -- Err ["expected none of ['a','b']"]

-}
noneOf : List Char -> Parser s Char
noneOf cs =
    satisfy (not << flip List.member cs) |> onerror ("expected none of " ++ toString cs)


{-| Parse a space character.
-}
space : Parser s Char
space =
    satisfy ((==) ' ') |> onerror "expected space"


{-| Parse a `\t` character.
-}
tab : Parser s Char
tab =
    satisfy ((==) '\t') |> onerror "expected tab"


{-| Parse a `\n` character.
-}
newline : Parser s Char
newline =
    satisfy ((==) '\n') |> onerror "expected newline"


{-| Parse a `\r\n` sequence, returning a `\n` character.
-}
crlf : Parser s Char
crlf =
    string "\r\n" |> onsuccess '\n' |> onerror "expected crlf"


{-| Parse an end of line character or sequence, returning a `\n` character.
-}
eol : Parser s Char
eol =
    or newline crlf


{-| Parse any lowercase character.
-}
lower : Parser s Char
lower =
    satisfy Char.isLower |> onerror "expected a lowercase character"


{-| Parse any uppercase character.
-}
upper : Parser s Char
upper =
    satisfy Char.isUpper |> onerror "expected an uppercase character"


{-| Parse any base 10 digit.
-}
digit : Parser s Char
digit =
    satisfy Char.isDigit |> onerror "expected a digit"


{-| Parse any base 8 digit.
-}
octDigit : Parser s Char
octDigit =
    satisfy Char.isOctDigit |> onerror "expected an octal digit"


{-| Parse any base 16 digit.
-}
hexDigit : Parser s Char
hexDigit =
    satisfy Char.isHexDigit |> onerror "expected a hexadecimal digit"
