module Combine.Char exposing ( .. )

{-| This module contains `Char`-specific Parsers.  Avoid using this
module if performance is a concern.

You can achieve everything that you can with this module by using
`Combine.regex`, `Combine.string` or `Combine.primitive` and, in
general, those will be much faster.

# Parsers
@docs satisfy, char, anyChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit
-}

import Char
import Combine exposing (..)
import String


{-| Parse a character matching the predicate.

    parse (satisfy ((==) 'a')) "a" ==
      (Ok 'a', { input = "", position = 1 })

    parse (satisfy ((==) 'a')) "b" ==
      (Err ["could not satisfy predicate"], { input = "b", position = 0 })

-}
satisfy : (Char -> Bool) -> Parser s Char
satisfy pred =
  primitive <| \state stream ->
    let message = "could not satisfy predicate" in
    case String.uncons stream.input of
      Just (h, rest) ->
        if pred h
        then (state, { stream | input = rest, position = stream.position + 1 }, Ok h)
        else (state, stream, Err [message])

      Nothing ->
        (state, stream, Err [message])


{-| Parse an exact character match.

    parse (char 'a') "a" ==
      (Ok 'a', { input = "", position = 1 })

    parse (char 'a') "b" ==
      (Err ["expected 'a'"], { input = "b", position = 0 })

-}
char : Char -> Parser s Char
char c = satisfy ((==) c) <?> ("expected " ++ (toString c))


{-| Parse any character.

    parse anyChar "a" ==
      (Ok 'a', { input = "", position = 1 })

    parse anyChar "" ==
      (Err ["expected any character"], { input = "", position = 0 })

-}
anyChar : Parser s Char
anyChar = satisfy (always True) <?> "expected any character"


{-| Parse a character from the given list.

    parse (oneOf ['a', 'b']) "a" ==
      (Ok 'a', { input = "", position = 1 })

    parse (oneOf ['a', 'b']) "c" ==
      (Err ["expected one of ['a','b']"], { input = "c", position = 0 })

-}
oneOf : List Char -> Parser s Char
oneOf cs = satisfy (flip List.member cs) <?> ("expected one of " ++ (toString cs))


{-| Parse a character that is not in the given list.

    parse (noneOf ['a', 'b']) "c" ==
      (Ok 'c', { input = "", position = 1 })

    parse (noneOf ['a', 'b']) "a" ==
      (Err ["expected none of ['a','b']"], { input = "a", position = 0 })

-}
noneOf : List Char -> Parser s Char
noneOf cs =
  satisfy (not << flip List.member cs) <?> ("expected none of " ++ (toString cs))


{-| Parse a space character. -}
space : Parser s Char
space = satisfy ((==) ' ') <?> "expected space"


{-| Parse a `\t` character. -}
tab : Parser s Char
tab = satisfy ((==) '\t') <?> "expected tab"


{-| Parse a `\n` character. -}
newline : Parser s Char
newline = satisfy ((==) '\n') <?> "expected newline"


{-| Parse a `\r\n` sequence, returning a `\n` character. -}
crlf : Parser s Char
crlf = '\n' <$ regex "\r\n" <?> "expected crlf"


{-| Parse an end of line character or sequence, returning a `\n` character. -}
eol : Parser s Char
eol = newline <|> crlf


{-| Parse any lowercase character. -}
lower : Parser s Char
lower = satisfy Char.isLower <?> "expected a lowercase character"


{-| Parse any uppercase character. -}
upper : Parser s Char
upper = satisfy Char.isUpper <?> "expected an uppercase character"


{-| Parse any base 10 digit. -}
digit : Parser s Char
digit = satisfy Char.isDigit <?> "expected a digit"


{-| Parse any base 8 digit. -}
octDigit : Parser s Char
octDigit = satisfy Char.isOctDigit <?> "expected an octal digit"


{-| Parse any base 16 digit. -}
hexDigit : Parser s Char
hexDigit = satisfy Char.isHexDigit <?> "expected a hexadecimal digit"
