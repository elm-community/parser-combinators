module Combine.Char where

{-| This module contains `Char`-specific Parsers.

# Parsers
@docs satisfy, char, anyChar, oneOf, noneOf, space, tab, newline, crlf, eol, lower, upper, digit, octDigit, hexDigit
-}

import Char
import Combine exposing (..)
import Combine.Infix exposing (..)
import String


{-| Parse a character matching the predicate.

    parse (satisfy ((==) 'a')) "a" ==
      (Done 'a', { input = "", position = 1 })

    parse (satisfy ((==) 'a')) "b" ==
      (Fail ["could not satisfy predicate"], { input = "b", position = 0 })
-}
satisfy : (Char -> Bool) -> Parser Char
satisfy pred =
  Parser <| \cx ->
    let message = "could not satisfy predicate" in
    case String.uncons cx.input of
      Just (h, rest) ->
        if pred h
        then (Done h, { cx | input = rest, position = cx.position + 1 })
        else (Fail [message], cx)

      Nothing ->
        (Fail [message], cx)


{-| Parse an exact character match.

    parse (char 'a') "a" ==
      (Done 'a', { input = "", position = 1 })

    parse (char 'a') "b" ==
      (Fail ["expected 'a'"], { input = "b", position = 0 })
-}
char : Char -> Parser Char
char c = satisfy ((==) c) <?> ("expected " ++ (toString c))


{-| Parse any character.

    parse anyChar "a" ==
      (Done 'a', { input = "", position = 1 })

    parse anyChar "" ==
      (Fail ["expected any character"], { input = "", position = 0 })
-}
anyChar : Parser Char
anyChar = satisfy (always True) <?> "expected any character"


{-| Parse a character from the given list.

    parse (oneOf ['a', 'b']) "a" ==
      (Done 'a', { input = "", position = 1 })

    parse (oneOf ['a', 'b']) "c" ==
      (Fail ["expected one of ['a','b']"], { input = "c", position = 0 })
-}
oneOf : List Char -> Parser Char
oneOf cs = satisfy (flip List.member cs) <?> ("expected one of " ++ (toString cs))


{-| Parse a character from the given list.

    parse (noneOf ['a', 'b']) "c" ==
      (Done 'c', { input = "", position = 1 })

    parse (noneOf ['a', 'b']) "a" ==
      (Fail ["expected none of ['a','b']"], { input = "a", position = 0 })
-}
noneOf : List Char -> Parser Char
noneOf cs =
  satisfy (not << flip List.member cs) <?> ("expected none of " ++ (toString cs))


{-| Parse a space character. -}
space : Parser Char
space = satisfy ((==) ' ') <?> "expected space"


{-| Parses a `\t` character. -}
tab : Parser Char
tab = satisfy ((==) '\t') <?> "expected tab"


{-| Parse a `\n` character. -}
newline : Parser Char
newline = satisfy ((==) '\n') <?> "expected newline"


{-| Parse a `\r\n` sequence. Returns a `\n` character. -}
crlf : Parser Char
crlf = '\n' <$ regex "\r\n" <?> "expected crlf"


{-| Parses an end of line character or sequence. Returns a `\n` character. -}
eol : Parser Char
eol = newline <|> crlf


{-| Parses any lowercase character. -}
lower : Parser Char
lower = satisfy Char.isLower <?> "expected a lowercase character"


{-| Parses any uppercase character. -}
upper : Parser Char
upper = satisfy Char.isUpper <?> "expected an uppercase character"


{-| Parses any base 10 digit. -}
digit : Parser Char
digit = satisfy Char.isDigit <?> "expected a digit"


{-| Parses any base 8 digit. -}
octDigit : Parser Char
octDigit = satisfy Char.isOctDigit <?> "expected an octal digit"


{-| Parses any base 16 digit. -}
hexDigit : Parser Char
hexDigit = satisfy Char.isHexDigit <?> "expected a hexadecimal digit"
