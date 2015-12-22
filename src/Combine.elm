module Combine ( Parser(..), ParseFn, Context, Result(..)
               , parse, app, rec
               , bimap, map, mapError
               , andThen, andMap
               , fail, succeed, string, regex, while, end
               , or, choice, optional, maybe, many, many1, manyTill
               , sepBy, sepBy1, skip, skipMany, skipMany1
               , chainl, chainr, count, between, parens
               , brackets, squareBrackets
               ) where

{-| This library provides reasonably fast parser combinators.

@docs Parser, ParseFn, Context, Result

# Running a Parser
@docs parse, app, rec

# Transforming Parsers
@docs bimap, map, mapError

# Chaining Parsers
@docs andThen, andMap

# Parsers
@docs fail, succeed, string, regex, while, end, or, choice, optional, maybe, many, many1, manyTill, sepBy, sepBy1, skip, skipMany, skipMany1, chainl, chainr, count, between, parens, brackets, squareBrackets
-}

import Lazy as L
import String
import Regex exposing (Regex(..))


{-| The contex over which `ParseFn`s operate. -}
type alias Context =
  { input : String
  , position : Int
  }


{-| Running a `Parser` results in one of two states:

* `Done` when the parser has successfully parsed the input (or part of
it) to a result, or
* `Fail` when the parser has failed with a list of error messages.
-}
type Result res
  = Done res
  | Fail (List String)


{-| At their core, Parsers are functions from a `Context` to a
tuple of a `Result` and a new `Context`.

    myParseFn : ParseFn Int
    myParseFn c = (Done 1, c)

    myParser : Parser Int
    myParser = Parser myParseFn

    parse myParser "a" ==
      (Done 1, { input = "a", position = 0 })
-}
type alias ParseFn res =
  Context -> (Result res, Context)


{-| A wrapper type around `ParseFn`s used to differentiate between
eager and lazy parsers. -}
type Parser res
  = Parser (ParseFn res)
  | RecursiveParser (L.Lazy (ParseFn res))


{-| Unwrap a parser so it can be applied to a context. -}
app : Parser res -> Context -> (Result res, Context)
app p =
  case p of
    Parser p ->
      p

    RecursiveParser t ->
      L.force t


{-| Parse a string. -}
parse : Parser res -> String -> (Result res, Context)
parse p input = app p { input = input, position = 0 }


{-| Defer a parser's evaluation.

    type E = ETerm String | EList (List E)

    whitespace : Parser String
    whitespace = regex "[ \t\r\n]*"

    term : Parser E
    term = ETerm `map` (whitespace *> regex "[a-zA-Z]+" <* whitespace)

    list : Parser E
    list = rec (\() -> EList `map` (string "(" *> many (term `or` list) <* string ")"))

    parse list "" ==
      (Fail ["expected \"(\""], { input = "", position = 0 })

    parse list "()" ==
      (Done (EList []), { input = "", position = 2 })

    parse list "(a (b c))" ==
      (Done (EList [ETerm "a", EList [ETerm "b", ETerm "c"]]), { input = "", position = 9 })
-}
rec : (() -> Parser res) -> Parser res
rec t =
  RecursiveParser << L.lazy <| \() -> app (t ())


{-| Transform both the result and error message of a parser. -}
bimap : (res -> res')
      -> (List String -> List String)
      -> Parser res
      -> Parser res'
bimap fok ferr p =
  Parser <| \cx ->
    case app p cx of
      (Done r, cx) ->
        (Done (fok r), cx)

      (Fail m, c) ->
        (Fail (ferr m), cx)


{-| Transform the result of a parser.

    parse (map String.toUpper (string "a")) "a" ==
      (Done "A", { input = "", position = 1 })
-}
map : (res -> res') -> Parser res -> Parser res'
map f p = bimap f identity p


{-| Transform the error of a parser.

    parse (mapError (\_ -> ["bad input"]) (string "a")) "b" ==
      (Fail ["bad input"], { input = "b", position = 0 })
-}
mapError : (List String -> List String) -> Parser res -> Parser res
mapError = bimap identity


{-| Sequence two parsers by passing in the results of the first parser
to the second. -}
andThen : Parser res -> (res -> Parser res') -> Parser res'
andThen p f =
  Parser <| \cx ->
    case app p cx of
      (Done res, cx) ->
        app (f res) cx

      (Fail m, cx) ->
        (Fail m, cx)


{-| Sequence two parsers.

    import Maybe
    import Result
    import String

    num : Parser Int
    num = (Maybe.withDefault 0 << Result.toMaybe << String.toInt) `map` regex "[0-9]+"

    sum : Parser Int
    sum = (+) `map` (num <* string "+") `andMap` num

    parse sum "1+2" ==
      (Done 3, { input = "", position = 3 })
-}
andMap : Parser (res -> res') -> Parser res -> Parser res'
andMap lp rp =
  lp
    `andThen` \f -> rp
    `andThen` \x -> succeed (f x)


{-| Fail without consuming any input. -}
fail : List String -> Parser res
fail ms =
  Parser <| \cx ->
    (Fail ms, cx)


{-| Return a value without consuming any input.

    parse (succeed 1) "a" ==
      (Done 1, { input = "a", position = 0 })
-}
succeed : res -> Parser res
succeed r =
  Parser <| \cx ->
    (Done r, cx)


{-| Parse an exact string match.

    parse (string "hello") "hello world" ==
      (Done "hello", { input = " world", position = 5 })

    parse (string "hello") "goodbye" ==
      (Fail ["expected \"hello\""], { input = "goodbye", position = 0 })
-}
string : String -> Parser String
string s =
  Parser <| \cx ->
    if String.startsWith s cx.input
    then
      let
        len = String.length s
        rem = String.dropLeft len cx.input
        pos = cx.position + len
      in (Done s, {cx | input = rem, position = pos})
    else (Fail ["expected " ++ (toString s)], cx)


{-| Parse a Regex match.

Regular expressions must match from the beginning of the input and their
subgroups are ignored. A `^` is added implicitly to the beginning of
every pattern unless one already exists.

    parse (regex "a+") "aaaaab" ==
      (Done "aaaaa", { input = "b", position = 5 })
-}
regex : String -> Parser String
regex pattern =
  let
    pattern' =
      if String.startsWith "^" pattern
      then pattern
      else "^" ++ pattern
  in
  Parser <| \cx ->
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern') cx.input of
      [match] ->
        let
          len = String.length match.match
          rem = String.dropLeft len cx.input
          pos = cx.position + len
        in (Done match.match, {cx | input = rem, position = pos })

      _ ->
        (Fail ["expected input matching Regexp /" ++ pattern' ++ "/"], cx)


{-| Consume input while the predicate matches.

    parse (while ((/=) ' ')) "test 123" ==
      (Done "test", { input = " 123", position = 4 })
-}
while : (Char -> Bool) -> Parser String
while pred =
  let
    accumulate acc cx =
      case String.uncons cx.input of
        Just (h, rest) ->
          if pred h
          then
            let
              c = String.cons h ""
              pos = cx.position + 1
            in accumulate (acc ++ c) {cx | input = rest, position = pos}
          else (acc, cx)

        Nothing ->
          (acc, cx)
  in
    Parser <| \cx ->
      let (res, cx') = accumulate "" cx in
      (Done res, cx')


{-| Fail when the input is not empty.

    parse end "" == (Done (), { input = "", position = 0 })
    parse end "a" == (Fail ["expected end of input"], { input = "a", position = 0 })
-}
end : Parser ()
end =
  Parser <| \cx ->
    if cx.input == ""
    then (Done (), cx)
    else (Fail ["expected end of input"], cx)


{-| Choose between two parsers.

    parse (string "a" `or` string "b") "a" ==
      (Done "a", { input = "", position = 1 })

    parse (string "a" `or` string "b") "b" ==
      (Done "b", { input = "", position = 1 })

    parse (string "a" `or` string "b") "c" ==
      (Fail ["expected \"a\"", "expected \"b\""], { input = "c", position = 0 })
-}
or : Parser res -> Parser res -> Parser res
or lp rp =
  Parser <| \cx ->
    let res = app lp cx in
    case res of
      (Done _, _) ->
        res

      (Fail lm, _) ->
        let res' = app rp cx in
        case res' of
          (Done _, _) ->
            res'

          (Fail rm, _) ->
            (Fail (lm ++ rm), cx)


{-| Choose between a list of parsers.

    parse (choice [string "a", string "b"]) "a" ==
      (Done "a", { input = "", position = 1 })

    parse (choice [string "a", string "b"]) "b" ==
      (Done "b", { input = "", position = 1 })
-}
choice : List (Parser res) -> Parser res
choice xs =
  List.foldr or (fail []) xs


{-| Return a default value when the given parser fails.

    letterA : Parser String
    letterA = optional "a" (string "a")

    parse letterA "a" == (Done "a", { input = "", position = 1 })
    parse letterA "b" == (Done "a", { input = "b", position = 0 })
-}
optional : res -> Parser res -> Parser res
optional res p =
  p `or` succeed res


{-| Wrap the return value into a `Maybe`. Returns `Nothing` on failure.

    parse (maybe (string "a")) "a" ==
      (Done (Just "a"), { input = "", position = 1 })

    parse (maybe (string "a")) "b" ==
      (Done Nothing, { input = "b", position = 0 })
-}
maybe : Parser res -> Parser (Maybe res)
maybe p =
  Parser <| \cx ->
    case app p cx of
      (Done res, cx') ->
        (Done (Just res), cx')

      _ ->
        (Done Nothing, cx)


{-| Apply a parser until it fails and return a list of the results.

    parse (many (string "a")) "aaab" ==
      (Done ["a", "a", "a"], { input = "b", position = 3 })

    parse (many (string "a")) "" ==
      (Done [], { input = "", position = 0 })
-}
many : Parser res -> Parser (List res)
many p =
  let
    accumulate acc cx =
      case app p cx of
        (Done res, cx') ->
          accumulate (res :: acc) cx'

        _ ->
          (List.reverse acc, cx)
  in
    Parser <| \cx ->
      let (res, cx') = accumulate [] cx in
      (Done res, cx')


{-| Parse at least one result.

    parse (many1 (string "a")) "a" ==
      (Done ["a"], { input = "", position = 1 })

    parse (many1 (string "a")) "" ==
      (Fail ["expected \"a\""], { input = "", position = 0 })
-}
many1 : Parser res -> Parser (List res)
many1 p =
  (::) `map` p `andMap` many p


{-| Applies parser `p` zero or more times until parser `end` succeeds.

Returns the list of values returned by `p`.

This parser can be used to scan comments:

    (string "<!--" *> manyTill anyChar (string "-->"))
-}
manyTill : Parser res -> Parser end -> Parser (List res)
manyTill p end =
  let
    accumulate acc cx =
      case app end cx of
        (Fail err, cx) ->
          case app p cx of
            (Done res, cx') ->
              accumulate (res :: acc) cx'

            _ ->
              (Fail err, cx)

        (Done _, cx') ->
          (Done (List.reverse acc), cx')
  in
    Parser <| accumulate []


{-| Parser zero or more occurences of one parser separated by another.

    parse (sepBy (string ",") (string "a")) "b" ==
      (Done [], { input = "b", position = 0 })

    parse (sepBy (string ",") (string "a")) "a,a,a" ==
      (Done ["a", "a", "a"], { input = "", position = 5 })

    parse (sepBy (string ",") (string "a")) "a,a,b" ==
      (Done ["a", "a"], { input = ",b", position = 3 })
-}
sepBy : Parser x -> Parser res -> Parser (List res)
sepBy sep p = sepBy1 sep p `or` succeed []


{-| Parse one or more occurences of one parser separated by another. -}
sepBy1 : Parser x -> Parser res -> Parser (List res)
sepBy1 sep p =
  (::) `map` p `andMap` many ((flip always) `map` sep `andMap` p)


{-| Apply a parser and skip its result. -}
skip : Parser x -> Parser ()
skip p = p `andThen` (always <| succeed ())


{-| Apply a parser and skip its result many times. -}
skipMany : Parser x -> Parser ()
skipMany p = many (skip p) `andThen` (always <| succeed ())


{-| Apply a parser and skip its result at least once. -}
skipMany1 : Parser x -> Parser ()
skipMany1 p = many1 (skip p) `andThen` (always <| succeed ())


{-| Parse one or more occurences of `p` separated by `op`, recursively
apply all functions returned by `op` to the values returned by `p`. See
the `examples/Calc.elm` file for an example.
-}
chainl : Parser res -> Parser (res -> res -> res) -> Parser res
chainl p op =
  let
    accumulate x =
      (op `andThen` \f -> p
          `andThen` \y -> accumulate (f x y)) `or` succeed x
  in
  p `andThen` accumulate


{-| Similar to `chainl` but functions of `op` are applied in
right-associative order to the values of `p`. -}
chainr : Parser res -> Parser (res -> res -> res) -> Parser res
chainr p op =
  let
    accumulate x =
      (op `andThen` \f -> p
          `andThen` accumulate
          `andThen` \y -> succeed (f x y)) `or` succeed x
  in
  p `andThen` accumulate


{-| Parse `n` occurences of `p`. -}
count : Int -> Parser res -> Parser (List res)
count n p =
  let
    accumulate x acc =
      if x <= 0
      then succeed (List.reverse acc)
      else p `andThen` \res -> accumulate (x - 1) (res :: acc)
  in accumulate n []


{-| Parse something between two other parsers.

The parser

    between (string "(") (string ")") (string "a")

is equivalent to the parser

    string "(" *> string "a" <* string ")"
-}
between : Parser left -> Parser right -> Parser res -> Parser res
between lp rp p = (flip (always << always)) `map` lp `andMap` p `andMap` rp


{-| Parse something between parentheses. -}
parens : Parser res -> Parser res
parens = between (string "(") (string ")")


{-| Parse something between brackets. -}
brackets : Parser res -> Parser res
brackets = between (string "{") (string "}")


{-| Parse something between square brackets. -}
squareBrackets : Parser res -> Parser res
squareBrackets = between (string "[") (string "]")
