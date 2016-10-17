module Combine
  exposing
    ( Parser, Context, primitive
    , parse, app, rec
    , map, mapError
    , andThen, andMap, sequence
    , fail, succeed, string, regex, while, end
    , or, choice, optional, maybe, many, many1, manyTill
    , sepBy, sepBy1, sepEndBy, sepEndBy1, skip, skipMany, skipMany1
    , chainl, chainr, count, between, parens
    , braces, brackets, whitespace
    , (<?>), (>>=), (<$>), (<$), ($>), (<*>), (<*), (*>), (<|>)
    )

{-| This library provides reasonably fast parser combinators.

# Parsers
@docs Parser, Context

## Constructing Parsers
@docs primitive, rec

## Running a Parser
@docs parse, app

## Transforming Parsers
@docs map, mapError

## Chaining Parsers
@docs andThen, andMap, sequence

## Combinators
@docs fail, succeed, string, regex, while, end, or, choice, optional, maybe, many, many1, manyTill, sepBy, sepBy1, sepEndBy, sepEndBy1, skip, skipMany, skipMany1, chainl, chainr, count, between, parens, braces, brackets, whitespace

## Infix combinators
@docs (<?>), (>>=), (<$>), (<$), ($>), (<*>), (<*), (*>), (<|>)
-}

import Lazy as L
import Regex exposing (Regex(..))
import String


{-| The contex over which `Parser`s operate. -}
type alias Context =
  { input : String
  , position : Int
  }


{-| Running a `Parser` results in one of two states:

* `Ok` when the parser has successfully parsed the input (or part of
it) to a result, or
* `Err` when the parser has failed with a list of error messages.
-}
type alias Result res =
  Result.Result (List String) res


type alias ParseFn res =
  Context -> (Result res, Context)


{-| The Parser type.

At their core, `Parser`s simply wrap functions from a `Context` to a
tuple of a `Result res` and a new `Context`. -}
type Parser res
  = Parser (ParseFn res)
  | RecursiveParser (L.Lazy (ParseFn res))


{-| Construct a new primitive Parser.

If you find yourself reaching for this function often consider opening
a [Github issue][issues] with the library to have your custom Parsers
included in the standard distribution.

[issues]: https://github.com/Bogdanp/elm-combine/issues
-}
primitive : (Context -> (Result res, Context)) -> Parser res
primitive = Parser


{-| Unwrap a parser so it can be applied to a context. -}
app : Parser res -> Context -> (Result res, Context)
app p =
  case p of
    Parser p ->
      p

    RecursiveParser t ->
      L.force t


{-| Parse a string.

    import Combine.Num exposing (int)

    parse int "123" ==
      (Ok 123, { input = "", position = 3 })

    parse int "abc" ==
      (Err ["expected an integer"], { input = "abc", position = 0 })

 -}
parse : Parser res -> String -> (Result res, Context)
parse p input = app p { input = input, position = 0 }


{-| Avoid running a parser until it's actually required.  Use this
function to avoid "bad-recursion" errors.

    type Expression
      = ETerm String
      | EList (List E)

    whitespace : Parser String
    whitespace = regex "[ \t\r\n]*"

    name : Parser String
    name = whitespace *> regex "[a-zA-Z]+" <* whitespace

    term : Parser Expression
    term = ETerm <$> name

    list : Parser Expression
    list =
      let
        -- helper is itself a function so we avoid the case where the
        -- value `list` calls itself prematurely.
        helper () =
          EList <$> between (string "(") (string ")") (many (term <|> list))
      in
        -- rec defers calling helper until it's actually needed.
        rec helper

    parse list "" ==
      (Err ["expected \"(\""], { input = "", position = 0 })

    parse list "()" ==
      (Ok (EList []), { input = "", position = 2 })

    parse list "(a (b c))" ==
      (Ok (EList [ETerm "a", EList [ETerm "b", ETerm "c"]]), { input = "", position = 9 })

-}
rec : (() -> Parser a) -> Parser a
rec t = RecursiveParser (L.lazy (\() -> app (t ())))


{-| Transform both the result and error message of a parser. -}
bimap : (a -> b)
      -> (List String -> List String)
      -> Parser a
      -> Parser b
bimap fok ferr p =
  Parser <| \cx ->
    case app p cx of
      (Ok r, rcx) ->
        (Ok (fok r), rcx)

      (Err m, ecx) ->
        (Err (ferr m), ecx)


{-| Transform the result of a parser.

    let
      parser =
        string "a"
          |> map String.toUpper
    in
      parse parser "a" ==
        (Ok "A", { input = "", position = 1 })

-}
map : (a -> b) -> Parser a -> Parser b
map f p = bimap f identity p


{-| Transform the error of a parser.

    let
      parser =
        string "a"
          |> mapError (always ["bad input"])
    in
      parse parser b ==
        (Err ["bad input"], { input = "b", position = 0 })

-}
mapError : (List String -> List String) -> Parser res -> Parser res
mapError = bimap identity


{-| Sequence two parsers, passing the result of the first parser to a
function that returns the second parser. The value of the second
parser is returned on success.

    import Combine.Num exposing (int)

    choosy : Parser String
    choosy =
      let
        createParser n =
          if n % 2 == 0 then
            string " is even"
          else
            string " is odd"
      in
        int
          |> andThen createParser

    parse choosy "1 is odd" ==
      (Ok " is odd", { input = "", position = 8 })

    parse choosy "1 is even" ==
      (Err ["expected \" is odd\""], { input = " is even", position = 1 })

    parse choosy "2 is even" ==
      (Ok " is even", { input = "", position = 9 })

-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen f p =
  Parser <| \cx ->
    case app p cx of
      (Ok res, rcx) ->
        app (f res) rcx

      (Err m, ecx) ->
        (Err m, ecx)


{-| Sequence two parsers.

    import Maybe
    import Result
    import String

    num : Parser Int
    num = (Maybe.withDefault 0 << Result.toMaybe << String.toInt) `map` regex "[0-9]+"

    sum : Parser Int
    sum = (+) `map` (num <* string "+") `andMap` num

    parse sum "1+2" ==
      (Ok 3, { input = "", position = 3 })

-}
andMap : Parser a -> Parser (a -> b) -> Parser b
andMap rp lp = lp >>= flip map rp


{-| Run a list of parsers in sequence, accumulating the results.  The
main use case for this parser is when you want to combine a list of
parsers into a single, top-level, parser.  For most use cases, you'll
want to use one of the other combinators instead.

    parse (sequence [string "a", string "b"]) "ab" ==
      (Ok ["a", "b"], { input = "", position = 2 })

    parse (sequence [string "a", string "b"]) "ac" ==
      (Err ["expected \"b\"", { input = "c", position = 1 })

 -}
sequence : List (Parser a) -> Parser (List a)
sequence ps =
  let
    accumulate acc ps cx =
      case ps of
        [] ->
          (Ok (List.reverse acc), cx)

        p::ps ->
          case app p cx of
            (Ok res, rcx) ->
              accumulate (res :: acc) ps rcx

            (Err ms, ecx) ->
              (Err ms, ecx)
  in
    Parser <| \cx ->
      accumulate [] ps cx


{-| Fail without consuming any input.

    parse (fail "some error") "hello" ==
      (Err ["some error"], { input = "a", position = 0 })

-}
fail : String -> Parser a
fail m =
  Parser <| \cx ->
    (Err [m], cx)


emptyErr : Parser a
emptyErr =
  Parser <| \cx ->
    (Err [], cx)


{-| Return a value without consuming any input.

    parse (succeed 1) "a" ==
      (Ok 1, { input = "a", position = 0 })

-}
succeed : a -> Parser a
succeed r =
  Parser <| \cx ->
    (Ok r, cx)


{-| Parse an exact string match.

    parse (string "hello") "hello world" ==
      (Ok "hello", { input = " world", position = 5 })

    parse (string "hello") "goodbye" ==
      (Err ["expected \"hello\""], { input = "goodbye", position = 0 })

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
      in (Ok s, {cx | input = rem, position = pos})
    else (Err ["expected " ++ (toString s)], cx)


{-| Parse a Regex match.

Regular expressions must match from the beginning of the input and their
subgroups are ignored. A `^` is added implicitly to the beginning of
every pattern unless one already exists.

    parse (regex "a+") "aaaaab" ==
      (Ok "aaaaa", { input = "b", position = 5 })

-}
regex : String -> Parser String
regex pat =
  let
    pattern =
      if String.startsWith "^" pat
      then pat
      else "^" ++ pat
  in
    Parser <| \cx ->
      case Regex.find (Regex.AtMost 1) (Regex.regex pattern) cx.input of
        [match] ->
          let
            len = String.length match.match
            rem = String.dropLeft len cx.input
            pos = cx.position + len
          in (Ok match.match, {cx | input = rem, position = pos })
        _ ->
          (Err ["expected input matching Regexp /" ++ pattern ++ "/"], cx)


{-| Consume input while the predicate matches.

    parse (while ((/=) ' ')) "test 123" ==
      (Ok "test", { input = " 123", position = 4 })

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
      let
        (res, rcx) = accumulate "" cx
      in
        (Ok res, rcx)


{-| Fail when the input is not empty.

    parse end "" == (Ok (), { input = "", position = 0 })
    parse end "a" == (Err ["expected end of input"], { input = "a", position = 0 })

-}
end : Parser ()
end =
  Parser <| \cx ->
    if cx.input == ""
    then (Ok (), cx)
    else (Err ["expected end of input"], cx)


{-| Choose between two parsers.

    parse (string "a" `or` string "b") "a" ==
      (Ok "a", { input = "", position = 1 })

    parse (string "a" `or` string "b") "b" ==
      (Ok "b", { input = "", position = 1 })

    parse (string "a" `or` string "b") "c" ==
      (Err ["expected \"a\"", "expected \"b\""], { input = "c", position = 0 })

-}
or : Parser a -> Parser a -> Parser a
or lp rp =
  Parser <| \cx ->
    let res = app lp cx in
    case res of
      (Ok _, _) ->
        res

      (Err lm, _) ->
        let rres = app rp cx in
        case rres of
          (Ok _, _) ->
            rres

          (Err rm, _) ->
            (Err (lm ++ rm), cx)


{-| Choose between a list of parsers.

    parse (choice [string "a", string "b"]) "a" ==
      (Ok "a", { input = "", position = 1 })

    parse (choice [string "a", string "b"]) "b" ==
      (Ok "b", { input = "", position = 1 })
-}
choice : List (Parser a) -> Parser a
choice xs =
  List.foldr or emptyErr xs


{-| Return a default value when the given parser fails.

    letterA : Parser String
    letterA = optional "a" (string "a")

    parse letterA "a" == (Ok "a", { input = "", position = 1 })
    parse letterA "b" == (Ok "a", { input = "b", position = 0 })

-}
optional : a -> Parser a -> Parser a
optional res p =
  p <|> succeed res


{-| Wrap the return value into a `Maybe`. Returns `Nothing` on failure.

    parse (maybe (string "a")) "a" ==
      (Ok (Just "a"), { input = "", position = 1 })

    parse (maybe (string "a")) "b" ==
      (Ok Nothing, { input = "b", position = 0 })

-}
maybe : Parser a -> Parser (Maybe a)
maybe p =
  Parser <| \cx ->
    case app p cx of
      (Ok res, rcx) ->
        (Ok (Just res), rcx)

      _ ->
        (Ok Nothing, cx)


{-| Apply a parser until it fails and return a list of the results.

    parse (many (string "a")) "aaab" ==
      (Ok ["a", "a", "a"], { input = "b", position = 3 })

    parse (many (string "a")) "" ==
      (Ok [], { input = "", position = 0 })

-}
many : Parser a -> Parser (List a)
many p =
  let
    accumulate acc cx =
      case app p cx of
        (Ok res, rcx) ->
          if cx == rcx then
            (List.reverse acc, cx)
          else
            accumulate (res :: acc) rcx

        _ ->
          (List.reverse acc, cx)
  in
    Parser <| \cx ->
      let
        (res, rcx) = accumulate [] cx
      in
        (Ok res, rcx)


{-| Parse at least one result.

    parse (many1 (string "a")) "a" ==
      (Ok ["a"], { input = "", position = 1 })

    parse (many1 (string "a")) "" ==
      (Err ["expected \"a\""], { input = "", position = 0 })

-}
many1 : Parser a -> Parser (List a)
many1 p =
  (::) <$> p <*> many p


{-| Apply parser `p` zero or more times until parser `end`
succeeds. On success, the list of `p`'s results is returned.

    string "<!--" *> manyTill anyChar (string "-->")

-}
manyTill : Parser a -> Parser end -> Parser (List a)
manyTill p end =
  let
    accumulate acc cx =
      case app end cx of
        (Ok _, ecx) ->
          (Ok (List.reverse acc), ecx)

        (Err err, ecx) ->
          case app p cx of
            (Ok res, pcx) ->
              accumulate (res :: acc) pcx

            _ ->
              (Err err, ecx)
  in
    Parser <| accumulate []


{-| Parser zero or more occurences of one parser separated by another.

    parse (sepBy (string ",") (string "a")) "b" ==
      (Ok [], { input = "b", position = 0 })

    parse (sepBy (string ",") (string "a")) "a,a,a" ==
      (Ok ["a", "a", "a"], { input = "", position = 5 })

    parse (sepBy (string ",") (string "a")) "a,a,b" ==
      (Ok ["a", "a"], { input = ",b", position = 3 })

-}
sepBy : Parser x -> Parser a -> Parser (List a)
sepBy sep p =
  sepBy1 sep p <|> succeed []


{-| Parse one or more occurences of one parser separated by another. -}
sepBy1 : Parser x -> Parser a -> Parser (List a)
sepBy1 sep p =
  (::) <$> p <*> many (sep *> p)


{-| Parse zero or more occurences of one parser separated and
optionally ended by another.

    parse (sepEndBy (string ",") (string "a")) "a,a,a," ==
      (Ok ["a", "a", "a"], { input = "", position = 6 })

-}
sepEndBy : Parser x -> Parser a -> Parser (List a)
sepEndBy sep p =
  sepEndBy1 sep p <|> succeed []


{-| Parse one or more occurences of one parser separated and
optionally ended by another.

    parse (sepEndBy1 (string ",") (string "a")) "" ==
      (Err ["expected \"a\""], { input = "", position = 0 })

    parse (sepEndBy1 (string ",") (string "a")) "a" ==
      (Ok ["a"], { input = "", position = 1 })

    parse (sepEndBy1 (string ",") (string "a")) "a," ==
      (Ok ["a"], { input = "", position = 2 })

-}
sepEndBy1 : Parser x -> Parser a -> Parser (List a)
sepEndBy1 sep p =
  sepBy1 sep p <* maybe sep


{-| Apply a parser and skip its result. -}
skip : Parser x -> Parser ()
skip p = () <$ p


{-| Apply a parser and skip its result many times. -}
skipMany : Parser x -> Parser ()
skipMany p = () <$ many (skip p)


{-| Apply a parser and skip its result at least once. -}
skipMany1 : Parser x -> Parser ()
skipMany1 p = () <$ many1 (skip p)


{-| Parse one or more occurences of `p` separated by `op`, recursively
apply all functions returned by `op` to the values returned by `p`. See
the `examples/Calc.elm` file for an example.
-}
chainl : Parser (a -> a -> a) -> Parser a -> Parser a
chainl op p =
  let
    accumulate x =
      (op
        |> andThen (\f -> p
        |> andThen (\y -> accumulate (f x y)))) <|> succeed x
  in
    andThen accumulate p


{-| Similar to `chainl` but functions of `op` are applied in
right-associative order to the values of `p`. -}
chainr : Parser (a -> a -> a) -> Parser a -> Parser a
chainr op p =
  let
    accumulate x =
      (op
        |> andThen (\f -> p
        |> andThen accumulate
        |> andThen (\y -> succeed (f x y)))) <|> succeed x
  in
    andThen accumulate p


{-| Parse `n` occurences of `p`. -}
count : Int -> Parser a -> Parser (List a)
count n p =
  let
    accumulate x acc =
      if x <= 0
      then succeed (List.reverse acc)
      else andThen (\res -> accumulate (x - 1) (res :: acc)) p
  in
    accumulate n []


{-| Parse something between two other parsers.

The parser

    between (string "(") (string ")") (string "a")

is equivalent to the parser

    string "(" *> string "a" <* string ")"
-}
between : Parser l -> Parser r -> Parser a -> Parser a
between lp rp p = lp *> p <* rp


{-| Parse something between parentheses. -}
parens : Parser a -> Parser a
parens = between (string "(") (string ")")


{-| Parse something between braces `{}`. -}
braces : Parser a -> Parser a
braces = between (string "{") (string "}")


{-| Parse something between square brackets `[]`. -}
brackets : Parser a -> Parser a
brackets = between (string "[") (string "]")


{-| Parse zero or more whitespace characters. -}
whitespace : Parser String
whitespace = regex "[ \t\r\n]*" <?> "whitespace"


-- Infix operators
-- ---------------
{-| Variant of `Combine.mapError` that replaces the Parser's error
with a List of a single string.

    parse (string "a" <?> "gimme an 'a'") "b" ==
      (Err ["gimme an 'a'"], { input = "b", position = 0 })

-}
(<?>) : Parser a -> String -> Parser a
(<?>) p m = mapError (always [m]) p


{-| Flipped synonym for `andThen`. -}
(>>=) : Parser a -> (a -> Parser b) -> Parser b
(>>=) = flip andThen


{-| Synonym for `map`. -}
(<$>) : (a -> b) -> Parser a -> Parser b
(<$>) = map


{-| Variant of `map` that ignores the Parser's result. -}
(<$) : a -> Parser x -> Parser a
(<$) res = map (\_ -> res)


{-| Variant of `map` that ignores the Parser's result. -}
($>) : Parser x -> a -> Parser a
($>) = flip (<$)


{-| Flipped synonym for `andMap`. -}
(<*>) : Parser (a -> b) -> Parser a -> Parser b
(<*>) = flip andMap


{-| Join two parsers, ignoring the result of the one on the right.

    unsuffix : Parser String
    unsuffix =
      regex "[a-z]"
        <* regex "[!?]"

    parse unsuffix "a!" ==
      (Ok "a", { input = "", position = 2 })

-}
(<*) : Parser a -> Parser x -> Parser a
(<*) lp rp =
  lp
    |> map always
    |> andMap rp


{-| Join two parsers, ignoring the result of the one on the left.

    unprefix : Parser String
    unprefix =
      string ">"
        *> while ((==) ' ')
        *> while ((/=) ' ')

    parse unprefix "> a" ==
      (Ok "a", { input = "", position = 3 })

-}
(*>) : Parser x -> Parser a -> Parser a
(*>) lp rp =
  lp
    |> map (flip always)
    |> andMap rp


{-| Synonym for `or`. -}
(<|>) : Parser a -> Parser a -> Parser a
(<|>) = or


-- Fixities
infix  0 <?>
infixl 1 >>=
infixr 1 <|>
infixl 4 <$>
infixl 4 <$
infixl 4 $>
infixl 4 <*>
infixl 4 <*
infixl 4 *>
