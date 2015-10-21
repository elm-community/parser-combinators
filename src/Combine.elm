module Combine where

import Lazy as L
import String
import Regex exposing (Regex(..))


type alias Context =
  { input : String
  }


type Result res
  = Done res
  | Fail String


type alias ParseFn res =
  Context -> (Result res, Context)


type Parser res
  = Parser (ParseFn res)
  | RecursiveParser (L.Lazy (ParseFn res))


app : Parser res -> (Context -> (Result res, Context))
app p =
  case p of
    Parser p ->
      p

    RecursiveParser t ->
      L.force t


{-| Parse a string. -}
parse : Parser res -> String -> (Result res, Context)
parse p input = app p { input = input }


{-| Defer a parser's evaluation.

    type E = ETerm String | EList (List E)

    whitespace = regex "[ \t\r\n]*"
    term = ETerm `map` (whitespace *> regex "[a-zA-Z]+" <* whitespace)
    list = rec (\() -> EList `map` (string "(" *> many (term `or` list) <* string ")"))

    parse list "" == \
      (Fail "expected '('", { input = "" })

    parse list "()" == \
      (Done (EList []), { input = "" })

    parse list "(a (b c))" == \
      (Done (EList [ETerm "a", EList [ETerm "b", ETerm "c"]]), { input = "" })
 -}
rec : (() -> Parser res) -> Parser res
rec t =
  RecursiveParser << L.lazy <| \() -> app (t ())


{-| Transform both the result and error message of a parser.
 -}
bimap : (res -> res')
      -> (String -> String)
      -> Parser res
      -> Parser res'
bimap fok ferr p =
  Parser <| \c ->
    case app p c of
      (Done r, c) ->
        (Done (fok r), c)

      (Fail m, c) ->
        (Fail (ferr m), c)


{-| Transform the result of a parser.

    parse (map String.toUpper (string "a")) "a" == \
      (Done "A", { input = "" })
 -}
map : (res -> res') -> Parser res -> Parser res'
map f p = bimap f identity p


{-| Sequence two parsers by passing in the results of the first parser
to the second.
 -}
andThen : Parser res -> (res -> Parser res') -> Parser res'
andThen p f =
  Parser <| \c ->
    case app p c of
      (Done res, c) ->
        app (f res) c

      (Fail m, c) ->
        (Fail m, c)


{-| Sequence two parsers.
 -}
andMap : Parser (res -> res') -> Parser res -> Parser res'
andMap lp rp =
  lp
    `andThen` \f -> rp
    `andThen` \x -> succeed (f x)


{-| Choose between two parsers.

    parse (string "a" `or` string "b") "a" == \
      (Done "a", { input = "" })

    parse (string "a" `or` string "b") "b" == \
      (Done "b", { input = "" })

    parse (string "a" `or` string "b") "c" == \
      (Fail "expected 'a' or expected 'b'", { input = "c" })
 -}
or : Parser res -> Parser res -> Parser res
or lp rp =
  Parser <| \c ->
    let res = app lp c in
    case res of
      (Done _, _) ->
        res

      (Fail lm, _) ->
        -- XXX: res' to avoid dynamic scoping issue in compiled JS.
        let res' = app rp c in
        case res' of
          (Done _, _) ->
            res'

          (Fail rm, _) ->
            (Fail (lm ++ " or " ++ rm), c)


{-| Choose between a list of parsers.

    parse (choice [string "a", string "b"]) "a" == \
      (Done "a", { input = "" })

    parse (choice [string "a", string "b"]) "b" == \
      (Done "b", { input = "" })
 -}
choice : List (Parser res) -> Parser res
choice xs =
  List.foldr or (fail "choice") xs


{-| Apply a parser until it fails and return a list of the results.

    parse (many (string "a")) "aaab" == \
      (Done ["a", "a", "a"], { input = "b" })

    parse (many (string "a")) "" == \
      (Done [], { input = "" })
 -}
many : Parser res -> Parser (List res)
many p =
  let
    accumulate acc c =
      case app p c of
        (Done res, c) ->
          accumulate (res :: acc) c

        _ ->
          (List.reverse acc, c)
  in
    Parser <| \cx ->
      let (res, c) = accumulate [] cx in
      (Done res, c)


{-| Parse at least one result.

    parse (many1 (string "a")) "a" == \
      (Done ["a"], { input = "" })

    parse (many1 (string "a")) "" == \
      (Fail "expected 'a'", { input = "" })
 -}
many1 : Parser res -> Parser (List res)
many1 p =
  (::) `map` p `andMap` many p


{-| Fail without consuming any input. -}
fail : String -> Parser res
fail m =
  Parser <| \c ->
    (Fail m, c)


{-| Return a value without consuming any input.

    parse (succeed 1) "a" == \
      (Done 1, { input = "a" })
 -}
succeed : res -> Parser res
succeed r =
  Parser <| \c ->
    (Done r, c)


{-| Parse an exact string match.

    parse (string "hello") "hello world" == \
      (Done "hello", { input = " world" })

    parse (string "hello") "goodbye" == \
      (Fail "expected 'hello'", { input = "goodbye" })
 -}
string : String -> Parser String
string s =
  Parser <| \c ->
    if String.startsWith s c.input
    then (Done s, {c | input <- String.dropLeft (String.length s) c.input})
    else (Fail ("expected '" ++ s ++ "'"), c)


{-| Parse a Regex match.

Regular expressions must match from the beginning of the input and their
subgroups are ignored. A `^` is added implicitly to the beginning of
every pattern unless one already exists.

    parse (regex "a+") "aaaaab" == \
      (Done "aaaaa", { input = "b" })
 -}
regex : String -> Parser String
regex pattern =
  let
    pattern' =
      if String.startsWith "^" pattern
      then pattern
      else "^" ++ pattern
  in
  Parser <| \c ->
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern') c.input of
      [match] ->
        let rem = String.dropLeft (String.length match.match) c.input
        in (Done match.match, {c | input <- rem })

      _ ->
        (Fail ("expected input matching Regexp /" ++ pattern' ++ "/"), c)


{-| Consume input while the predicate matches.

    parse (while ((/=) ' ')) "test 123" == \
      (Done "test", { input = " 123" })
 -}
while : (Char -> Bool) -> Parser String
while p =
  let
    accumulate acc c =
      case String.uncons c.input of
        Just (h, rest) ->
          if p h
          then accumulate (acc ++ (String.left 1 c.input)) { c | input <- rest }
          else (acc, c)

        Nothing ->
          (acc, c)
  in
    Parser <| \cx ->
      let (res, c) = accumulate "" cx in
      (Done res, c)


{-| Fail when the input is not empty.

    parse end "" == (Done (), { input = "" })
    parse end "a" == (Fail "expected end of input", { input = "a" })
 -}
end : Parser ()
end =
  Parser <| \c ->
    if c.input == ""
    then (Done (), c)
    else (Fail "expected end of input", c)


{-| Return a default value when the given parser fails.

    letterA : Parser String
    letterA = optional (string "a") "a"

    parse letterA "a" == (Done "a", { input = "" })
    parse letterA "b" == (Done "a", { input = "b" })
 -}
optional : Parser res -> res -> Parser res
optional p res =
  p `or` succeed res


{-| Join two parsers, ignoring the result of the one on the right.

    unsuffix : Parser String
    unsuffix = regex "[a-z]" <* regex "[!?]"

    parse unsuffix "a!" == (Done "a", { input = "" })
 -}
(<*) : Parser res -> Parser x -> Parser res
(<*) lp rp =
  always `map` lp `andMap` rp


{-| Join two parsers, ignoring the result of the one on the left.

    unprefix : Parser String
    unprefix = string ">" *> while ((==) ' ') *> while ((/=) ' ')

    parse unprefix "> a" == (Done "a", { input = "" })
 -}
(*>) : Parser x -> Parser res -> Parser res
(*>) lp rp =
  (flip always) `map` lp `andMap` rp
