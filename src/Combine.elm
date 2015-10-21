module Combine where

import Lazy as L
import String
import Regex exposing (Regex(..))


type alias Context =
  { input : String
  , position : Int
  }


type Result res
  = Done res
  | Fail String


type alias ParseFn res =
  Context -> (Result res, Context)


type Parser res
  = Parser (ParseFn res)
  | RecursiveParser (L.Lazy (ParseFn res))


{-| Unwrap a parser so it can be applied to a context. -}
app : Parser res -> (Context -> (Result res, Context))
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

    parse list "" == \
      (Fail "expected '('", { input = "", position = 0 })

    parse list "()" == \
      (Done (EList []), { input = "", position = 2 })

    parse list "(a (b c))" == \
      (Done (EList [ETerm "a", EList [ETerm "b", ETerm "c"]]) \
      , { input = "", position = 9 })
 -}
rec : (() -> Parser res) -> Parser res
rec t =
  RecursiveParser << L.lazy <| \() -> app (t ())


{-| Transform both the result and error message of a parser. -}
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
      (Done "A", { input = "", position = 1 })
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

    import Maybe
    import Result
    import String

    num : Parser Int
    num = (Maybe.withDefault 0 << Result.toMaybe << String.toInt) `map` regex "[0-9]+"

    sum : Parser Int
    sum = (+) `map` (num <* string "+") `andMap` num

    parse sum "1+2" == \
      (Done 3, { input = "", position = 3 })
 -}
andMap : Parser (res -> res') -> Parser res -> Parser res'
andMap lp rp =
  lp
    `andThen` \f -> rp
    `andThen` \x -> succeed (f x)


{-| Choose between two parsers.

    parse (string "a" `or` string "b") "a" == \
      (Done "a", { input = "", position = 1 })

    parse (string "a" `or` string "b") "b" == \
      (Done "b", { input = "", position = 1 })

    parse (string "a" `or` string "b") "c" == \
      (Fail "expected 'a' or expected 'b'", { input = "c", position = 0 })
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
      (Done "a", { input = "", position = 1 })

    parse (choice [string "a", string "b"]) "b" == \
      (Done "b", { input = "", position = 1 })
 -}
choice : List (Parser res) -> Parser res
choice xs =
  List.foldr or (fail "choice") xs


{-| Apply a parser until it fails and return a list of the results.

    parse (many (string "a")) "aaab" == \
      (Done ["a", "a", "a"], { input = "b", position = 3 })

    parse (many (string "a")) "" == \
      (Done [], { input = "", position = 0 })
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
      (Done ["a"], { input = "", position = 1 })

    parse (many1 (string "a")) "" == \
      (Fail "expected 'a'", { input = "", position = 0 })
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
      (Done 1, { input = "a", position = 0 })
 -}
succeed : res -> Parser res
succeed r =
  Parser <| \c ->
    (Done r, c)


{-| Parse an exact string match.

    parse (string "hello") "hello world" == \
      (Done "hello", { input = " world", position = 5 })

    parse (string "hello") "goodbye" == \
      (Fail "expected 'hello'", { input = "goodbye", position = 0 })
 -}
string : String -> Parser String
string s =
  Parser <| \c ->
    if String.startsWith s c.input
    then
      let
        len = String.length s
        rem = String.dropLeft len c.input
        pos = c.position + len
      in (Done s, {c | input <- rem, position <- pos})
    else (Fail ("expected '" ++ s ++ "'"), c)


{-| Parse a Regex match.

Regular expressions must match from the beginning of the input and their
subgroups are ignored. A `^` is added implicitly to the beginning of
every pattern unless one already exists.

    parse (regex "a+") "aaaaab" == \
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
  Parser <| \c ->
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern') c.input of
      [match] ->
        let
          len = String.length match.match
          rem = String.dropLeft len c.input
          pos = c.position + len
        in (Done match.match, {c | input <- rem, position <- pos })

      _ ->
        (Fail ("expected input matching Regexp /" ++ pattern' ++ "/"), c)


{-| Consume input while the predicate matches.

    parse (while ((/=) ' ')) "test 123" == \
      (Done "test", { input = " 123", position = 4 })
 -}
while : (Char -> Bool) -> Parser String
while pred =
  let
    accumulate acc c =
      case String.uncons c.input of
        Just (h, rest) ->
          if pred h
          then
            let
              char = String.cons h ""
              pos = c.position + 1
            in accumulate (acc ++ char) { c | input <- rest, position <- pos }
          else (acc, c)

        Nothing ->
          (acc, c)
  in
    Parser <| \cx ->
      let (res, c) = accumulate "" cx in
      (Done res, c)


{-| Fail when the input is not empty.

    parse end "" == (Done (), { input = "", position = 0 })
    parse end "a" == (Fail "expected end of input", { input = "a", position = 0 })
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

    parse letterA "a" == (Done "a", { input = "", position = 1 })
    parse letterA "b" == (Done "a", { input = "b", position = 0 })
 -}
optional : Parser res -> res -> Parser res
optional p res =
  p `or` succeed res


{-| Join two parsers, ignoring the result of the one on the right.

    unsuffix : Parser String
    unsuffix = regex "[a-z]" <* regex "[!?]"

    parse unsuffix "a!" == (Done "a", { input = "", position = 2 })
 -}
(<*) : Parser res -> Parser x -> Parser res
(<*) lp rp =
  always `map` lp `andMap` rp


{-| Join two parsers, ignoring the result of the one on the left.

    unprefix : Parser String
    unprefix = string ">" *> while ((==) ' ') *> while ((/=) ' ')

    parse unprefix "> a" == (Done "a", { input = "", position = 3 })
 -}
(*>) : Parser x -> Parser res -> Parser res
(*>) lp rp =
  (flip always) `map` lp `andMap` rp
