module Combine where

import String
import Regex exposing (Regex(..))

type alias Context =
  { input : String
  }

type Result res = Done res
                | Fail String

type Parser res =
  Parser (Context -> (Result res, Context))

app : Parser res -> (Context -> (Result res, Context))
app p =
  case p of
    Parser p -> p

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

map : (res -> res') -> Parser res -> Parser res'
map f p = bimap f identity p

andMap : Parser res -> Parser (res -> res') -> Parser res'
andMap rp lp =
  Parser <| \c ->
    case app lp c of
      (Done f, c) ->
        app (map f rp) c

      (Fail m, c) ->
        (Fail m, c)

andThen : Parser res -> (res -> Parser res') -> Parser res'
andThen p f =
  Parser <| \c ->
    case app p c of
      (Done res, c) ->
        app (f res) c

      (Fail m, c) ->
        (Fail m, c)

and : Parser (res -> res') -> Parser res -> Parser res'
and lp rp =
  andMap rp lp

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

choice : List (Parser res) -> Parser res
choice xs =
  List.foldr or (fail "choice") xs

many : Parser res -> Parser (List res)
many p =
  let
    accumulate acc c =
      case app p c of
        (Done res, c) ->
          accumulate (res :: acc) c

        (Fail _, _) ->
          (List.reverse acc, c)
  in
    Parser <| \cx ->
      let (res, c) = accumulate [] cx in
      (Done res, c)

many1 : Parser res -> Parser (List res)
many1 p =
  map (::) p `and` many p

parse : Parser res -> String -> (Result res, Context)
parse p input = app p { input = input }

fail : String -> Parser res
fail m =
  Parser <| \c ->
    (Fail m, c)

succeed : res -> Parser res
succeed r =
  Parser <| \c ->
    (Done r, c)

string : String -> Parser String
string s =
  Parser <| \c ->
    if String.startsWith s c.input
    then (Done s, {c | input <- String.dropLeft (String.length s) c.input })
    else (Fail ("expected '" ++ s ++ "'"), c)

regex : String -> Parser String
regex pattern =
  Parser <| \c ->
    case Regex.find (Regex.AtMost 1) (Regex.regex pattern) c.input of
      [match] ->
        let rem = String.dropLeft (match.index + (String.length match.match)) c.input
        in (Done match.match, {c | input <- rem })

      _ ->
        (Fail ("expected pattern matching /" ++ pattern ++ "/"), c)

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

end : Parser ()
end =
  Parser <| \c ->
    if c.input == ""
    then (Done (), c)
    else (Fail "expected end of input", c)

optional : Parser res -> res -> Parser res
optional p res =
  p `or` succeed res

{-| Join two parsers, ignoring the result of the one on the right.

    unsuffix : Parser String
    unsuffix = regex "[a-z]" <* regex "[!?]"

    -- parse unsuffix "a!" == (Done "a", { input = "" })
 -}
(<*) : Parser res -> Parser x -> Parser res
(<*) lp rp =
  map always lp `and` rp

{-| Join two parsers, ignoring the result of the one on the left.

    unprefix : Parser String
    unprefix = string ">" *> while ((==) ' ') *> while ((/=) ' ')

    -- parse unprefix "> a" == (Done "a", { input = "" })
 -}
(*>) : Parser x -> Parser res -> Parser res
(*>) lp rp =
  map (flip always) lp `and` rp

infixr 3 `or`
infixl 4 `and`
infixl 4 `map`
