module Combine.Infix where

{-| This module provides infix versions of various functions from the
`Combine` module.

# Transforming Parsers
@docs (<$>), (<$), (<?>)

# Chaining Parsers
@docs (<*>), (*>), (<*)

# Parsers
@docs (<|>)
-}

import Combine exposing (..)


{-| Synonym for `Combine.map`. -}
(<$>) : (res -> res') -> Parser res -> Parser res'
(<$>) = map


{-| Synonym for `Combine.andMap`. -}
(<*>) : Parser (res -> res') -> Parser res -> Parser res'
(<*>) = andMap


{-| Variant of `Combine.map` that ignores the Parser's result. -}
(<$) : res -> Parser x -> Parser res
(<$) res = map (\_ -> res)


{-| Variant of `Combine.mapError` that replaces the Parser's error
with a List of a single string.

    parse (string "a" <?> "gimme an 'a'") "b" ==
      (Err ["gimme an 'a'"], { input = "b", position = 0 })
-}
(<?>) : Parser res -> String -> Parser res
(<?>) p m = mapError (\_ -> [m]) p


{-| Join two parsers, ignoring the result of the one on the right.

    unsuffix : Parser String
    unsuffix = regex "[a-z]" <* regex "[!?]"

    parse unsuffix "a!" == (Ok "a", { input = "", position = 2 })
-}
(<*) : Parser res -> Parser x -> Parser res
(<*) lp rp =
  always `map` lp `andMap` rp


{-| Join two parsers, ignoring the result of the one on the left.

    unprefix : Parser String
    unprefix = string ">" *> while ((==) ' ') *> while ((/=) ' ')

    parse unprefix "> a" == (Ok "a", { input = "", position = 3 })
-}
(*>) : Parser x -> Parser res -> Parser res
(*>) lp rp =
  (flip always) `map` lp `andMap` rp


{-| Synonym for `or`. -}
(<|>) : Parser res -> Parser res -> Parser res
(<|>) = or
