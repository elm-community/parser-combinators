# Changelog


## Version 1.0.0 (2017-02-09)

* Transfer from `Bogdanp/elm-combine`.

---

> This repository is transferred from [Bogdanp/elm-combine](github.com/Bogdanp/elm-combine). The following changelog statements originate from that repository.


---
## elm-combine 3.1.0 (2016-11-14)

### Additions

* Brought back `app`

## elm-combine 3.0.0 (2016-11-14)

### Breaking changes

* The `Combine.Infix` module has been merged into `Combine`
* The `Parser` type has changed from `Parser res` to `Parser state res`
* The signature of `andThen` has changed to `(a -> Parser s b) -> Parser s a -> Parser s b`
* The signature of `andMap` has changed to `Parser s a -> Parser s (a -> b) -> Parser s b`
* The signature of `chainl` has changed to `Parser s (a -> a -> a) -> Parser s a -> Parser s a`
* The signature of `chainr` has changed to `Parser s (a -> a -> a) -> Parser s a -> Parser s a`
* The signature of `parse` has changed to `Parser () res -> String -> Result (ParseErr ()) (ParseOk () res)`
* The signature of `fail` has changed to `String -> Parser s a`
* `rec` has been renamed to `lazy`
* `app` has been removed, use `primitive`, `parse` or `runParser` instead
* `bimap` has been removed, use `map` and `mapError` instead

### Additions

* Added `InputStream`, `ParseLocation`, `ParseContext`, `ParseResult`, `ParseErr` and `ParseOk` types
* Added `runParser`, `withState`, `putState`, `modifyState`
* Added `withLocation`, `withLine`, `withColumn`, `currentLocation`, `currentSourceLine`, `currentLine`, `currentColumn`
* Added `lookAhead` and `whitespace` parsers

### Upgrading from 2.2.1

* Replace all occurrences of `Parser *` with `Parser s *`
* Replace all infix occurrences of andThen with `a |> andThen b`
* Replace all imports of `Combine.Infix` with `Combine`
* Replace all pattern matches on `Combine.parse` like so:

``` elm
case Combine.parse someParser inputData of
  (Ok result, context) ->
    Just result

  (Err errors, context) ->
    Nothing
```

becomes

``` elm
case Combine.parse someParser inputData of
  Ok (state, stream, result) ->
    Just result

  Err (state, stream, errors) ->
    Nothing
```

## elm-combine 2.2.1 (2016-05-11)

### Additions

* Added support for Elm 0.17

## elm-combine 2.2.0 (2016-03-06)

### Additions

* Added `sequence`

## elm-combine 2.1.0 (2016-02-28)

### Additions

* Added `sepEndBy` and `sepEndBy1` (contributed by @prt2121)

## elm-combine 2.0.2

### Fixes

* Fixed issue [10](https://github.com/Bogdanp/elm-combine/issues/10).
* Fixed an issue with `mapError` where the incorrect context was returned.

## elm-combine 2.0.1

### Fixes

* Fixed issue [8](https://github.com/Bogdanp/elm-combine/issues/8).

## elm-combine 2.0.0

### Changes

* Replaced custom `Result` ADT with `Result.Result` from `core`
* Renamed `brackets` to `braces`
* Renamed `squareBrackets` to `brackets`
* Removed `Parser` and `RecursiveParser` constructors
* Added `primitive` function for construcing custom `Parser` instances
* Added basic test suite
* Added Travis CI
* Updated documentation

### Upgrading from 1.2.0

* Replace all occurrences of `Done` with `Ok`
* Replace all occurrences of `Fail` with `Err`
* Replace all occurrences of `Result a` with `Result (List String) a`
* Replace all occurrences of `ParseFn a` with `Context -> (Result a, Context)`
* Replace all calls to the `Parser` constructor with `primitive`
