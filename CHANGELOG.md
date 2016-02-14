# elm-combine changelog

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
