# Railroad

[![Build Status](https://github.com/PerplexSystems/railroad/actions/workflows/build.yml/badge.svg)](https://github.com/PerplexSystems/railroad/actions)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)
[![Perplex Systems on libera.chat](https://img.shields.io/badge/libera.chat-%23perplexsystems-blue.svg)](http://webchat.freenode.net/?channels=writeas)

![Railroad logo](./docs/images/logo.png)

An advanced testing library and test runner for Standard ML, highly
inspired by [elm-test](https://github.com/elm-explorations/test) and
[Expecto](https://github.com/haf/expecto).

This library provides a set of composable functions for writing tests,
along with a built-in test runner.

## Installation

```sh
$ git clone https://github.com/PerplexSystems/railroad.git $YOUR_PROJECT/vendor/railroad
```

Then reference `vendor/railroad/lib.mlb` on your project's `.mlb`
file. Example:

```sml
$(SML_LIB)/basis/basis.mlb
vendor/railroad/lib.mlb
main.sml
```

## Usage

```sml
val tests =
  describe "math operations"
    [ test "sum 1 + 1" (fn _ =>
        Expect.equal Int.compare 2 (1 + 1))
    ]

val _ = run tests
```

Check out the table of contents below for more information:

- [Railroad](#railroad)
  - [Installation](#installation)
  - [Usage](#usage)
- [API Reference](#api-reference)
  - [Railroad](#railroad-1)
  - [Railroad.Test](#railroadtest)
    - [concat](#concat)
    - [describe](#describe)
    - [focus](#focus)
    - [run](#run)
    - [runWithConfig](#runwithconfig)
    - [skip](#skip)
    - [test](#test)
  - [Test.Configuration](#testconfiguration)
    - [Setting](#setting)
    - [ExecutionOrder](#executionorder)
  - [Expect](#expect)
    - [actual](#actual)
    - [expected](#expected)
    - [comparer](#comparer)
    - [tostring](#tostring)
    - [pass](#pass)
    - [fail](#fail)
    - [onFail](#onfail)
    - [all](#all)
    - [any](#any)
    - [isTrue](#istrue)
    - [isFalse](#isfalse)
    - [some](#some)
    - [none](#none)
    - [equal](#equal)
    - [equalFmt](#equalfmt)
    - [notEqual](#notequal)
    - [notEqualFmt](#notequalfmt)
    - [atMost](#atmost)
    - [atMostFmt](#atmostfmt)
    - [atLeast](#atleast)
    - [atLeastFmt](#atleastfmt)
    - [less](#less)
    - [lessFmt](#lessfmt)
    - [greater](#greater)
    - [greaterFmt](#greaterfmt)
  - [License](#license)


# API Reference

## Railroad

The [`Railroad`](#railroad) module is the root module of this library.

## Railroad.Test

The [`Railroad.Test`](#railroadtest) module consists of functions that are involved in
creating and managing tests.

### concat

`val concat: Test list -> Test`

Concatenates a list of tests.

```sml
concat [ userTests, baggageTests ]
```

### describe

`val describe: string -> Test list -> Test`

Describes a list of tests.

```sml
describe "math operators"
  [ test "sum" (fn _ =>
      Expect.equal Int.compare 2 (1 + 1))
  , test "failing sum" (fn _ =>
      Expect.equal Int.compare 3 (2 + 3))
  ]
```

### focus

`val focus: Test -> Test`

Returns a [`Railroad.Test`](#railroadtest) that causes other tests to be skipped, and
only runs the given one.

Calls to [`focus`](#focus) aren't meant to be committed to version
control. Instead, use them when you want to focus on getting a
particular subset of your tests to pass. If you use `focus`, your
entire test suite will fail, even if each of the individual tests
pass. This is to help avoid accidentally committing a `focus` to
version control.

If you you use `focus` on multiple tests, only those tests will run.
If you put a `focus` inside another `focus`, only the outermost only
will affect which tests gets run.

See also [`skip`](#skip). Note that `skip` takes precedence over
`focus`; if you use a `skip` inside a `focus`, it will still get
skipped, and if you use a `focus` inside a `skip`, it will also get
skipped.

```sml
describe "math operators"
  [ test "sum" (fn _ =>
      Expect.equal Int.compare 2 (1 + 1))
  , focus (test "this is the only test that will run" (fn _ =>
      Expect.equal Int.compare 3 (2 + 3)))
  ]
```

### run

`val run: Test -> unit`

Runs the provided tests with [default
configuration](#testconfiguration) and exits with success or failure
based on the results.

```sml
run (test "sum" (fn _ => Expect.equal Int.compare 2 (1 + 1)))
```

### runWithConfig

`val runWithConfig: Setting list -> `

Runs the provided tests with the provided [`Setting`](#setting)s,
exits with success or failure based on the tests results.

```sml
val sumTest =
  (test "sum" (fn _ => Expect.equal Int.compare 2 (1 + 1)))

runWithConfig [ Output TextIO.stdOut ] sumTest
```

### skip

`val skip: Test -> Test`

Returns a [`Railroad.Test`](#railroadtest) that gets skipped.

Calls to [`skip`](#skip) aren't meant to be committed to version
control. Instead, use it when you want to focus on getting a
particular subset of your tests to pass. If you use `skip`, your
entire test suite will fail, even if each of the individual tests
pass. This is to help avoid accidentally committing a skip to version
control.

See also [`focus`](#focus). Note that `skip` takes precedence over
`focus`; if you use a `skip` inside a `focus`, it will still get
skipped, and if you use a `focus` inside a `skip`, it will also get
skipped.

```sml
describe "math operators"
  [ test "this test will be the only one to run" (fn _ =>
      Expect.equal Int.compare 2 (1 + 1))
  , skip (test "this test is skipped" (fn _ =>
      Expect.equal Int.compare 3 (2 + 3)))
  ]
```

### test

`val test: string -> (unit -> Expectation) -> Test`

Return a [`Railroad.Test`](#railroadtest) that evaluates a single `Expectation`.

```sml
test "sum" (fn _ => Expect.equal Int.compare 2 (1 + 1))
```

## Test.Configuration

The [`Railroad.Test`](#railroadtest) module consists of types and functions that are involved in
configuring the test runner.

The default [`Setting`](#setting)s are the following:

```sml
{ output = TextIO.stdOut
, printPassed = true
, stopOnFirstFailure = false
, executionOrder = RandomOrder NONE
}
```

### Setting

```sml
datatype Setting =
    Output of TextIO.outstream
  | PrintPassed of bool
  | StopOnFirstFailure of bool
  | ExecutionOrder of ExecutionOrder
```

Represents the possible settings for the runner configuration.

- `Output`: Where the output should be redirected to.
- `PrintPassed`: Whether to print passing tests (default: `true`).
- `StopOnFirstFailure`: Whether to stop running tests after the first failure (default: `false`).
- `ExecutionOrder`: The order in which tests should be executed (default: `RandomOrder NONE`).

### ExecutionOrder

```sml
datatype ExecutionOrder =
    AlphabeticalOrder
  | DeclarationOrder
  | RandomOrder of int option
```

Controls the order in which tests are executed:

- `AlphabeticalOrder`: Runs tests in alphabetical order of their descriptions (full path including parent describes).
- `DeclarationOrder`: Runs tests in the order they were declared in the source code.
- `RandomOrder of int option`: Runs tests in a random order. This is the default. When using `RandomOrder NONE`, a random seed is generated and printed to the output so you can reproduce the run. When using `RandomOrder (SOME seed)`, the provided seed is used for deterministic shuffling.

```sml
(* Run tests in alphabetical order *)
runWithConfig [ ExecutionOrder AlphabeticalOrder ] tests

(* Run tests in random order with auto-generated seed *)
runWithConfig [ ExecutionOrder (RandomOrder NONE) ] tests

(* Run tests in random order with a specific seed for reproducibility *)
runWithConfig [ ExecutionOrder (RandomOrder (SOME 12345)) ] tests
```

When using `RandomOrder`, the seed will be printed at the end of the output:

```
...
=== PASS: Expect.atMost.greater value

Random seed: 1501112078
Passed: 60, failed: 0, skipped: 0, focused: 0
```

## Expect

The [`Expect`](#expect) module consists of assertion functions that
describes a claim to be tested.

### actual

`type 'a actual = 'a`

Represents the actual value passed to an assertion function.

### expected

`type 'a expected = 'a`

Represents the expected value passed to an assertion function.

### comparer

`type 'a comparer = ('a expected * 'a actual) -> General.order`

Represents a function that compares the [`expected`](#expected)
against the [`actual`](#actual) value.

### tostring

`type 'a tostring = 'a -> string`

Represents a function that converts the given value to a `string`.

### pass

`val pass: Expectation`

Always passes.

```sml
test "this sum is always two" (fn _ =>
  if (1 + 1) = 2 then
    Expect.pass
  else
    Expect.fail "man, something is up...")
```

### fail

`val fail: string -> Expectation`

Always fails.

```sml
test "this sum is always two" (fn _ =>
  if (1 + 1) = 2 then
    Expect.pass
  else
    Expect.fail "man, something is up...")
```

### onFail

`val onFail: string -> Expectation -> Expectation`

If the given expectation fails, replace its failure message with a
custom one.

```sml
test "sum" (fn _ =>
  Expect.onFail
    "this shouldn't be failing"
    (Expect.equal Int.compare 4 (2 + 2)))
```

### all

`val all: Expectation list -> Expectation`

Passes if all of the given expectations pass. If any expectation fails,
returns the first failure.

```sml
test "value is within range" (fn _ =>
  (* ... *)
  Expect.all
    [ Expect.atLeast Int.compare 0 value
    , Expect.atMost Int.compare 100 value
    ])
```

### any

`val any: Expectation list -> Expectation`

Passes if at least one of the given expectations passes. If all
expectations fail, the test fails.

```sml
test "value is either negative or greater than 10" (fn _ =>
  (* ... *)
  Expect.any
    [ Expect.less Int.compare 0 value
    , Expect.greater Int.compare 10 value
    ])
```

### isTrue

`val isTrue: bool actual -> Expectation`

Passes if the provided value is `true`.

```sml
Expect.isTrue (2 > 1)
```

### isFalse

`val isFalse: bool actual -> Expectation`

Passes if the provided value is `false`.

```sml
Expect.isTrue (2 < 1)
```

### some

`val some: 'a option actual -> Expectation`

Passes if the provided value is `SOME`.

```sml
val value = SOME 1
Expect.some value
```

### none

`val none: 'a option actual -> Expectation`

Passes if the provided value is `NONE`.

```sml
val value = NONE
Expect.none value
```

### equal

`val equal: 'a comparer -> 'a expected -> 'a actual -> Expectation`

Passes if the arguments are equal.

```sml
Expect.equal Int.compare 2 (1 + 1)
```

### equalFmt

`val equalFmt: 'a comparer -> 'a tostring -> 'a expected -> 'a actual
-> Expectation`

Passes if the arguments are equal, but receives a
[`tostring`](#tostring) that encapsulates the values on the
`Expectation`.

```sml
Expect.equalFmt Int.compare Int.toString 2 (1 + 1)
```

### notEqual

`val notEqual: 'a comparer -> 'a expected -> 'a actual -> Expectation`

Passes if the arguments are not equal.

```sml
Expect.notEqual Int.compare 3 (1 + 1)
```

### notEqualFmt

`val notEqualFmt: 'a comparer -> 'a tostring -> 'a expected -> 'a
actual -> Expectation`

Passes if the arguments are not equal, but receives a
[`tostring`](#tostring) that encapsulates the values on the
`Expectation`.

```sml
Expect.equalFmt Int.compare Int.toString 2 (1 + 1)
```

### atMost

`val atMost: 'a comparer -> 'a expected -> 'a actual -> Expectation`

Passes if the provide value is less or equal than the expected value.

```sml
Expect.atMost Int.compare 3 2
Expect.atMost Int.compare 2 2
```

### atMostFmt

`val atMostFmt: 'a comparer -> 'a tostring-> 'a expected-> 'a actual->
Expectation`

Passes if the provided value is less or equal than the expeted value,
but receives a [`tostring`](#tostring) that encapsulates the values on
the `Expectation`.

```sml
Expect.atMost Int.compare Int.toString 3 2
Expect.atMost Int.compare Int.toString 2 2
```

### atLeast

`val atLeast: 'a comparer -> 'a expected -> 'a actual -> Expectation`

Passes if the provide value is greater or equal than the expected
value.

```sml
Expect.atMost Int.compare 3 4
Expect.atMost Int.compare 3 3
```

### atLeastFmt

`val atLeastFmt: 'a comparer -> 'a tostring -> 'a expected -> 'a
actual -> Expectation`

Passes if the provided value is greater or equal than the expeted
value, but receives a [`tostring`](#tostring) that encapsulates the
values on the `Expectation`.

```sml
Expect.atMost Int.compare Int.toString 3 4
Expect.atMost Int.compare Int.toString 3 3
```

### less

`val less: 'a comparer -> 'a expected -> 'a actual -> Expectation`

Passes if the provided value is less than the expected value.

```sml
Expect.notEqual Int.compare 3 (1 + 1)
```

### lessFmt

`val lessFmt: 'a comparer -> 'a tostring -> 'a expected -> 'a actual
-> Expectation`

Passes if the provided value is less than the expeted value, but
receives a [`tostring`](#tostring) that encapsulates the values on the
`Expectation`.

```sml
Expect.atMost Int.compare Int.toString 3 2
```

### greater

`val greater: 'a comparer -> 'a expected -> 'a actual -> Expectation`

Passes if the provided value is greater than the expected value.

```sml
Expect.notEqual Int.compare 3 4
```

### greaterFmt

`val greaterFmt: 'a comparer -> 'a tostring -> 'a expected -> 'a
actual -> Expectation`

Passes if the provided value is greater than the expeted value, but
receives a [`tostring`](#tostring) that encapsulates the values on the
`Expectation`.

```sml
Expect.atMost Int.compare Int.toString 3 4
```

## License

[Apache 2.0](https://choosealicense.com/licenses/apache-2.0/)
