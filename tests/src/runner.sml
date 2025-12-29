(*
  Copyright 2023 Perplex Systems

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
*)

structure TestRunner =
struct
  val tests =
    let
      open Test
      open Runner
      open TestHelper
    in
      describe "Runner"
        [ describe "fromTest"
            [ test "focus inside a focus has no effect" (fn _ =>
                let
                  val tests = describe "three tests"
                    [ test "passes" expectPass
                    , focus (describe "two tests"
                        [ test "fails" (fn _ => Expect.fail "failed on purpose")
                        , focus (test "is an only" (fn _ =>
                            Expect.fail "failed on purpose"))
                        ])
                    ]

                  val runners = fromTest tests
                  val expected = 2
                  val actual =
                    case runners of
                      Focusing {runners, ...} => List.length runners
                    | _ => 0
                in
                  (Expect.equalFmt Int.compare Int.toString expected actual)
                end)

            , test "a skip inside a focus takes effect" (fn _ =>
                let
                  val tests = describe "three tests"
                    [ test "passes" expectPass
                    , focus (describe "two tests"
                        [ test "fails" (fn _ => Expect.fail "failed on purpose")
                        , skip (test "is skipped" (fn _ =>
                            Expect.fail "failed on purpose"))
                        ])
                    ]

                  val runners = fromTest tests
                  val expected = 1
                  val actual =
                    case runners of
                      Focusing {runners, ...} => List.length runners
                    | _ => 0
                in
                  (Expect.equalFmt Int.compare Int.toString expected actual)
                end)

            , test "a focus inside a skip has no effect" (fn _ =>
                let
                  val tests = describe "three tests"
                    [ test "passes" expectPass
                    , skip (describe "two tests"
                        [ test "fails" (fn _ => Expect.fail "failed on purpose")
                        , focus (test "is skipped" (fn _ =>
                            Expect.fail "failed on purpose"))
                        ])
                    ]

                  val runners = fromTest tests
                  val expected = 1
                  val actual =
                    case runners of
                      Skipping {runners, ...} => List.length runners
                    | _ => 0
                in
                  (Expect.equalFmt Int.compare Int.toString expected actual)
                end)
            ]
        , describe "runtests"
            [ test "stops on first failure when stopOnFirstFailure is true"
                (fn _ =>
                   let
                     val output = TextIO.openOut "/dev/null"
                     val tests = describe "three tests"
                       [ test "first fails" (fn _ =>
                           Expect.fail "first failure")
                       , test "second fails" (fn _ =>
                           Expect.fail "second failure")
                       , test "third fails" (fn _ =>
                           Expect.fail "third failure")
                       ]
                     val runners =
                       case fromTest tests of
                         Plain {runners, ...} => runners
                       | _ => []
                     val report =
                       runtests output false true NONE
                         {skipped = 0, focused = 0} runners
                     val _ = TextIO.closeOut output
                   in
                     Expect.equalFmt Int.compare Int.toString 1 (#failed report)
                   end)
            , test "runs all tests when stopOnFirstFailure is false" (fn _ =>
                let
                  val output = TextIO.openOut "/dev/null"
                  val tests = describe "three tests"
                    [ test "first fails" (fn _ => Expect.fail "first failure")
                    , test "second fails" (fn _ => Expect.fail "second failure")
                    , test "third fails" (fn _ => Expect.fail "third failure")
                    ]
                  val runners =
                    case fromTest tests of
                      Plain {runners, ...} => runners
                    | _ => []
                  val report =
                    runtests output false false NONE {skipped = 0, focused = 0}
                      runners
                  val _ = TextIO.closeOut output
                in
                  Expect.equalFmt Int.compare Int.toString 3 (#failed report)
                end)
            ]
        , describe "todistribution"
            [ test "have a single test" (fn _ =>
                let
                  val tests = describe "single test"
                    [test "" (fn _ => Expectation.Pass)]
                  val distribution = todistribution tests
                  val expected = 1
                  val actual = List.length (#all distribution)
                in
                  Expect.equalFmt Int.compare Int.toString expected actual
                end)
            , test "have a focused test" (fn _ =>
                let
                  val tests = describe "single test"
                    [ focus (test "" (fn _ => Expectation.Pass))
                    , test "" (fn _ => Expectation.Pass)
                    ]
                  val distribution = todistribution tests
                  val actual = List.length (#focused distribution)
                in
                  Expect.equalFmt Int.compare Int.toString 1 actual
                end)
            , test "have three tests" (fn _ =>
                let
                  val tests = testTheory "even numbers" [2, 4, 6] (fn num =>
                    Expect.isTrue (num mod 2 = 0))
                  val distribution = todistribution tests
                  val actual = List.length (#all distribution)
                  val expected = 3
                in
                  Expect.equalFmt Int.compare Int.toString expected actual
                end)
            ]
        ]
    end
end
