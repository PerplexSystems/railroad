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

structure TestExecutionOrder =
struct
  val tests =
    let
      open Test
      open Runner
      open TestHelper

      fun compareIntOption (NONE, NONE) = EQUAL
        | compareIntOption (NONE, SOME _) = LESS
        | compareIntOption (SOME _, NONE) = GREATER
        | compareIntOption (SOME a, SOME b) = Int.compare (a, b)

      fun getLabels runners =
        List.map (fn {labels, ...}: Runner.Runner => String.concatWith "." labels) runners

      fun makeTestRunners () =
        let
          val tests = describe "suite"
            [ test "zebra" expectPass
            , test "apple" expectPass
            , test "mango" expectPass
            , test "banana" expectPass
            ]
        in
          case fromTest tests of
            Plain rs => rs
          | _ => []
        end
    in
      describe "ExecutionOrder"
        [ describe "DeclarationOrder"
            [ test "preserves declaration order" (fn _ =>
                let
                  val runners = makeTestRunners ()
                  val (ordered, seedOpt) = applyExecutionOrder Configuration.DeclarationOrder runners
                  val labels = getLabels ordered
                  val expected = ["suite.zebra", "suite.apple", "suite.mango", "suite.banana"]
                in
                  Expect.all
                    [ Expect.equal (List.collate String.compare) expected labels
                    , Expect.none seedOpt
                    ]
                end)
            ]

        , describe "AlphabeticalOrder"
            [ test "sorts tests alphabetically by label" (fn _ =>
                let
                  val runners = makeTestRunners ()
                  val (ordered, seedOpt) = applyExecutionOrder Configuration.AlphabeticalOrder runners
                  val labels = getLabels ordered
                  val expected = ["suite.apple", "suite.banana", "suite.mango", "suite.zebra"]
                in
                  Expect.all
                    [ Expect.equal (List.collate String.compare) expected labels
                    , Expect.none seedOpt
                    ]
                end)

            , test "handles nested describes" (fn _ =>
                let
                  val tests = describe "root"
                    [ describe "z-group"
                        [ test "first" expectPass ]
                    , describe "a-group"
                        [ test "second" expectPass ]
                    ]
                  val runners =
                    case fromTest tests of
                      Plain rs => rs
                    | _ => []
                  val (ordered, _) = applyExecutionOrder Configuration.AlphabeticalOrder runners
                  val labels = getLabels ordered
                  val expected = ["root.a-group.second", "root.z-group.first"]
                in
                  Expect.equal (List.collate String.compare) expected labels
                end)
            ]

        , describe "RandomOrder"
            [ test "shuffles tests" (fn _ =>
                let
                  val runners = makeTestRunners ()
                  val (ordered, seedOpt) = applyExecutionOrder (Configuration.RandomOrder NONE) runners
                  val originalLabels = getLabels runners
                  val shuffledLabels = getLabels ordered
                in
                  Expect.all
                    [ Expect.some seedOpt
                    (* Verify same number of tests *)
                    , Expect.equal Int.compare (List.length originalLabels) (List.length shuffledLabels)
                    ]
                end)

            , test "same seed produces same order" (fn _ =>
                let
                  val seed = 12345
                  val runners = makeTestRunners ()
                  val (ordered1, seed1) = applyExecutionOrder (Configuration.RandomOrder (SOME seed)) runners
                  val labels1 = getLabels ordered1

                  val runners2 = makeTestRunners ()
                  val (ordered2, seed2) = applyExecutionOrder (Configuration.RandomOrder (SOME seed)) runners2
                  val labels2 = getLabels ordered2
                in
                  Expect.all
                    [ Expect.equal (List.collate String.compare) labels1 labels2
                    , Expect.equal compareIntOption seed1 seed2
                    ]
                end)

            , test "different seeds produce different orders" (fn _ =>
                let
                  val runners1 = makeTestRunners ()
                  val (ordered1, _) = applyExecutionOrder (Configuration.RandomOrder (SOME 111)) runners1
                  val labels1 = getLabels ordered1

                  val runners2 = makeTestRunners ()
                  val (ordered2, _) = applyExecutionOrder (Configuration.RandomOrder (SOME 999)) runners2
                  val labels2 = getLabels ordered2
                in
                  Expect.notEqual (List.collate String.compare) labels1 labels2
                end)

            , test "returns the seed used" (fn _ =>
                let
                  val seed = 42
                  val runners = makeTestRunners ()
                  val (_, seedOpt) = applyExecutionOrder (Configuration.RandomOrder (SOME seed)) runners
                in
                  Expect.equal compareIntOption (SOME seed) seedOpt
                end)
            ]
        ]
    end
end
