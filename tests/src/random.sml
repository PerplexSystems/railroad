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

structure TestRandom =
struct
  val tests =
    let
      open Test
      open TimeBasedRandom
    in
      describe "TimeBasedRandom"
        [ describe "rand"
            [test "creates a random generator" (fn _ =>
               let val _ = rand ()
               in Expect.pass
               end)]
        , describe "setSeed"
            [test "allows reproducible sequences" (fn _ =>
               let
                 val r1 = rand ()
                 val r2 = rand ()
                 val _ = setSeed r1 42
                 val _ = setSeed r2 42
                 val v1 = randInt r1 (0, 1000)
                 val v2 = randInt r2 (0, 1000)
               in
                 Expect.equalFmt Int.compare Int.toString v1 v2
               end)]
        , describe "randInt"
            [ test "returns value within range" (fn _ =>
                let
                  val r = rand ()
                  val _ = setSeed r 123
                  val v = randInt r (10, 20)
                in
                  Expect.all
                    [ Expect.atLeastFmt Int.compare Int.toString 10 v
                    , Expect.atMostFmt Int.compare Int.toString 20 v
                    ]
                end)
            , test "returns deterministic value with fixed seed" (fn _ =>
                let
                  val r = rand ()
                  val _ = setSeed r 42
                  val v1 = randInt r (0, 100)
                  val _ = setSeed r 42
                  val v2 = randInt r (0, 100)
                in
                  Expect.equalFmt Int.compare Int.toString v1 v2
                end)
            , test "returns different values on successive calls" (fn _ =>
                let
                  val r = rand ()
                  val _ = setSeed r 42
                  val v1 = randInt r (0, 1000000)
                  val v2 = randInt r (0, 1000000)
                in
                  Expect.notEqual Int.compare v1 v2
                end)
            ]
        , describe "randReal"
            [ test "returns value between 0 and 1" (fn _ =>
                let
                  val r = rand ()
                  val _ = setSeed r 42
                  val v = randReal r
                in
                  Expect.all
                    [ Expect.atLeastFmt Real.compare Real.toString 0.0 v
                    , Expect.atMostFmt Real.compare Real.toString 1.0 v
                    ]
                end)
            , test "returns deterministic value with fixed seed" (fn _ =>
                let
                  val r = rand ()
                  val _ = setSeed r 99
                  val v1 = randReal r
                  val _ = setSeed r 99
                  val v2 = randReal r
                in
                  Expect.equalFmt Real.compare Real.toString v1 v2
                end)
            ]
        , describe "randRange"
            [ test "returns value within range" (fn _ =>
                let
                  val r = rand ()
                  val _ = setSeed r 42
                  val v = randRange r 5 15
                in
                  Expect.all
                    [ Expect.atLeastFmt Int.compare Int.toString 5 v
                    , Expect.atMostFmt Int.compare Int.toString 15 v
                    ]
                end)
            , test "is equivalent to randInt" (fn _ =>
                let
                  val r1 = rand ()
                  val r2 = rand ()
                  val _ = setSeed r1 42
                  val _ = setSeed r2 42
                  val v1 = randRange r1 10 50
                  val v2 = randInt r2 (10, 50)
                in
                  Expect.equalFmt Int.compare Int.toString v1 v2
                end)
            ]
        ]
    end
end
