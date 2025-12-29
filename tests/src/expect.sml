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

structure TestExpect =
struct
  open Expect

  val tests =
    let
      open Test
      open TestHelper
    in
      describe "Expect"
        [ test "pass" (fn _ => Expect.pass)
        , test "fail" (fn _ =>
            expectToFail (Expect.fail "This test should fail"))
        , test "onFail" (fn _ =>
            expectToFail
              (Expect.onFail "This test should fail" (Expect.fail "custom fail")))
        , test "isTrue" (fn _ => Expect.isTrue true)
        , test "isFalse" (fn _ => Expect.isFalse false)
        , test "equal" (fn _ => Expect.equal Int.compare 1 1)
        , test "equalFmt" (fn _ => Expect.equalFmt Int.compare Int.toString 1 1)
        , test "notEqual" (fn _ => Expect.notEqual Int.compare 1 2)
        , test "notEqualFmt" (fn _ => Expect.notEqual Int.compare 1 2)
        , describe "atMost"
            [ test "equal value" (fn _ => Expect.atMost Int.compare 1 1)
            , test "less value" (fn _ =>
                expectToFail (Expect.atMost Int.compare 1 2))
            , test "greater value" (fn _ => Expect.atMost Int.compare 2 1)
            , test "fmt equal value" (fn _ =>
                Expect.atMostFmt Int.compare Int.toString 1 1)
            ]
        , describe "atLeast"
            [ test "equal value" (fn _ => Expect.atLeast Int.compare 1 1)
            , test "less value" (fn _ => Expect.atLeast Int.compare 1 2)
            , test "greater value" (fn _ =>
                expectToFail (Expect.atLeast Int.compare 2 1))
            , test "fmt greater value" (fn _ =>
                expectToFail (Expect.atLeastFmt Int.compare Int.toString 2 1))
            ]
        , test "less" (fn _ => Expect.less Int.compare 2 1)
        , test "lessFmt" (fn _ => Expect.lessFmt Int.compare Int.toString 2 1)
        , test "greater" (fn _ => Expect.greater Int.compare 1 2)
        , test "greaterFmt" (fn _ =>
            Expect.greaterFmt Int.compare Int.toString 1 2)
        , describe "all"
            [ test "passes when all expectations pass" (fn _ =>
                Expect.all
                  [ Expect.pass
                  , Expect.isTrue true
                  , Expect.equal Int.compare 1 1
                  ])
            , test "fails when one expectation fails" (fn _ =>
                expectToFail (Expect.all
                  [ Expect.pass
                  , Expect.fail "this fails"
                  , Expect.pass
                  ]))
            , test "fails when all expectations fail" (fn _ =>
                expectToFail (Expect.all
                  [ Expect.fail "first"
                  , Expect.fail "second"
                  ]))
            , test "fails with empty list" (fn _ =>
                expectToFail (Expect.all []))
            ]
        , describe "any"
            [ test "passes when all expectations pass" (fn _ =>
                Expect.any
                  [ Expect.pass
                  , Expect.pass
                  ])
            , test "passes when one expectation passes" (fn _ =>
                Expect.any
                  [ Expect.fail "this fails"
                  , Expect.pass
                  , Expect.fail "this also fails"
                  ])
            , test "fails when all expectations fail" (fn _ =>
                expectToFail (Expect.any
                  [ Expect.fail "first"
                  , Expect.fail "second"
                  ]))
            , test "fails with empty list" (fn _ =>
                expectToFail (Expect.any []))
            ]
        ]
    end
end
