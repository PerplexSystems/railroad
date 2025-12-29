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

structure Main =
struct
  fun main (_: string, _: string list) =
    let
      open Test
    in
      run (concat
        [ TestExpect.tests
        , TestRunner.tests
        , TestExpectation.tests
        , TestRandom.tests
        , TestExecutionOrder.tests
        , TestFuzz.tests
        ])
    end
end

val _ = Main.main ("", [])
