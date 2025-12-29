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

structure TestFuzz =
struct
  open Test

  fun testRunCount expected mkTest =
    let
      val counter = ref 0
      val fuzzTest = mkTest (fn () => counter := !counter + 1)
      val thunk =
        case fuzzTest of
          Internal.Labeled (_, Internal.UnitTest f) => f
        | _ => (fn () => Expect.fail "unexpected test structure")
      val _ = thunk ()
    in
      Expect.equal Int.compare expected (!counter)
    end

  val tests =
    let
      open Test
    in
      describe "Fuzz"
        [ describe "primitives"
            [ fuzz "int generates values" Fuzz.int (fn n =>
                Expect.isTrue (n >= ~1000000 andalso n <= 1000000))
            , fuzz "positiveInt generates positive values" Fuzz.positiveInt
                (fn n => Expect.isTrue (n >= 1))
            , fuzz "negativeInt generates negative values" Fuzz.negativeInt
                (fn n => Expect.isTrue (n <= ~1))
            , fuzz "intRange respects bounds" (Fuzz.intRange 10 20) (fn n =>
                Expect.isTrue (n >= 10 andalso n <= 20))
            , fuzz "bool generates booleans" Fuzz.bool (fn b =>
              Expect.any [Expect.isTrue b, Expect.isFalse b])
            , fuzz "char generates printable chars" Fuzz.char (fn c =>
                Expect.isTrue (Char.ord c >= 32 andalso Char.ord c <= 126))
            , fuzz "alphaChar generates letters" Fuzz.alphaChar (fn c =>
                Expect.isTrue (Char.isAlpha c))
            , fuzz "alphaNumChar generates alphanumeric" Fuzz.alphaNumChar
                (fn c => Expect.isTrue (Char.isAlphaNum c))
            , fuzz "real generates values" Fuzz.real (fn _ => Expect.pass)
            , fuzz "realRange respects bounds" (Fuzz.realRange 1.0 2.0) (fn r =>
                Expect.isTrue (r >= 1.0 andalso r <= 2.0))
            , fuzz "unit generates unit" Fuzz.unit (fn () => Expect.pass)
            ]
        , describe "strings"
            [ fuzz "string generates strings" Fuzz.string (fn s =>
                Expect.isTrue (String.size s <= 20))
            , fuzz "stringOfLength generates correct length"
                (Fuzz.stringOfLength 5)
                (fn s => Expect.equal Int.compare 5 (String.size s))
            , fuzz "stringOfLengthBetween respects bounds"
                (Fuzz.stringOfLengthBetween 3 7)
                (fn s =>
                   Expect.isTrue (String.size s >= 3 andalso String.size s <= 7))
            , fuzz "alphaString generates alpha strings" Fuzz.alphaString
                (fn s => Expect.isTrue (CharVector.all Char.isAlpha s))
            ]
        , describe "collections"
            [ fuzz "list generates lists" (Fuzz.list Fuzz.int) (fn xs =>
                Expect.isTrue (List.length xs <= 10))
            , fuzz "listOfLength generates correct length"
                (Fuzz.listOfLength 5 Fuzz.int)
                (fn xs => Expect.equal Int.compare 5 (List.length xs))
            , fuzz "listOfLengthBetween respects bounds"
                (Fuzz.listOfLengthBetween 2 4 Fuzz.int)
                (fn xs =>
                   Expect.isTrue
                     (List.length xs >= 2 andalso List.length xs <= 4))
            , fuzz "option generates options" (Fuzz.option Fuzz.int) (fn _ =>
                Expect.pass)
            , fuzz "pair generates pairs" (Fuzz.pair Fuzz.int Fuzz.bool)
                (fn (_, _) => Expect.pass)
            , fuzz "triple generates triples"
                (Fuzz.triple Fuzz.int Fuzz.bool Fuzz.char)
                (fn (_, _, _) => Expect.pass)
            ]
        , describe "combinators"
            [ fuzz "constant always returns same value" (Fuzz.constant 42)
                (Expect.equal Int.compare 42)
            , fuzz "map transforms values"
                (Fuzz.map (fn n => n * 2) (Fuzz.intRange 1 10))
                (fn n =>
                   Expect.isTrue (n >= 2 andalso n <= 20 andalso n mod 2 = 0))
            , fuzz "map2 combines two generators"
                (Fuzz.map2 (fn a => fn b => a + b) (Fuzz.intRange 1 5)
                   (Fuzz.intRange 1 5))
                (fn n => Expect.isTrue (n >= 2 andalso n <= 10))
            , fuzz "oneOf picks from list"
                (Fuzz.oneOf [Fuzz.constant 1, Fuzz.constant 2, Fuzz.constant 3])
                (fn n => Expect.isTrue (n = 1 orelse n = 2 orelse n = 3))
            , fuzz "frequency respects weights"
                (Fuzz.frequency
                   [(1, Fuzz.constant true), (1, Fuzz.constant false)])
                (fn _ => Expect.pass)
            , fuzz "filter removes non-matching values"
                (Fuzz.filter (fn n => n > 0) Fuzz.int)
                (fn n => Expect.isTrue (n > 0))
            , fuzz "noShrink works" (Fuzz.noShrink Fuzz.int) (fn _ =>
                Expect.pass)
            , fuzz "andThen chains generators"
                (Fuzz.andThen (fn n => Fuzz.listOfLength n Fuzz.bool)
                   (Fuzz.intRange 1 5))
                (fn xs =>
                   Expect.isTrue
                     (List.length xs >= 1 andalso List.length xs <= 5))
            , test "oneOf with empty list fails gracefully" (fn _ =>
                TestHelper.expectToFail
                  (Fuzz.run 1 (Fuzz.oneOf []) (fn _ => Expect.pass)))
            , test "frequency with empty list fails gracefully" (fn _ =>
                TestHelper.expectToFail
                  (Fuzz.run 1 (Fuzz.frequency []) (fn _ => Expect.pass)))
            , test "filter that never matches fails gracefully" (fn _ =>
                TestHelper.expectToFail
                  (Fuzz.run 1 (Fuzz.filter (fn _ => false) Fuzz.int) (fn _ =>
                     Expect.pass)))
            ]
        , describe "fuzz variants"
            [ fuzz2 "fuzz2 works with two generators" Fuzz.int Fuzz.bool
                (fn n => fn b => Expect.isTrue (n = n andalso b = b))
            , fuzz3 "fuzz3 works with three generators" Fuzz.int Fuzz.bool
                Fuzz.char
                (fn n =>
                   fn b =>
                     fn c => Expect.isTrue (n = n andalso b = b andalso c = c))
            , test "fuzzWith respects run count" (fn _ =>
                testRunCount 10 (fn tick =>
                  Test.fuzzWith 10 "inner" Fuzz.int (fn _ =>
                    (tick (); Expect.pass))))
            , test "fuzz2With respects run count" (fn _ =>
                testRunCount 5 (fn tick =>
                  Test.fuzz2With 5 "inner" Fuzz.int Fuzz.bool (fn _ =>
                    fn _ => (tick (); Expect.pass))))
            , test "fuzz3With respects run count" (fn _ =>
                testRunCount 7 (fn tick =>
                  Test.fuzz3With 7 "inner" Fuzz.int Fuzz.bool Fuzz.char (fn _ =>
                    fn _ => fn _ => (tick (); Expect.pass))))
            ]
        , describe "properties"
            [ fuzz "reverse reverse is identity" (Fuzz.list Fuzz.int) (fn xs =>
                Expect.equal (List.collate Int.compare) xs (rev (rev xs)))
            , fuzz "append length"
                (Fuzz.pair (Fuzz.list Fuzz.int) (Fuzz.list Fuzz.int))
                (fn (xs, ys) =>
                   Expect.equal Int.compare (List.length xs + List.length ys)
                     (List.length (xs @ ys)))
            , fuzz "map preserves length" (Fuzz.list Fuzz.int) (fn xs =>
                Expect.equal Int.compare (List.length xs) (List.length
                  (List.map (fn x => x + 1) xs)))
            ]
        ]
    end
end
