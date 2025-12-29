signature TEST =
sig
  type test

  structure Configuration: CONFIGURATION

  val describe: string -> test list -> test
  val test: string -> (unit -> Expectation.Expectation) -> test
  val testTheory: string -> 'a list -> ('a -> Expectation.Expectation) -> test
  val skip: test -> test
  val focus: test -> test
  val concat: test list -> test

  val fuzz: string -> 'a Fuzz.t -> ('a -> Expectation.Expectation) -> test
  val fuzz2: string
             -> 'a Fuzz.t
             -> 'b Fuzz.t
             -> ('a -> 'b -> Expectation.Expectation)
             -> test
  val fuzz3: string
             -> 'a Fuzz.t
             -> 'b Fuzz.t
             -> 'c Fuzz.t
             -> ('a -> 'b -> 'c -> Expectation.Expectation)
             -> test
  val fuzzWith: int
                -> string
                -> 'a Fuzz.t
                -> ('a -> Expectation.Expectation)
                -> test
  val fuzz2With: int
                 -> string
                 -> 'a Fuzz.t
                 -> 'b Fuzz.t
                 -> ('a -> 'b -> Expectation.Expectation)
                 -> test
  val fuzz3With: int
                 -> string
                 -> 'a Fuzz.t
                 -> 'b Fuzz.t
                 -> 'c Fuzz.t
                 -> ('a -> 'b -> 'c -> Expectation.Expectation)
                 -> test

  val run: test -> unit
  val runWithConfig: Configuration.Setting list -> test -> unit
end

structure Test: TEST =
struct
  structure Configuration = Configuration
  open Expectation
  open Internal

  type test = Internal.Test

  fun describe description tests =
    let
      val desc = String.trim description
    in
      if desc = "" then
        failnow
          { description = "This `describe` has a blank description."
          , reason = Invalid BadDescription
          }
      else if List.null tests then
        failnow
          { description = "This `describe` " ^ desc ^ "` has no tests in it."
          , reason = Invalid EmptyList
          }
      else
        case duplicatedNames tests of
          ERROR dups =>
            let
              fun dupDescription duped =
                "The `describe` '" ^ desc ^ "' Contains multiple tests named '"
                ^ duped ^ "'. Rename them to know which is which."
            in
              Labeled (desc, Internal.failnow
                { description = String.concatWith "\n"
                    (List.map dupDescription dups)
                , reason = Invalid DuplicatedName
                })
            end
        | OK children =>
            if List.exists (fn x => x = desc) children then
              Labeled (desc, Internal.failnow
                { description =
                    "The test '" ^ desc
                    ^ "' contains a child test of the same name '" ^ desc
                    ^ "'. Rename them to know which is which."
                , reason = Invalid DuplicatedName
                })
            else
              Labeled (desc, Batch tests)
    end

  fun test description code =
    let
      val desc = String.trim description
    in
      if desc = "" then blankDescriptionFail
      else Labeled (description, UnitTest code)
    end

  fun testTheory description theories code =
    let
      val desc = String.trim description

      fun createTest count theory =
        test (Int.toString count) (fn _ => (code theory))

      fun accumulateTests (theory, (count, tests)) =
        let val theoryTest = createTest count theory
        in (count + 1, theoryTest :: tests)
        end

      val (_, tests) = List.foldl accumulateTests (1, []) theories
    in
      if desc = "" then
        failnow
          { description = "This `testTheory` has a blank description."
          , reason = Invalid BadDescription
          }
      else if List.null theories then
        failnow
          { description =
              "This `testTheory` " ^ desc ^ "` has no theories in it."
          , reason = Invalid EmptyList
          }
      else
        describe desc tests
    end

  fun skip test = Skipped test
  fun focus test = Focused test

  fun concat tests =
    if List.length tests = 0 then
      UnitTest (fn _ =>
        fail
          { description = "This `concat` has no tests in it."
          , reason = Invalid EmptyList
          })
    else
      case duplicatedNames tests of
        OK _ => Batch tests
      | ERROR duplicates =>
          let
            open Expectation

            fun duplicatedDescription duped =
              "A test group contains multiple tests named '" ^ duped
              ^ "'. Do some renaming so that tests have unique names."

            val description = String.concatWith "\n"
              (List.map duplicatedDescription duplicates)
          in
            failnow {description = description, reason = Invalid DuplicatedName}
          end

  val defaultFuzzRuns = 100

  fun fuzzWith runs description gen testFn =
    let
      val desc = String.trim description
    in
      if desc = "" then blankDescriptionFail
      else Labeled (desc, UnitTest (fn () => Fuzz.run runs gen testFn))
    end

  fun fuzz description gen testFn =
    fuzzWith defaultFuzzRuns description gen testFn

  fun fuzz2With runs description genA genB testFn =
    fuzzWith runs description (Fuzz.pair genA genB) (fn (a, b) => testFn a b)

  fun fuzz2 description genA genB testFn =
    fuzz2With defaultFuzzRuns description genA genB testFn

  fun fuzz3With runs description genA genB genC testFn =
    fuzzWith runs description (Fuzz.triple genA genB genC) (fn (a, b, c) =>
      testFn a b c)

  fun fuzz3 description genA genB genC testFn =
    fuzz3With defaultFuzzRuns description genA genB genC testFn

  val run = Runner.run
  val runWithConfig = Runner.runWithConfig
end
