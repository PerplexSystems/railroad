structure Runner =
struct
  open Expectation
  open Internal

  type Runner = {run: unit -> Expectation, labels: string list}

  fun runnerLabel ({labels, ...}: Runner) =
    String.concatWith "." (List.map Internal.String.trim labels)

  fun sortAlphabetically (runners: Runner list) =
    List.mergeSort (fn (a, b) => String.compare (runnerLabel a, runnerLabel b))
      runners

  fun shuffle rng (runners: Runner list) =
    let
      val arr = Array.fromList runners
      val n = Array.length arr
      fun swap (i, j) =
        let val tmp = Array.sub (arr, i)
        in Array.update (arr, i, Array.sub (arr, j)); Array.update (arr, j, tmp)
        end
      fun loop i =
        if i <= 0 then
          ()
        else
          let val j = TimeBasedRandom.randInt rng (0, i)
          in swap (i, j); loop (i - 1)
          end
    in
      loop (n - 1);
      Array.foldr (op::) [] arr
    end

  fun applyExecutionOrder order (runners: Runner list) =
    case order of
      Configuration.DeclarationOrder => (runners, NONE)
    | Configuration.AlphabeticalOrder => (sortAlphabetically runners, NONE)
    | Configuration.RandomOrder seedOpt =>
        let
          val rng =
            case seedOpt of
              SOME seed => TimeBasedRandom.randFromSeed seed
            | NONE => TimeBasedRandom.rand ()
          val initialSeed = TimeBasedRandom.getSeed rng
        in
          (shuffle rng runners, SOME initialSeed)
        end

  datatype Runnable = Thunk of (unit -> Expectation)

  datatype RunnableTree =
    Runnable of Runnable
  | LabeledRunnable of (string * RunnableTree)

  type Distribution =
    { all: RunnableTree list
    , focused: RunnableTree list
    , skipped: RunnableTree list
    }

  type RunReport =
    {passed: int, failed: int, skipped: int, focused: int, seed: int option}

  type RunnerResult = {runners: Runner list, skipped: int, focused: int}

  datatype Runners =
    Plain of RunnerResult
  | Focusing of RunnerResult
  | Skipping of RunnerResult
  | Invalid of string

  fun runThunk (Thunk thunk) = thunk ()

  fun fromRunnableTree labels runner =
    case runner of
      Runnable runnable => [{labels = labels, run = fn () => runThunk runnable}]
    | LabeledRunnable (label, subRunner) =>
        fromRunnableTree (labels @ [label]) subRunner

  fun todistribution test =
    case test of
      UnitTest code =>
        {all = [Runnable (Thunk (fn _ => code ()))], focused = [], skipped = []}

    | Labeled (description, ts) =>
        let
          val next = todistribution ts
          val labelTests = (fn tests => LabeledRunnable (description, tests))
        in
          { all = List.map labelTests (#all next)
          , focused = List.map labelTests (#focused next)
          , skipped = List.map labelTests (#skipped next)
          }
        end

    | Batch ts =>
        List.foldl
          (fn (final, prev) =>
             let
               val next = todistribution final
             in
               { all = (#all prev) @ (#all next)
               , focused = (#focused prev) @ (#focused next)
               , skipped = (#skipped prev) @ (#skipped next)
               }
             end) {all = [], focused = [], skipped = []} ts

    | Focused t =>
        let val next = todistribution t
        in {all = (#all next), focused = (#all next), skipped = (#skipped next)}
        end

    | Skipped t =>
        let val next = todistribution t
        in {all = [], focused = [], skipped = (#all next)}
        end

  fun fromTest test =
    let
      val {focused, skipped, all} = todistribution test

      fun countallrunnables trees =
        let
          fun countrunnables runnable =
            case runnable of
              Runnable _ => 1
            | LabeledRunnable (_, runner) => countrunnables runner
        in
          List.foldl (fn (runnable, acc) => (countrunnables runnable) + acc) 0
            trees
        end
      val skippedCount = countallrunnables skipped
      val focusedCount = countallrunnables focused
    in
      if focusedCount = 0 then
        if skippedCount = 0 then
          Plain
            { runners = Internal.List.concatMap (fromRunnableTree []) all
            , skipped = 0
            , focused = 0
            }
        else
          Skipping
            { runners = Internal.List.concatMap (fromRunnableTree []) all
            , skipped = skippedCount
            , focused = 0
            }
      else
        Focusing
          { runners = Internal.List.concatMap (fromRunnableTree []) focused
          , skipped = skippedCount
          , focused = focusedCount
          }
    end

  fun evalrunner (runner: Runner) =
    let
      val {labels, run} = runner
      val label = String.concatWith "." (List.map Internal.String.trim labels)

      val expectation = run ()
      val expectationstr = Expectation.toString expectation
      val str =
        case expectation of
          Pass => "=== PASS: " ^ label
        | Fail _ => "=== FAIL: " ^ label ^ "\n    " ^ expectationstr ^ "\n"
    in
      {result = str ^ "\n", passed = (expectation = Pass)}
    end

  fun runreport seed {skipped, focused} runs =
    List.foldl
      (fn ({passed, result = _}, acc) =>
         if passed then
           { passed = (#passed acc) + 1
           , failed = (#failed acc)
           , skipped = skipped
           , focused = focused
           , seed = seed
           }
         else
           { passed = (#passed acc)
           , failed = (#failed acc) + 1
           , skipped = skipped
           , focused = focused
           , seed = seed
           })
      { passed = 0
      , failed = 0
      , skipped = skipped
      , focused = focused
      , seed = seed
      } runs

  fun printreport stream {passed, failed, skipped, focused, seed} =
    let
      val seedOutput =
        case seed of
          SOME s => "Random seed: " ^ Int.toString s ^ "\n"
        | NONE => ""
      val output =
        ("\n" ^ seedOutput ^ "Passed: " ^ Int.toString passed ^ ", failed: "
         ^ Int.toString failed ^ ", skipped: " ^ Int.toString skipped
         ^ ", focused: " ^ Int.toString focused ^ "\n")
    in
      TextIO.output (stream, output)
    end

  fun runtests stream printPassed stopOnFirstFailure (seed: int option) counts
    runners =
    let
      fun loop [] acc = acc
        | loop (runner :: rest) acc =
            let
              val run = evalrunner runner
              val newAcc = acc @ [run]
            in
              if not (#passed run) andalso stopOnFirstFailure then newAcc
              else loop rest newAcc
            end
      val runs = loop runners []
      val report = runreport seed counts runs
    in
      ( List.app
          (fn {result, passed} =>
             if not passed orelse printPassed then
               TextIO.output (stream, result)
             else
               ()) runs
      ; report
      )
    end

  fun runWithConfig options test =
    let
      val {output, printPassed, stopOnFirstFailure, executionOrder} =
        Configuration.fromList options

      val runners = let open Configuration in fromTest test end

      fun runWithOrder {runners, skipped, focused} =
        let
          val (orderedRunners, seedOpt) =
            applyExecutionOrder executionOrder runners
        in
          runtests output printPassed stopOnFirstFailure seedOpt
            {skipped = skipped, focused = focused} orderedRunners
        end
    in
      case runners of
        Plain result =>
          let
            val report = runWithOrder result
            val _ = printreport output report
          in
            if (#failed report) > 0 then OS.Process.exit OS.Process.failure
            else OS.Process.exit OS.Process.success
          end
      | Skipping result =>
          let
            val report = runWithOrder result
            val _ = printreport output report
          in
            (* skipping a test should always fail all the tests *)
            OS.Process.exit OS.Process.failure
          end

      | Focusing result =>
          let
            val report = runWithOrder result
            val _ = printreport output report
          in
            (* focusing a test should always fail all the tests *)
            OS.Process.exit OS.Process.failure
          end
      | Invalid _ => (* TODO *) OS.Process.exit OS.Process.failure
    end

  fun run test = runWithConfig [] test
end
