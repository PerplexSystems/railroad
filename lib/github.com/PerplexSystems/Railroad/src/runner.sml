structure Runner =
struct
  open Expectation
  open Internal

  type Runner = {run: unit -> Expectation, labels: string list}

  datatype Runnable = Thunk of (unit -> Expectation)

  datatype RunnableTree =
    Runnable of Runnable
  | LabeledRunnable of (string * RunnableTree)

  type Distribution =
    { all: RunnableTree list
    , focused: RunnableTree list
    , skipped: RunnableTree list
    }

  type RunReport = {passed: int, failed: int, skipped: int}

  datatype Runners =
    Plain of Runner list
  | Focusing of Runner list
  | Skipping of Runner list
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
    in
      if List.null focused then
        if (countallrunnables skipped) = 0 then
          Plain (Internal.List.concatMap (fromRunnableTree []) all)
        else
          Skipping (Internal.List.concatMap (fromRunnableTree []) all)
      else
        Focusing (Internal.List.concatMap (fromRunnableTree []) focused)
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

  fun runreport runs =
    List.foldl
      (fn ({passed, result}, acc) =>
         if passed then {passed = (#passed acc) + 1, failed = (#failed acc)}
         else {passed = (#passed acc), failed = (#failed acc) + 1})
      {passed = 0, failed = 0} runs

  fun printreport stream {passed, failed} =
    let
      val output =
        ("Passed: " ^ Int.toString passed ^ ", failed: " ^ Int.toString failed
         ^ "\n")
    in
      TextIO.output (stream, output)
    end

  fun runtests stream printPassed stopOnFirstFailure runners =
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
      val report = runreport runs
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
      val {output, printPassed, stopOnFirstFailure} = Configuration.fromList options

      val runners = let open Configuration in fromTest test end
    in
      case runners of
        Plain rs =>
          let
            val report = runtests output printPassed stopOnFirstFailure rs
            val _ = printreport output report
          in
            if (#failed report) > 0 then OS.Process.exit OS.Process.failure
            else OS.Process.exit OS.Process.success
          end
      | Skipping rs =>
          let
            val report = runtests output printPassed stopOnFirstFailure rs
            val _ = printreport output report
          in
            (* skipping a test should always fail all the tests *)
            OS.Process.exit OS.Process.failure
          end

      | Focusing rs =>
          let
            val report = runtests output printPassed stopOnFirstFailure rs
            val _ = printreport output report
          in
            (* focusing a test should always fail all the tests *)
            OS.Process.exit OS.Process.failure
          end
      | Invalid _ => (* TODO *) OS.Process.exit OS.Process.failure
    end

  fun run test = runWithConfig [] test
end
