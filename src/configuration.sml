signature CONFIGURATION =
sig
  datatype ExecutionOrder =
    AlphabeticalOrder
  | DeclarationOrder
  | RandomOrder of int option

  datatype Setting =
    Output of TextIO.outstream
  | PrintPassed of bool
  | StopOnFirstFailure of bool
  | ExecutionOrder of ExecutionOrder

  type Configuration =
    { output: TextIO.outstream
    , printPassed: bool
    , stopOnFirstFailure: bool
    , executionOrder: ExecutionOrder
    }

  val fromList: Setting list -> Configuration
  val default: Configuration
end

structure Configuration: CONFIGURATION =
struct
  datatype ExecutionOrder =
    AlphabeticalOrder
  | DeclarationOrder
  | RandomOrder of int option

  datatype Setting =
    Output of TextIO.outstream
  | PrintPassed of bool
  | StopOnFirstFailure of bool
  | ExecutionOrder of ExecutionOrder

  type Configuration =
    { output: TextIO.outstream
    , printPassed: bool
    , stopOnFirstFailure: bool
    , executionOrder: ExecutionOrder
    }

  val default =
    { output = TextIO.stdOut
    , printPassed = true
    , stopOnFirstFailure = false
    , executionOrder = RandomOrder NONE
    }

  fun withOutput newOutput {output = _, printPassed, stopOnFirstFailure, executionOrder} =
    {output = newOutput, printPassed = printPassed, stopOnFirstFailure = stopOnFirstFailure, executionOrder = executionOrder}

  fun withPrintPassed newPrintPassed {output, printPassed = _, stopOnFirstFailure, executionOrder} =
    {output = output, printPassed = newPrintPassed, stopOnFirstFailure = stopOnFirstFailure, executionOrder = executionOrder}

  fun withStopOnFirstFailure newStopOnFirstFailure {output, printPassed, stopOnFirstFailure = _, executionOrder} =
    {output = output, printPassed = printPassed, stopOnFirstFailure = newStopOnFirstFailure, executionOrder = executionOrder}

  fun withExecutionOrder newExecutionOrder {output, printPassed, stopOnFirstFailure, executionOrder = _} =
    {output = output, printPassed = printPassed, stopOnFirstFailure = stopOnFirstFailure, executionOrder = newExecutionOrder}

  fun fromList options =
    List.foldl
      (fn (setting, config) =>
         case setting of
           Output newOutput => withOutput newOutput config
         | PrintPassed newPrintPassed => withPrintPassed newPrintPassed config
         | StopOnFirstFailure newStopOnFirstFailure => withStopOnFirstFailure newStopOnFirstFailure config
         | ExecutionOrder newExecutionOrder => withExecutionOrder newExecutionOrder config)
      default options
end
