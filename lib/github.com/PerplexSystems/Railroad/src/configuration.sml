signature CONFIGURATION =
sig
  datatype Setting = Output of TextIO.outstream | PrintPassed of bool | StopOnFirstFailure of bool

  type Configuration = {output: TextIO.outstream, printPassed: bool, stopOnFirstFailure: bool}

  val fromList: Setting list -> Configuration
  val default: Configuration
end

structure Configuration: CONFIGURATION =
struct
  datatype Setting = Output of TextIO.outstream | PrintPassed of bool | StopOnFirstFailure of bool

  type Configuration = {output: TextIO.outstream, printPassed: bool, stopOnFirstFailure: bool}

  val default = {output = TextIO.stdOut, printPassed = true, stopOnFirstFailure = false}

  fun withOutput newOutput {output = _, printPassed, stopOnFirstFailure} =
    {output = newOutput, printPassed = printPassed, stopOnFirstFailure = stopOnFirstFailure}

  fun withPrintPassed newPrintPassed {output, printPassed = _, stopOnFirstFailure} =
    {output = output, printPassed = newPrintPassed, stopOnFirstFailure = stopOnFirstFailure}

  fun withStopOnFirstFailure newStopOnFirstFailure {output, printPassed, stopOnFirstFailure = _} =
    {output = output, printPassed = printPassed, stopOnFirstFailure = newStopOnFirstFailure}

  fun fromList options =
    List.foldl
      (fn (setting, config) =>
         case setting of
           Output newOutput => withOutput newOutput config
         | PrintPassed newPrintPassed => withPrintPassed newPrintPassed config
         | StopOnFirstFailure newStopOnFirstFailure => withStopOnFirstFailure newStopOnFirstFailure config)
      default options
end
