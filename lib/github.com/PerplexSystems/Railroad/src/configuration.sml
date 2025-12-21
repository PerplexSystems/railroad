signature CONFIGURATION =
sig
  datatype Setting = Output of TextIO.outstream | PrintPassed of bool

  type Configuration = {output: TextIO.outstream, printPassed: bool}

  val fromList: Setting list -> Configuration
  val default: Configuration
end

structure Configuration: CONFIGURATION =
struct
  datatype Setting = Output of TextIO.outstream | PrintPassed of bool

  type Configuration = {output: TextIO.outstream, printPassed: bool}

  val default = {output = TextIO.stdOut, printPassed = true}

  fun withOutput newOutput {output = _, printPassed} =
    {output = newOutput, printPassed = printPassed}

  fun withPrintPassed newPrintPassed {output, printPassed = _} =
    {output = output, printPassed = newPrintPassed}

  fun fromList options =
    List.foldl
      (fn (setting, config) =>
         case setting of
           Output newOutput => withOutput newOutput config
         | PrintPassed newPrintPassed => withPrintPassed newPrintPassed config)
      default options
end
