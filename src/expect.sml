signature EXPECT =
sig
  type 'a expected = 'a
  type 'a actual = 'a

  type 'a comparer = ('a expected * 'a actual) -> General.order
  type 'a formatter = 'a -> string

  val pass: Expectation.Expectation
  val fail: string -> Expectation.Expectation
  val onFail: string -> Expectation.Expectation -> Expectation.Expectation
  val all: Expectation.Expectation list -> Expectation.Expectation
  val any: Expectation.Expectation list -> Expectation.Expectation

  val isTrue: bool actual -> Expectation.Expectation
  val isFalse: bool actual -> Expectation.Expectation

  val some: 'a option actual -> Expectation.Expectation
  val none: 'a option actual -> Expectation.Expectation

  val equal: 'a comparer -> 'a expected -> 'a actual -> Expectation.Expectation
  val equalFmt: 'a comparer
                -> 'a formatter
                -> 'a expected
                -> 'a actual
                -> Expectation.Expectation

  val notEqual: 'a comparer
                -> 'a expected
                -> 'a actual
                -> Expectation.Expectation
  val notEqualFmt: 'a comparer
                   -> 'a formatter
                   -> 'a expected
                   -> 'a actual
                   -> Expectation.Expectation

  val atMost: 'a comparer -> 'a expected -> 'a actual -> Expectation.Expectation
  val atMostFmt: 'a comparer
                 -> 'a formatter
                 -> 'a expected
                 -> 'a actual
                 -> Expectation.Expectation

  val atLeast: 'a comparer
               -> 'a expected
               -> 'a actual
               -> Expectation.Expectation
  val atLeastFmt: 'a comparer
                  -> 'a formatter
                  -> 'a expected
                  -> 'a actual
                  -> Expectation.Expectation

  val less: 'a comparer -> 'a expected -> 'a actual -> Expectation.Expectation
  val lessFmt: 'a comparer
               -> 'a formatter
               -> 'a expected
               -> 'a actual
               -> Expectation.Expectation

  val greater: 'a comparer
               -> 'a expected
               -> 'a actual
               -> Expectation.Expectation
  val greaterFmt: 'a comparer
                  -> 'a formatter
                  -> 'a expected
                  -> 'a actual
                  -> Expectation.Expectation

  datatype FloatingPointTolerance =
    Absolute of real
  | Relative of real
  | AbsoluteOrRelative of (real * real)
end

structure Expect: EXPECT =
struct
  open Expectation

  type 'a expected = 'a
  type 'a actual = 'a

  type 'a comparer = ('a actual * 'a expected) -> General.order
  type 'a formatter = 'a -> string

  val pass = Expectation.Pass
  fun fail str = Expectation.fail {description = str, reason = Custom}
  fun onFail str expectation =
    case expectation of
      Pass => expectation
    | Fail _ => fail str

  fun all expectations =
    case expectations of
      [] =>
        Expectation.fail
          { description = "Expect.all"
          , reason = Equality "Expect.all requires at least one expectation."
          }
    | _ =>
        case List.find (fn e => e <> Pass) expectations of
          NONE => Pass
        | SOME failure => failure

  fun any expectations =
    case expectations of
      [] =>
        Expectation.fail
          { description = "Expect.any"
          , reason = Equality "Expect.any requires at least one expectation."
          }
    | _ =>
        if List.exists (fn e => e = Pass) expectations then
          Pass
        else
          Expectation.fail
            { description = "Expect.any"
            , reason = Equality "None of the expectations passed."
            }

  fun isTrue actual =
    if actual then
      Pass
    else
      Expectation.fail
        { description = "Expect.isTrue"
        , reason = Equality "The value provided is not true."
        }

  fun isFalse actual =
    if Bool.not actual then
      Pass
    else
      Expectation.fail
        { description = "Expect.isFalse"
        , reason = Equality "The value provided is not false."
        }

  fun some actual =
    case actual of
      SOME _ => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.some"
          , reason = Equality "The value provided is not SOME."
          }

  fun none actual =
    case actual of
      NONE => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.none"
          , reason = Equality "The value provided is not NONE."
          }

  fun equal comparer expected actual =
    case comparer (actual, expected) of
      EQUAL => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.equal"
          , reason = Equality
              "The value provided is not equal to the expected one."
          }

  fun equalFmt comparer formatter expected actual =
    case equal comparer expected actual of
      Pass => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.equalFmt"
          , reason = EqualityFormatter
              ((formatter expected), (formatter actual))
          }

  fun notEqual comparer expected actual =
    case comparer (actual, expected) of
      EQUAL =>
        Expectation.fail
          { description = "Expect.notEqual"
          , reason = Equality "The value provided is equal to the expected one."
          }
    | _ => Pass

  fun notEqualFmt comparer formatter expected actual =
    case notEqual comparer expected actual of
      Pass => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.notEqualFmt"
          , reason = EqualityFormatter
              ((formatter expected), (formatter actual))
          }

  fun atMost comparer expected actual =
    case comparer (actual, expected) of
      GREATER =>
        Expectation.fail
          { description = "Expect.atMost"
          , reason = Equality
              "The value provided is greater than the expected one."
          }
    | _ => Pass

  fun atMostFmt comparer formatter expected actual =
    case atMost comparer expected actual of
      Pass => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.atMostFmt"
          , reason = EqualityFormatter
              ((formatter expected), (formatter actual))
          }

  fun atLeast comparer expected actual =
    case comparer (actual, expected) of
      LESS =>
        Expectation.fail
          { description = "Expect.notEqual"
          , reason = Equality
              "The value provided is less than the expected one."
          }
    | _ => Pass

  fun atLeastFmt comparer formatter expected actual =
    case atLeast comparer expected actual of
      Pass => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.atLeastFmt"
          , reason = EqualityFormatter
              ((formatter expected), (formatter actual))
          }

  fun less comparer expected actual =
    case comparer (actual, expected) of
      LESS => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.less"
          , reason = Equality
              "The value provided is not less than the expected one."
          }

  fun lessFmt comparer formatter expected actual =
    case less comparer expected actual of
      Pass => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.lessFmt"
          , reason = EqualityFormatter
              ((formatter expected), (formatter actual))
          }

  fun greater comparer expected actual =
    case comparer (actual, expected) of
      GREATER => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.greater"
          , reason = Equality
              "The value provided is not greater than the expected one."
          }

  fun greaterFmt comparer formatter expected actual =
    case greater comparer expected actual of
      Pass => Pass
    | _ =>
        Expectation.fail
          { description = "Expect.greaterFmt"
          , reason = EqualityFormatter
              ((formatter expected), (formatter actual))
          }

  datatype FloatingPointTolerance =
    Absolute of real
  | Relative of real
  | AbsoluteOrRelative of (real * real)
end
