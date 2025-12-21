signature EXPECTATION =
sig
  datatype InvalidReason = EmptyList | DuplicatedName | BadDescription

  type actual = string
  type expected = string

  datatype Reason =
    Custom
  | Invalid of InvalidReason
  | Equality of string
  | EqualityFormatter of expected * actual

  type fail = {description: string, reason: Reason}
  datatype Expectation = Pass | Fail of fail

  val pass: Expectation
  val fail: fail -> Expectation
  val toString: Expectation -> string
end

structure Expectation: EXPECTATION =
struct
  datatype InvalidReason = EmptyList | DuplicatedName | BadDescription

  type actual = string
  type expected = string

  datatype Reason =
    Custom
  | Invalid of InvalidReason
  | Equality of string
  | EqualityFormatter of expected * actual

  type fail = {description: string, reason: Reason}

  datatype Expectation = Pass | Fail of fail

  val pass = Pass
  fun fail {description: string, reason: Reason} =
    Fail {description = description, reason = reason}

  fun toString expectation =
    case expectation of
      Pass => ""
    | Fail failReason =>
        let
          val {description, reason} = failReason
        in
          case reason of
            Custom => description
          | Equality str => (description ^ ": " ^ str)
          | EqualityFormatter (expected, actual) =>
              (description ^ "\n" ^ "Expected: " ^ expected ^ "\nActual: "
               ^ actual)
          | Invalid invalid =>
              case invalid of
                EmptyList => (description ^ ": " ^ "list cannot be empty.")
              | DuplicatedName => description
              | BadDescription => (description ^ ": " ^ "bad description.")
        end
end
