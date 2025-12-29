structure Internal =
struct
  datatype ('a, 'e) Result = OK of 'a | ERROR of 'e

  structure List =
  struct
    open List

    fun concatMap f l =
      List.concat (List.map f l)

    fun mergeSort _ [] = []
      | mergeSort _ [x] = [x]
      | mergeSort cmp xs =
          let
            val n = List.length xs div 2
            val left = List.take (xs, n)
            val right = List.drop (xs, n)
            fun merge ([], ys) = ys
              | merge (xs_, []) = xs_
              | merge (x :: xs_, y :: ys) =
                  if cmp (x, y) = GREATER then y :: merge (x :: xs_, ys)
                  else x :: merge (xs_, y :: ys)
          in
            merge (mergeSort cmp left, mergeSort cmp right)
          end
  end

  structure String =
  struct
    open String

    val trim = let open Substring
               in string o dropl Char.isSpace o dropr Char.isSpace o full
               end
  end

  datatype Test =
    UnitTest of (unit -> Expectation.Expectation)
  | Labeled of (string * Test)
  | Skipped of Test
  | Focused of Test
  | Batch of Test list

  fun failnow record =
    UnitTest (fn _ => Expectation.fail record)

  val blankDescriptionFail =
    let
      open Expectation
    in
      failnow
        { description =
            "This test has a blank description. Let's give it a useful one!"
        , reason = Invalid BadDescription
        }
    end

  fun duplicatedNames tests =
    let
      fun names test =
        case test of
          Labeled (description, _) => [description]
        | Batch subtests => List.concatMap names subtests
        | UnitTest _ => []
        | Skipped subTest => names subTest
        | Focused subTest => names subTest

      (* we don't have a Set structure for now ;( *)
      fun insertIfNotExists item l =
        if List.exists (fn x => x = item) l then l else item :: l

      fun accumDuplicates (newName, (dups, uniques)) =
        if List.exists (fn unique => unique = newName) uniques then
          (insertIfNotExists newName dups, uniques)
        else
          (dups, insertIfNotExists newName uniques)

      val accumulatedDuplicates = List.concatMap names tests

      val (accDups, accUniques) =
        List.foldl accumDuplicates ([], []) accumulatedDuplicates
    in
      if List.null accDups then OK accUniques else ERROR accDups
    end
end
