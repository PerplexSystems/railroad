signature FUZZ =
sig
  type 'a t

  val int: int t
  val intRange: int -> int -> int t
  val positiveInt: int t
  val negativeInt: int t
  val bool: bool t
  val char: char t
  val alphaChar: char t
  val alphaNumChar: char t
  val real: real t
  val realRange: real -> real -> real t
  val unit: unit t

  val string: string t
  val stringOfLength: int -> string t
  val stringOfLengthBetween: int -> int -> string t
  val alphaString: string t

  val list: 'a t -> 'a list t
  val listOfLength: int -> 'a t -> 'a list t
  val listOfLengthBetween: int -> int -> 'a t -> 'a list t
  val option: 'a t -> 'a option t
  val pair: 'a t -> 'b t -> ('a * 'b) t
  val triple: 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  val constant: 'a -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  val map3: ('a -> 'b -> 'c -> 'd) -> 'a t -> 'b t -> 'c t -> 'd t
  val andThen: ('a -> 'b t) -> 'a t -> 'b t
  val oneOf: 'a t list -> 'a t
  val frequency: (int * 'a t) list -> 'a t
  val filter: ('a -> bool) -> 'a t -> 'a t

  val custom: (TimeBasedRandom.rand -> 'a) -> ('a -> 'a list) -> 'a t
  val noShrink: 'a t -> 'a t

  val run: int
           -> 'a t
           -> ('a -> Expectation.Expectation)
           -> Expectation.Expectation
end

structure Fuzz :> FUZZ =
struct
  structure Random = TimeBasedRandom

  (* Shrink tree: a value with lazily-computed shrunk alternatives, or a failure *)
  datatype 'a tree =
    Node of 'a * (unit -> 'a tree list)
  | Failed of string

  (* A generator produces a shrink tree from random state *)
  type 'a t = Random.rand -> 'a tree

  val maxShrinks = 100

  fun rootOf (Node (v, _)) = v
    | rootOf (Failed msg) =
        rootOf (Failed msg)

  fun shrinks (Node (_, s)) = s ()
    | shrinks (Failed _) = []

  fun mapTree f (Node (v, s)) =
        Node (f v, fn () => List.map (mapTree f) (s ()))
    | mapTree _ (Failed msg) = Failed msg

  fun shrinkInt n =
    if n = 0 then
      []
    else
      let
        val half = n div 2
        val candidates =
          if n > 0 then 0 :: (if half > 0 then [half] else []) @ [n - 1]
          else 0 :: (if half < 0 then [half] else []) @ [n + 1]
      in
        List.filter (fn x => x <> n) candidates
      end

  fun shrinkBool b =
    if b then [false] else []

  fun shrinkChar c =
    let val ord = Char.ord c
    in if ord > 97 then [chr 97] else []
    end

  fun shrinkReal r =
    if Real.== (r, 0.0) then [] else [0.0, r / 2.0]

  fun shrinkList shrinkElem xs =
    case xs of
      [] => []
    | [x] => [] :: List.map (fn v => [v]) (shrinkElem x)
    | _ =>
        let
          val len = List.length xs
          val half = len div 2
          val front = List.take (xs, half)
          val back = List.drop (xs, half)
          fun shrinkOne (y :: ys) =
                List.map (fn v => v :: ys) (shrinkElem y)
                @ List.map (fn vs => y :: vs) (shrinkOne ys)
            | shrinkOne [] = []
        in
          [front, back] @ shrinkOne xs
        end

  fun shrinkString s =
    List.map String.implode (shrinkList shrinkChar (String.explode s))

  fun shrinkOption shrinkElem opt =
    case opt of
      NONE => []
    | SOME x => NONE :: List.map SOME (shrinkElem x)

  (* Build a shrink tree from a value and shrink function *)
  fun buildTree shrink v =
    Node (v, fn () => List.map (buildTree shrink) (shrink v))

  (* generators *)
  fun int rand =
    let val v = Random.randInt rand (~1000000, 1000000)
    in buildTree shrinkInt v
    end

  fun intRange lo hi rand =
    let
      val v = Random.randInt rand (lo, hi)
      fun shrinkInRange n =
        List.filter (fn x => x >= lo andalso x <= hi) (shrinkInt n)
    in
      buildTree shrinkInRange v
    end

  fun positiveInt rand =
    let
      val v = Random.randInt rand (1, 1000000)
      fun shrinkPos n =
        List.filter (fn x => x >= 1) (shrinkInt n)
    in
      buildTree shrinkPos v
    end

  fun negativeInt rand =
    let
      val v = Random.randInt rand (~1000000, ~1)
      fun shrinkNeg n =
        List.filter (fn x => x <= ~1) (shrinkInt n)
    in
      buildTree shrinkNeg v
    end

  fun bool rand =
    let val v = Random.randInt rand (0, 1) = 1
    in buildTree shrinkBool v
    end

  fun char rand =
    let val v = chr (Random.randInt rand (32, 126))
    in buildTree shrinkChar v
    end

  fun alphaChar rand =
    let
      val isUpper = Random.randInt rand (0, 1) = 1
      val v =
        if isUpper then chr (Random.randInt rand (65, 90))
        else chr (Random.randInt rand (97, 122))
    in
      buildTree shrinkChar v
    end

  fun alphaNumChar rand =
    let
      val choice = Random.randInt rand (0, 2)
      val v =
        case choice of
          0 => chr (Random.randInt rand (48, 57))
        | 1 => chr (Random.randInt rand (65, 90))
        | _ => chr (Random.randInt rand (97, 122))
    in
      buildTree shrinkChar v
    end

  fun real rand =
    let
      val sign = if Random.randInt rand (0, 1) = 0 then 1.0 else ~1.0
      val v = sign * Random.randReal rand * 1000000.0
    in
      buildTree shrinkReal v
    end

  fun realRange lo hi rand =
    let
      val range = hi - lo
      val v = lo + Random.randReal rand * range
      fun shrinkInRange r =
        List.filter (fn x => x >= lo andalso x <= hi) (shrinkReal r)
    in
      buildTree shrinkInRange v
    end

  fun unit _ =
    Node ((), fn () => [])

  (* String generators *)
  fun stringOfLength len rand =
    let
      fun genChars 0 = []
        | genChars n =
            let val c = chr (Random.randInt rand (32, 126))
            in c :: genChars (n - 1)
            end
      val v = String.implode (genChars len)
    in
      buildTree shrinkString v
    end

  fun stringOfLengthBetween lo hi rand =
    let val len = Random.randInt rand (lo, hi)
    in stringOfLength len rand
    end

  fun string rand =
    stringOfLengthBetween 0 20 rand

  fun alphaString rand =
    let
      val len = Random.randInt rand (0, 20)
      fun genChars 0 = []
        | genChars n =
            let
              val isUpper = Random.randInt rand (0, 1) = 1
              val c =
                if isUpper then chr (Random.randInt rand (65, 90))
                else chr (Random.randInt rand (97, 122))
            in
              c :: genChars (n - 1)
            end
      val v = String.implode (genChars len)
    in
      buildTree shrinkString v
    end

  (* Collection generators *)
  fun listOfLength len gen rand =
    let
      fun genItems 0 = []
        | genItems n =
            gen rand :: genItems (n - 1)
      val trees = genItems len
      val values = List.map rootOf trees

      fun shrinkWithTrees ts =
        case ts of
          [] => []
        | [t] =>
            let val shrunkSingles = List.map (fn t' => [rootOf t']) (shrinks t)
            in [] :: shrunkSingles
            end
        | _ =>
            let
              val l = List.length ts
              val half = l div 2
              val front = List.take (ts, half)
              val back = List.drop (ts, half)
            in
              [List.map rootOf front, List.map rootOf back]
            end
    in
      Node (values, fn () =>
        List.map (buildTree (shrinkList (fn _ => []))) (shrinkWithTrees trees))
    end

  fun listOfLengthBetween lo hi gen rand =
    let val len = Random.randInt rand (lo, hi)
    in listOfLength len gen rand
    end

  fun list gen rand =
    listOfLengthBetween 0 10 gen rand

  fun option gen rand =
    let
      val isSome = Random.randInt rand (0, 3) > 0
    in
      if isSome then
        let
          val innerTree = gen rand
          val v = SOME (rootOf innerTree)
          fun shrinkOpt () =
            Node (NONE, fn () => [])
            :: List.map (mapTree SOME) (shrinks innerTree)
        in
          Node (v, shrinkOpt)
        end
      else
        Node (NONE, fn () => [])
    end

  fun pair genA genB rand =
    let
      val treeA = genA rand
      val treeB = genB rand
      val v = (rootOf treeA, rootOf treeB)

      fun shrinkPair () =
        let
          val shrunkA =
            List.map (fn t => (rootOf t, rootOf treeB)) (shrinks treeA)
          val shrunkB =
            List.map (fn t => (rootOf treeA, rootOf t)) (shrinks treeB)
        in
          List.map (buildTree (fn _ => [])) (shrunkA @ shrunkB)
        end
    in
      Node (v, shrinkPair)
    end

  fun triple genA genB genC rand =
    let
      val treeA = genA rand
      val treeB = genB rand
      val treeC = genC rand
      val v = (rootOf treeA, rootOf treeB, rootOf treeC)

      fun shrinkTriple () =
        let
          val a = rootOf treeA
          val b = rootOf treeB
          val c = rootOf treeC
          val shrunkA = List.map (fn t => (rootOf t, b, c)) (shrinks treeA)
          val shrunkB = List.map (fn t => (a, rootOf t, c)) (shrinks treeB)
          val shrunkC = List.map (fn t => (a, b, rootOf t)) (shrinks treeC)
        in
          List.map (buildTree (fn _ => [])) (shrunkA @ shrunkB @ shrunkC)
        end
    in
      Node (v, shrinkTriple)
    end

  (* Combinators *)
  fun constant v _ =
    Node (v, fn () => [])

  fun map f gen rand =
    mapTree f (gen rand)

  fun map2 f genA genB rand =
    let
      val treeA = genA rand
      val treeB = genB rand
      val v = f (rootOf treeA) (rootOf treeB)

      fun shrinkMapped () =
        let
          val b = rootOf treeB
          val a = rootOf treeA
          val shrunkFromA = List.map (fn t => f (rootOf t) b) (shrinks treeA)
          val shrunkFromB = List.map (fn t => f a (rootOf t)) (shrinks treeB)
        in
          List.map (buildTree (fn _ => [])) (shrunkFromA @ shrunkFromB)
        end
    in
      Node (v, shrinkMapped)
    end

  fun map3 f genA genB genC rand =
    let
      val treeA = genA rand
      val treeB = genB rand
      val treeC = genC rand
      val v = f (rootOf treeA) (rootOf treeB) (rootOf treeC)

      fun shrinkMapped () =
        let
          val a = rootOf treeA
          val b = rootOf treeB
          val c = rootOf treeC
          val shrunkFromA = List.map (fn t => f (rootOf t) b c) (shrinks treeA)
          val shrunkFromB = List.map (fn t => f a (rootOf t) c) (shrinks treeB)
          val shrunkFromC = List.map (fn t => f a b (rootOf t)) (shrinks treeC)
        in
          List.map (buildTree (fn _ => []))
            (shrunkFromA @ shrunkFromB @ shrunkFromC)
        end
    in
      Node (v, shrinkMapped)
    end

  fun andThen f gen rand =
    let
      val treeA = gen rand
      val a = rootOf treeA
      val treeB = f a rand
    in
      treeB
    end

  fun oneOf gens rand =
    case gens of
      [] => Failed "Fuzz.oneOf requires a non-empty list of generators"
    | _ =>
        let
          val idx = Random.randInt rand (0, List.length gens - 1)
          val gen = List.nth (gens, idx)
        in
          gen rand
        end

  fun frequency weighted rand =
    case weighted of
      [] => Failed "Fuzz.frequency requires a non-empty list of generators"
    | _ =>
        let
          val total = List.foldl (fn ((w, _), acc) => w + acc) 0 weighted
        in
          if total <= 0 then
            Failed "Fuzz.frequency: weights must be positive"
          else
            let
              val target = Random.randInt rand (1, total)

              fun pick ((w, gen) :: rest) remaining =
                    if remaining <= w then gen rand
                    else pick rest (remaining - w)
                | pick [] _ =
                    Failed "Fuzz.frequency: internal error"
            in
              pick weighted target
            end
        end

  fun filter pred gen rand =
    let
      val maxAttempts = 100

      fun attempt 0 =
            Failed
              "Fuzz.filter could not find a matching value after 100 attempts"
        | attempt n =
            let
              val tree = gen rand
            in
              case tree of
                Failed msg => Failed msg
              | Node _ => if pred (rootOf tree) then tree else attempt (n - 1)
            end

      fun filterShrinks (Node (v, s)) =
            Node (v, fn () =>
              List.mapPartial
                (fn t =>
                   case t of
                     Failed _ => NONE
                   | Node _ =>
                       if pred (rootOf t) then SOME (filterShrinks t) else NONE)
                (s ()))
        | filterShrinks (Failed msg) = Failed msg
    in
      case attempt maxAttempts of
        Failed msg => Failed msg
      | tree => filterShrinks tree
    end

  (* Custom generators *)
  fun custom generate shrink rand =
    let val v = generate rand
    in buildTree shrink v
    end

  fun noShrink gen rand =
    case gen rand of
      Failed msg => Failed msg
    | Node (v, _) => Node (v, fn () => [])

  (* === Test execution with shrinking === *)

  fun runWithShrinking tree testFn =
    case tree of
      Failed msg =>
        Expectation.Fail {description = msg, reason = Expectation.Custom}
    | Node _ =>
        let
          fun findSmallest (Node (v, getShrinks)) failingExpectation shrinkCount =
                if shrinkCount >= maxShrinks then
                  (v, failingExpectation)
                else
                  let
                    val shrunkTrees = getShrinks ()

                    fun tryNext [] = (v, failingExpectation)
                      | tryNext ((Failed _) :: rest) = tryNext rest
                      | tryNext ((t as Node _) :: rest) =
                          let
                            val result = testFn (rootOf t)
                          in
                            case result of
                              Expectation.Pass => tryNext rest
                            | Expectation.Fail _ =>
                                findSmallest t result (shrinkCount + 1)
                          end
                  in
                    tryNext shrunkTrees
                  end
            | findSmallest (Failed msg) failingExpectation shrinkCount =
                findSmallest (Failed msg) failingExpectation shrinkCount

          val initialResult = testFn (rootOf tree)
        in
          case initialResult of
            Expectation.Pass => Expectation.Pass
          | Expectation.Fail _ =>
              let val (_, finalExp) = findSmallest tree initialResult 0
              in finalExp
              end
        end

  fun run runs gen testFn =
    let
      val rand = Random.rand ()

      fun runOne () =
        let val tree = gen rand
        in runWithShrinking tree testFn
        end

      fun loop 0 = Expectation.Pass
        | loop n =
            case runOne () of
              Expectation.Pass => loop (n - 1)
            | fail => fail
    in
      loop runs
    end
end
