signature RANDOM =
sig
    type rand
    val rand : unit -> rand
    val randFromSeed : int -> rand
    val getSeed : rand -> int
    val randInt : rand -> int * int -> int
    val randReal : rand -> real
    val randRange : rand -> int -> int -> int
    val setSeed : rand -> int -> unit
end

structure TimeBasedRandom : RANDOM =
struct
    type rand = int ref

    (* LCG parameters (same as glibc) *)
    val a : LargeInt.int = 1103515245
    val c : LargeInt.int = 12345
    val m : LargeInt.int = 0x7FFFFFFF

    fun rand () =
        let
            val now = Time.now()
            val nano = Time.toNanoseconds now
            val seed = LargeInt.toInt (nano mod m)
        in
            ref seed
        end

    fun randFromSeed seed = ref seed

    fun getSeed r = !r

    fun next r =
        let
            val current = LargeInt.fromInt (!r)
            val n = (current * a + c) mod m
        in
            r := LargeInt.toInt n; LargeInt.toInt n
        end

    fun randInt r (lo, hi) =
        lo + (next r mod (hi - lo + 1))

    fun randReal r =
        Real.fromInt (next r) / Real.fromLargeInt m

    fun randRange r lo hi = randInt r (lo, hi)

    fun setSeed r seed = r := seed
end
