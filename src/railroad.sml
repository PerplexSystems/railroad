signature RAILROAD =
sig
  structure Test: TEST
  structure Expect: EXPECT
  structure Fuzz: FUZZ
  structure Configuration: CONFIGURATION
end

structure Railroad =
struct
  structure Test = Test
  structure Expect = Expect
  structure Fuzz = Fuzz
  structure Configuration = Configuration
end
