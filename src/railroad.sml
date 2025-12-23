signature RAILROAD =
sig
  structure Test: TEST
  structure Expect: EXPECT
  structure Configuration: CONFIGURATION
end

structure Railroad =
struct
  structure Test = Test
  structure Expect = Expect
  structure Configuration = Configuration
end
