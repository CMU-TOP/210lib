structure TestTreeSequence :
sig
  val run : unit -> unit
end =
struct
  structure S = TreeSequence
  structure TestCommon = MkTestSequence
    (val structName = "TreeSequence"
     structure Seq = S)

  fun checkSplitMid (S.EMPTY) = true
    | checkSplitMid (S.ONE x) = true
    | checkSplitMid (S.PAIR (s1, s2)) =
        (S.length s1 = S.length s2 orelse S.length s1 + 1 = S.length s2)

  fun runSpecific () =
    Test.check "TreeSequence.splitMid (sizes)"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.implies (fn l => length l >= 2,
        Test.satisfies (fn l => checkSplitMid (S.splitMid (S.% l)))))

  fun run () =
    let
      val _ = print ("Testing TreeSequence..." ^ "\n")

      (* Tests for all libraries implementing SEQUENCE *)
      val _ = print ("\n" ^ "Common SEQUENCE tests:" ^ "\n")
      val _ = TestCommon.run ()

      (* Other TreeSequence-specific tests *)
      val _ = print ("\n" ^ "TreeSequence-specefic tests:" ^ "\n")
      val _ = runSpecific ()
    in ()
    end
end
