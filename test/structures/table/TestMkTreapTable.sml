structure TestMkTreapTable :>
sig
  val run : unit -> unit
end
=
struct
  structure RankedIntKey : HASHKEY =
    struct
      type t = int Ranked.ranked
      val compare = Ranked.compareRank
      val equal = Ranked.equalRank
      val toString = Ranked.show Show.int
      val hash = Ranked.hashRank
    end

  structure RankedIntKeyGen : RANDKEY =
    struct
      type t = int Ranked.ranked
      val gen = Ranked.gen Gen.int
      fun cogen g = Ranked.cogen Gen.coint g
    end

  structure Table = MkTreapTable (structure Key = RankedIntKey)

  structure TestSet = MkTestSet
    (val structName = "MkTreapTable.Set"
     structure Set = Table.Set
     structure KeyGen = RankedIntKeyGen
     val keySourceEqual = Ranked.equal op=)

  fun run () =
    (
      print ("Testing MkTreapTable..." ^ "\n")
    ;
      TestSet.run ()
    )
end
