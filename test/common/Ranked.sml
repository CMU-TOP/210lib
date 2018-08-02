structure Ranked :>
sig
  type 'a ranked
  val compareRank : 'a ranked * 'a ranked -> order
  val equalRank : 'a ranked * 'a ranked -> bool
  val equal : ('a * 'a -> bool) -> ('a ranked * 'a ranked -> bool)
  val gen : 'a Gen.gen -> 'a ranked Gen.gen
  val cogen : ('a, 'b) Gen.co -> ('a ranked, 'b) Gen.co
  val show : ('a -> string) -> ('a ranked -> string)
  val hashRank : 'a ranked -> int (* for HASHKEY *)
end
=
struct
  type 'a ranked = int * 'a
  val genRank = Gen.choose' [(3, Gen.range (~5, 5)), (1, Gen.int)]

  fun compareRank ((x, _), (y, _)) = Int.compare (x, y)
  fun equalRank ((x, _), (y, _)) = (x = y)
  fun equal eq = Eq.pair (op=, eq)

  fun gen g = Gen.pair (genRank, g)
  fun cogen co = Gen.copair (Gen.coint, co)
  fun show s (r, x) = s x ^ " ranked " ^ Show.int r
  fun hashRank (r, _) = IntElt.hash r
end
