(*
 * We also assume @code{Seq.equal} and @code{Seq.%} are correct
 * and @code{Seq.%} is surjective for testing @code{Seq}.
 *
 * We assume @code{Key}, @code{Seq.toList}, @code{Seq.%} and @code{toSeq}
 * are correct and both @code{fromSeq} and @code{Seq.%} are surjective
 * for the rest.
 *
 * We did not test @code{toString} which breaks parametricity.
 * We can only test self-consistency for @code{reduceKey} and @code{iterateKey}.
 *
 * A complete test should at least involve @code{ranked int}.
 *)
functor MkTestSet
  (val structName : string
   structure Set : SET
   structure KeyGen : RANDKEY where type t = Set.Key.t
   (* We might intentionally hide tracing information about Key from Set
    * while testing.  keyFullyEqual will check even the hidden tracing
    * information in keys.  For example if Key.t = 'a ranked, then Key.equal
    * will only check the rank but keySourceSource will check the rank
    * and the ranked value. *)
   val keySourceEqual : Set.Key.t * Set.Key.t -> bool) :>
sig
  val run : unit -> unit
end =
struct
  fun check funName = Test.check (structName ^ "." ^ funName)

  (* the sequence structure under testing *)
  structure S = Set
  structure Key = S.Key
  structure Seq = S.Seq
  (* the reference implementation for tested functions *)
  structure DLS = DumbListSequence
  structure D = MkDumbListSet
    (structure Key = Key
     structure Seq = DLS)

  (* testing the sequence structure first *)
  structure TestSeq = MkTestSequence
    (val structName = structName ^ ".Seq"
     structure Seq = Seq)

  (* the relation: assumes Seq.toList, S.toSeq *)
  infix 4 ~= (* the precedence of op= *)
  fun bagSourceEq ([], []) = true
    | bagSourceEq (x :: l, r) =
        let
          val (lsame, lrest) = List.partition (fn x' => keySourceEqual (x, x')) l
          val (rsame, rrest) = List.partition (fn x' => keySourceEqual (x, x')) r
        in
          1 + List.length lsame = List.length rsame andalso bagSourceEq (lrest, rrest)
        end
    | bagSourceEq _ = false
  fun s ~= l = bagSourceEq (Seq.toList (S.toSeq s), D.toSeq l)

  (* misc *)
  val % = Set.fromSeq o Seq.%
  val ` = D.fromSeq
  fun snoc (l, x) = x :: l
  val genKey = KeyGen.gen
  val cogenKey = KeyGen.cogen
  val showKey = Key.toString

  (******************************************************************)

  fun run () =
  (
    (* test the Seq *)
    TestSeq.run ()
  ;
    (* val size : set -> int *)
    check "size"
      (Gen.list genKey, Show.list showKey)
      (Test.compare op= (fn l => (S.size (% l),
                                  D.size (` l))))
  ;
    (* val toString : set -> string *)
    () (* untested *)
  ;
    (* val empty : unit -> set *)
    check "empty"
      (Gen.unit, Show.unit)
      (Test.compare op~= (fn () => (S.empty (),
                                    D.empty ())))
  ;
    (* val singleton : Key.t -> set *)
    check "singleton"
      (genKey, showKey)
      (Test.compare op~= (fn k => (S.singleton k,
                                   D.singleton k)))
  ;
    (* val fromSeq : Key.t Seq.t -> set *)
    check "fromSeq"
      (Gen.list genKey, Show.list showKey)
      (Test.compare op~= (fn l => (S.fromSeq (Seq.% l),
                                   D.fromSeq l)))
  ;
    (* val find : set -> Key.t -> bool *)
    check "find"
      (Gen.pair (Gen.list genKey, genKey), Show.pair (Show.list showKey, showKey))
      (Test.compare op= (fn (l, i) => (S.find (% l) i,
                                       D.find (` l) i)))
  ;
    (* val insert : set -> Key.t -> bool *)
    check "insert"
      (Gen.pair (Gen.list genKey, genKey), Show.pair (Show.list showKey, showKey))
      (Test.compare op~= (fn (l, i) => (S.insert (% l, i),
                                        D.insert (` l, i))))
  ;
    (* val delete : set -> Key.t -> bool *)
    check "delete"
      (Gen.pair (Gen.list genKey, genKey), Show.pair (Show.list showKey, showKey))
      (Test.compare op~= (fn (l, i) => (S.delete (% l, i),
                                        D.delete (` l, i))))
  ;
    (* val filterKey : (Key.t -> bool) -> set -> set *)
    check "filterKey"
      (Gen.pair (Gen.function (cogenKey, Gen.bool), Gen.list genKey),
       Show.pair (Show.list Show.bool, Show.list showKey) o (fn (f, l) => (List.map f l, l)))
      (Test.compare op~= (fn (f, l) => (S.filterKey f (% l),
                                        D.filterKey f (` l))))
  ;
    (* val reduceKey : (Key.t * Key.t -> Key.t) -> Key.t -> set -> Key.t *)
    (* XXX how to show the function? *)
    check "reduceKey"
      (Gen.pair3 (Gen.function (Gen.copair (cogenKey, cogenKey), genKey), genKey, Gen.list genKey),
       Show.pair3 (Show.string, showKey, Show.list showKey) o (fn (f, k, l) => ("-", k, l)))
      (Test.compare keySourceEqual
        (fn (f, k, l) => (S.reduceKey f k (% l), Seq.reduce f k (S.toSeq (% l)))))
  ;
    (* val iterateKey : ('a * Key.t -> 'a) -> 'a -> set -> 'a *)
    check "iterateKey"
      (Gen.list genKey, Show.list showKey)
      (Test.compare (ListPair.allEq keySourceEqual)
        (fn l => (S.iterateKey snoc [] (% l), Seq.iterate snoc [] (S.toSeq (% l)))))
  ;
    (* val union : set * set -> set *)
    check "union"
      (Gen.pair (Gen.list genKey, Gen.list genKey),
       Show.pair (Show.list showKey, Show.list showKey))
      (Test.compare op~= (fn (l1, l2) => (S.union (% l1, % l2),
                                          D.union (` l1, ` l2))))
  ;
    (* val intersection : set * set -> set *)
    check "intersection"
      (Gen.pair (Gen.list genKey, Gen.list genKey),
       Show.pair (Show.list showKey, Show.list showKey))
      (Test.compare op~= (fn (l1, l2) => (S.intersection (% l1, % l2),
                                          D.intersection (` l1, ` l2))))
  ;
    (* val difference : set * set -> set *)
    check "difference"
      (Gen.pair (Gen.list genKey, Gen.list genKey),
       Show.pair (Show.list showKey, Show.list showKey))
      (Test.compare op~= (fn (l1, l2) => (S.difference (% l1, % l2),
                                          D.difference (` l1, ` l2))))
  ;
    (* val $ : Key.t -> set *)
    check "$"
      (genKey, showKey)
      (Test.compare op~= (fn k => (S.$ k,
                                   D.$ k)))
  )

end
