(*
 * We assume @code{fromSeq} and @code{equal} are correct,
 * and @code{fromSeq} is surjective.
 *)
structure TestSequenceShared =
struct
  datatype caught_exceptions = RANGE | SIZE
end
functor MkSequenceWrapper (Seq : SEQUENCE) =
struct
  open TestSequenceShared
  fun catch Seq.Range = RANGE
    | catch Seq.Size = SIZE
    | catch e = raise e

  (* Function f takes two inputs. *)
  fun wrap2 f = Test.wrap2 catch f

  fun wrap f = Test.wrap catch f
end
functor MkTestSequence
  (val structName : string
   structure Seq : SEQUENCE) :>
sig
  val run : unit -> unit
end =
struct
  open TestSequenceShared
  fun check funName = Test.check (structName ^ "." ^ funName)

  (* the sequence structure under testing *)
  structure S = Seq
  (* the reference implementation for tested functions *)
  structure D = DumbListSequence

  (* the relation: assumes S.equal, S.% *)
  fun related eq (s, l) =
    let
(*      val _ = print "related: COMPARING RESULTS\n" *)
      val testPassed =  S.equal eq (s, S.% l)
(*
      val _ = print ("related: END OF COMPARISON: Test passed =  " ^ Bool.toString testPassed ^ "\n"
)
*)
    in
      testPassed
      end

  fun relatedWithPrint eq prettyp (s, l) =
      let
        val () = print ((prettyp (S.toList s)) ^ " Seq\n")
        val () = print ((prettyp l) ^ " DumbSeq\n")
      val testPassed =  S.equal eq (s, S.% l)
     in
       (if testPassed
        then print "(passed)\n"
        else print "(failed)\n");
       testPassed
    end
  infix 4 ~= (* the precedence of op= *)

  fun s ~= l =
    let
(*
    val _ = print "COMPARING RESULTS\n"
*)
      val r = S.% l
(*
      val _ = print ("COMPARE: computed sequence length = " ^ (Int.toString (S.length s)) ^ "\n")
      val _ = print ("COMPARE: expected sequence length = " ^ (Int.toString (S.length r)) ^ "\n")
*)
      val testPassed =  S.equal op= (s, r)
(*
      val _ = print ("END OF COMPARISON: Test passed =  " ^ Bool.toString testPassed ^ "\n")
*)
    in
      testPassed
    end
  (* misc *)
  val % = S.%
  fun snoc (x, y) = y :: x (* different from the usual snoc *)
  fun %% l = % (List.map % l)
  val id = fn x => x
  val $ = D.singleton

  (* order and sorting  *)
  (* Ranked is for testing stability of sorting.
   *
   *)

  val genIntList = Gen.list Gen.int
  val genSortedIntList = Gen.map (D.sort Int.compare) genIntList
  val genRankedIntList = Gen.list (Ranked.gen Gen.int)
  val genSortedRankedIntList = Gen.map (D.sort Ranked.compareRank) genRankedIntList

  (* helper functions for testing *)
  fun listviewEq (S.NIL, D.NIL) = true
    | listviewEq (S.CONS (x, s), D.CONS (y, l)) = x = y andalso s ~= l
    | listviewEq _ = false

  fun keyValuesEq keyEq (s, l) =
    related (Eq.pair (keyEq, S.equal op=)) (s, List.map (fn (k, vs) => (k, % vs)) l)

  fun checkSplitMid (S.EMPTY, []) = true
    | checkSplitMid (S.ONE y, [x]) = x = y
    | checkSplitMid (S.PAIR (s1, s2), l) =
        S.length s1 > 0 andalso S.length s2 > 0 andalso S.append (s1, s2) ~= l
    | checkSplitMid _ = false

  (* exception wrappers *)
  structure SW = MkSequenceWrapper (S)
  structure DW = MkSequenceWrapper (D)

  (******************************************************************)

  fun run () =
  let
  in
  (


    (* val nth : 'a seq -> int -> 'a *)
    check "nth"
      (Gen.pair (Gen.list Gen.int, Gen.index),
       Show.pair (Show.list Show.int, Show.int))
      (Test.compare' op= [RANGE] (fn (l, i) => (SW.wrap2 S.nth (% l) i,
                                                DW.wrap2 D.nth    l  i)))
  ;

    (* val length : 'a seq -> int
     *
     * @code{unit list} is one most general input.
     *)
    check "length"
      (Gen.list Gen.unit, Show.list Show.unit)
      (Test.compare op= (fn l => (S.length (% l),
                                  D.length    l)))
  ;

    (* val empty : unit -> 'a seq *)
    check "empty"
      (Gen.unit, Show.unit)
      (Test.compare op~= (fn () => (S.empty (),
                                    D.empty () : unit list)))
  ;
    (* val singleton : 'a → 'a seq *)
    check "singleton"
      (Gen.int, Show.int)
      (Test.compare op~= (fn i => (S.singleton i,
                                   D.singleton i)))
  ;
    (* val $ : 'a -> 'a seq *)
    check "$"
      (Gen.int, Show.int)
      (Test.compare op~= (fn x => (S.$ x,
                                   D.$ x)))
  ;
    (* val subseq : 'a seq -> int * int -> 'a seq *)
    check "subseq"
      (Gen.pair (Gen.list Gen.int, Gen.pair (Gen.index, Gen.index)),
       Show.pair (Show.list Show.int, Show.pair (Show.int, Show.int)))
      (Test.compare' op~= [RANGE, SIZE]
        (fn (l, (i, n)) => (SW.wrap2 S.subseq (% l) (i, n),
                            DW.wrap2 D.subseq    l  (i, n))))
  ;

    (* val take : 'a seq -> int -> 'a seq *)
    check "take"
      (Gen.pair (Gen.list Gen.int, Gen.index),
       Show.pair (Show.list Show.int, Show.int))
      (Test.compare' op~= [RANGE, SIZE] (fn (l, n) => (SW.wrap2 S.take (% l) n,
                                                       DW.wrap2 D.take    l  n)))
  ;


    (* val drop : 'a seq -> int -> 'a seq *)
    check "drop"
      (Gen.pair (Gen.list Gen.int, Gen.index),
       Show.pair (Show.list Show.int, Show.int))
      (Test.compare' op~= [RANGE, SIZE] (fn (l, n) => (SW.wrap2 S.drop (% l) n,
                                                       DW.wrap2 D.drop    l  n)))
  ;

   (* val fromList : 'a list -> 'a seq *)
    check "fromList"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.fromList l,
                                   D.fromList l)))
  ;

    (* val toList : 'a seq -> 'a list *)
    check "toList"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op= (fn l => (S.toList (% l),
                                  D.toList    l)))
  ;

    (* val tabulate : (int -> 'a) -> int -> 'a seq
     *
     * One most general input function is @code{id}.
     *)
    check "tabulate"
      (Gen.len, Show.int)
      (Test.compare' op~= [SIZE] (fn i => (SW.wrap2 S.tabulate id i,
                                           DW.wrap2 D.tabulate id i)))
  ;

    (* val enum : 'a seq -> (int * 'a) seq *)
    check "enum"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.enum (% l),
                                   D.enum    l)))

  ;


    (* val map : ('a -> 'b) -> 'a seq -> 'b seq
     *
     * One most general input function is @code{id}.
     *)
    check "map"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.map id (% l),
                                   D.map id    l)))
  ;

    (* val mapIdx : (int * 'a -> 'b) -> 'a seq -> 'b seq
     *
     * One most general input function is @code{id}.
     *)
    check "mapIdx"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.mapIdx id (% l),
                                   D.mapIdx id    l)))
  ;

    (* val toString : ('a -> string) -> 'a seq -> string *)
    check "toString"
      (Gen.list Gen.string, Show.list Show.string)
      (Test.compare op= (fn l => (S.toString id (% l),
                                  D.toString id    l)))
  ;

    (* val rev : 'a seq -> 'a seq *)
    check "rev"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.rev (% l),
                                   D.rev    l)))
  ;

    (* val append : 'a seq * 'a seq -> 'a seq *)
    check "append"
      (Gen.pair (Gen.list Gen.int, Gen.list Gen.int),
       Show.pair (Show.list Show.int, Show.list Show.int))
      (Test.compare op~= (fn (l1, l2) => (S.append (% l1, % l2),
                                          D.append   (l1,   l2))))
  ;

    (* val zip : 'a seq * 'b seq -> ('a * 'b) seq *)
    check "zip"
      (Gen.pair (Gen.list Gen.int, Gen.list Gen.int),
       Show.pair (Show.list Show.int, Show.list Show.int))
      (Test.compare op~= (fn (l1, l2) => (S.zip (% l1, % l2),
                                          D.zip   (l1,   l2))))
  ;

    (* val zipWith : ('a * 'b -> 'c) -> 'a seq * 'b seq -> 'c seq
     *
     * One most general input function is @code{id}.
     *)
    check "zipWith"
      (Gen.pair (Gen.list Gen.int, Gen.list Gen.int),
       Show.pair (Show.list Show.int, Show.list Show.int))
      (Test.compare op~= (fn (l1, l2) => (S.zipWith id (% l1, % l2),
                                          D.zipWith id (l1, l2))))
  ;

    (* val scan : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq * 'a
     *
     * Lists (free monoid) are the most general.
     *)
    check "scan"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare (Eq.pair (op~=, op=)) (fn l => (S.scan List.@ [] (% (List.map $ l)),
                                                    D.scan List.@ []    (List.map $ l))))
  ;

    (* val scanIncl : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a seq
     *
     * Lists (free monoid) are the most general.
     *)
    check "scanIncl"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.scanIncl List.@ [] (% (List.map $ l)),
                                   D.scanIncl List.@ []    (List.map $ l))))
  ;

    (* val reduce : ('a * 'a -> 'a) -> 'a -> 'a seq -> 'a *)
    check "reduce"
      (Gen.list (Trace.genVar Gen.int), Show.list (Trace.show (Show.string, Show.int)))
      (Test.compare op= (fn l => (S.reduce (Trace.bin "f") (Trace.const "id") (% l),
                                  D.reduce (Trace.bin "f") (Trace.const "id")    l)))
  ;

    (* val filter : ('a -> bool) -> 'a seq -> 'a seq
     *
     * One most general input is arbitrarily labeled @code{bool}s.
     *)


    check "filter"
      (Gen.list (Gen.pair (Gen.bool, Gen.int)),
       Show.list (Show.pair (Show.bool, Show.int)))
      (Test.compare op~= (fn (l : (bool * int) list) =>
                                                        (S.filter (fn (b, _) => b) (% l),
                                                        (D.filter (fn (b, _) => b) l))))
  ;

    (* val filterIdx : (int * 'a -> bool) -> 'a seq -> 'a seq *)
    check "filterIdx"
      (Gen.pair (Gen.function (Gen.copair (Gen.coint, Gen.coint), Gen.bool), Gen.list Gen.int),
       Show.pair (Show.list Show.bool, Show.list Show.int) o (fn (f, l) => (D.map f (D.enum l), l)))
      (Test.compare op~= (fn (f, l) => (S.filterIdx f (% l),
                                        D.filterIdx f    l)))
  ;

    (* val update : 'a seq * (int * 'a) -> 'a seq *)
    check "update"
      (Gen.pair (Gen.list Gen.int, Gen.pair (Gen.index, Gen.int)),
       Show.pair (Show.list Show.int, Show.pair (Show.int, Show.int)))
      (Test.compare' op~= [RANGE] (fn (l, u) => (SW.wrap S.update (% l, u),
                                                 DW.wrap D.update   (l, u))))
  ;

    (* val splitHead : 'a seq -> 'a listview *)
    check "splitHead"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare listviewEq (fn l => (S.splitHead (% l),
                                         D.splitHead    l)))
    ;

    (* val splitMid : 'a seq -> 'a treeview *)
    (* This test is weak because the function breaks observational equivalence. *)
    check "splitMid (weak)"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.satisfies (fn l => checkSplitMid (S.splitMid (% l), l)))
    ;

    (* val iterate : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b
     *
     * This is the elimination rule for lists.
     *)
    check "iterate"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op= (fn l => (S.iterate snoc [] (% l),
                                  D.iterate snoc []    l)))
  ;

    (* val iteratePrefixes : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq * 'b *)
    check "iteratePrefixes"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare (Eq.pair (op~=, op=)) (fn l => (S.iteratePrefixes snoc [] (% l),
                                                    D.iteratePrefixes snoc []    l)))
  ;

    (* val iteratePrefixesIncl : ('b * 'a -> 'b) -> 'b -> 'a seq -> 'b seq *)
    check "iteratePrefixesIncl"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (S.iteratePrefixesIncl snoc [] (% l),
                                   D.iteratePrefixesIncl snoc []    l)))
  ;

    (* val collate : 'a ord -> 'a seq ord *)
    check "collate"
      (Gen.pair (Gen.list (Ranked.gen Gen.int), Gen.list (Ranked.gen Gen.int)),
       Show.pair (Show.list (Ranked.show Show.int), Show.list (Ranked.show Show.int)))
      (Test.compare op= (fn (l1, l2) => (S.collate Ranked.compareRank (% l1, % l2),
                                         D.collate Ranked.compareRank   (l1,   l2))))
  ;


    (* val argmax : 'a ord -> 'a seq -> int *)
    check "argmax"
      (Gen.list (Ranked.gen Gen.int), Show.list (Ranked.show Show.int))
      (Test.compare' op= [RANGE] (fn l => (SW.wrap2 S.argmax Ranked.compareRank (% l),
                                           DW.wrap2 D.argmax Ranked.compareRank    l)))
  ;



    (* val flatten : 'a seq seq -> 'a seq *)
    check "flatten"
      (Gen.list (Gen.list Gen.int),
       Show.list (Show.list Show.int))
      (Test.compare op~= (fn l => (S.flatten (%% l),
                                   D.flatten     l)))
  ;

    (* val merge : 'a ord -> 'a seq * 'a seq -> 'a seq
     *
     * One most general input is a pair of arbitrarily labeled @code{int}s.
     *)
    check "merge"
      (Gen.pair (genSortedIntList, genSortedIntList),
       Show.pair (Show.list Show.int, Show.list Show.int))
      (Test.compare (related op=)
        (fn (l1, l2) => (S.merge Int.compare (% l1, % l2),
                         D.merge Int.compare (l1, l2))))
  ;

    (* val sort : 'a ord -> 'a seq -> 'a seq *)
    check "sort"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare (related op=) (fn l => (S.sort Int.compare (% l),
                                            D.sort Int.compare l)))
  ;

    (* val merge : 'a ord -> 'a seq * 'a seq -> 'a seq
     *
     * One most general input is a pair of arbitrarily labeled @code{int}s.
     *)
    check "merge/stable"
      (Gen.pair (genSortedRankedIntList, genSortedRankedIntList),
       Show.pair (Show.list (Ranked.show Show.int), Show.list (Ranked.show Show.int)))
      (Test.compare (related (Ranked.equal op=))
        (fn (l1, l2) => (S.merge Ranked.compareRank (% l1, % l2),
                         D.merge Ranked.compareRank   (l1,   l2))))
  ;

(*
    (* val sort : 'a ord -> 'a seq -> 'a seq *)
    check "sort/stable"
      (Gen.list (Ranked.gen Gen.int), Show.list (Ranked.show Show.int))
      (Test.compare (related (Ranked.equal op=)) (fn l => (S.sort Ranked.compareRank (% l),
                                                           D.sort Ranked.compareRank    l)))
  ;
*)

(*
    (* val collect : 'a ord -> ('a * 'b) seq -> ('a * 'b seq) seq *)
    check "collect"
      (Gen.list (Gen.pair (Ranked.gen Gen.int, Gen.int)),
       Show.list (Show.pair (Ranked.show Show.int, Show.int)))
      (Test.compare (keyValuesEq (Ranked.equal op=)) (fn l => (S.collect Ranked.compareRank (% l),
                                                               D.collect Ranked.compareRank    l)))
  ;
*)
(*
    (* val inject : 'a seq * (int * 'a) seq -> 'a seq *)
    (* Inject is not consistent with the expected semantics. *)
    check "inject"
      (Gen.pair (Gen.list Gen.int, Gen.list (Gen.pair (Gen.index, Gen.int))),
       Show.pair (Show.list Show.int, Show.list (Show.pair (Show.int, Show.int))))
      (Test.compare' op~= [RANGE] (fn (l, u) => (SW.wrap S.inject (% l, % u),
                                                 DW.wrap D.inject   (l, u))))
  ;
*)

    () (* assumed *)
  )
  end
end
