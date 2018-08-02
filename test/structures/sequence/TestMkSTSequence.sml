(*
 * We assume @code{toSeq} is correct and @code{fromSeq} is surjective.
 *
 * This test is quite weak because internal states are critical
 * but there are no obvious way to test internal states from
 * the input-output behaviors.
 *)
structure TestMkSTSequenceShared =
struct
  datatype caught_exceptions = RANGE

  (* sequence of commands *)
  (* The first argument refers to the index of the sequences in history (in
   * reverse order).  One can access any used sequence.
   *)
  datatype 'a cmd = NTH of int * int
                  | UPDATE of int * (int * 'a)
                  | INJECT of int * (int * 'a) list
                  | TOSEQ of int
                  | FROMSEQ of int list
  datatype ('a, 'b) observation = SINGLE of 'a
                                | LIST of 'a list
                                | NOTHING
                                | RAISE of 'b
  val showUpdate = Show.pair (Show.int, Show.int)
  fun showCmd (NTH (n, i)) = "nth (#" ^ Show.int n ^ ", " ^ Show.int i ^ ")"
    | showCmd (UPDATE (n, u)) =
        "update (#" ^ Show.int n ^ ", " ^ showUpdate u ^ ")"
    | showCmd (INJECT (n, u)) =
        "inject (#" ^ Show.int n ^ ", " ^ Show.list showUpdate u ^ ")"
    | showCmd (TOSEQ n) = "toSeq #" ^ Show.int n
    | showCmd (FROMSEQ l) = "fromSeq " ^ Show.list Show.int l
  val genCmdList : int cmd list Gen.gen =
    let
      fun pick l = Gen.map (fn i => (i, List.nth (l, i))) (Gen.safeIndex (length l))
      fun goNth lens r =
            let
              val ((si, len), r) = pick lens r
              val (i, r) = Gen.index' len r
            in cont lens (NTH (si, i)) r
            end
      and goUpdate lens r =
            let
              val ((si, len), r) = pick lens r
              val (u, r) = Gen.pair (Gen.index' len, Gen.int) r
            in cont (len :: lens) (UPDATE (si, u)) r
            end
      and goInject lens r =
            let
              val ((si, len), r) = pick lens r
              val (u, r) = Gen.list (Gen.pair (Gen.index' len, Gen.int)) r
            in cont (len :: lens) (INJECT (si, u)) r
            end
      and goFromSeq lens r =
            let val (l, r) = Gen.list Gen.int r
            in cont (List.length l :: lens) (FROMSEQ l) r
            end
      and goToSeq lens r =
            let val ((si, _), r) = pick lens r
            in cont lens (TOSEQ si) r
            end
      and loop [] = goFromSeq []
        | loop lens = Gen.choose'
            [ (1, Gen.lift nil)
            , (10, goNth lens)
            , (10, goUpdate lens)
            , (10, goInject lens)
            , (2, goFromSeq lens)
            , (2, goToSeq lens)
            ]
      and cont lens e = Gen.map (fn rest => e :: rest) (loop lens)
    in loop []
    end
end
functor MkTestMkSTSequenceWrapper
  (ST : ST_SEQUENCE where type 'a Seq.t = 'a list) =
struct
  open TestMkSTSequenceShared
  fun catch ST.Range = RANGE
    | catch e = raise e
  fun wrap2 f = Test.wrap2 catch f
  fun wrap f = Test.wrap catch f

  fun apply (ss, NTH (n, i)) =
        let val s = List.nth (ss, n)
        in case wrap2 ST.nth (List.nth (ss, n)) i of
              Test.VAL x => (ss, SINGLE x)
            | Test.EXCEPTION x => (ss, RAISE x)
        end
    | apply (ss, UPDATE (n, u)) =
        let val s = List.nth (ss, n)
        in case wrap ST.update (s, u) of
              Test.VAL s => (s :: ss, NOTHING)
            | Test.EXCEPTION x => (s :: ss, RAISE x)
        end
    | apply (ss, INJECT (n, u)) =
        let val s = List.nth (ss, n)
        in case wrap ST.inject (s, u) of
              Test.VAL s => (s :: ss, NOTHING)
            | Test.EXCEPTION x => (s :: ss, RAISE x)
        end
    | apply (ss, TOSEQ n) =
        let val s = List.nth (ss, n)
        in (ss, LIST (ST.toSeq s))
        end
    | apply (ss, FROMSEQ l) = (ST.fromSeq l :: ss, NOTHING)
  fun run cs =
    let
      fun loop (ss, []) = (ss, [])
        | loop (ss, (c :: cs)) =
            let
              val (ss, ob) = apply (ss, c)
              val (ss, obs) = loop (ss, cs)
            in (ss, ob :: obs)
            end
    in loop ([], cs)
    end
end
structure TestMkSTSequence :>
sig
  val run : unit -> unit
end =
struct
  open TestMkSTSequenceShared

  (* the reference implementation for tested functions *)
  structure D = DumbSTListSequence
  (* the single-threaded version *)
  structure ST = MkSTSequence (structure Seq = DumbListSequence)

  (* the relation: assumes *)
  infix 4 ~= (* the precedence of op= *)
  fun s ~= l = ST.toSeq s = l

  (* misc *)
  val % = ST.fromSeq

  (* helper functions for testing *)
  fun traceEq ((ss, os1), (ls, os2)) =
    ListPair.allEq (op~=) (ss, ls) andalso os1 = os2

  (* exception wrappers *)
  structure STW = MkTestMkSTSequenceWrapper (ST)
  structure DW = MkTestMkSTSequenceWrapper (D)

  (******************************************************************)

  fun run () =
  (
    print ("Testing MkSTSequence..." ^ "\n")
  ;
    Test.check "access sequences"
      (genCmdList, Show.list showCmd)
      (Test.compare traceEq (fn cs => (STW.run cs,
                                        DW.run cs)))
  ;
    (* val fromSeq : 'a Seq.t -> 'a stseq *)
    Test.check "MkSTSequence.fromSeq (weak)"
      (Gen.list Gen.int, Show.list Show.int)
      (Test.compare op~= (fn l => (ST.fromSeq l,
                                    D.fromSeq l)))
  ;
    (* val toSeq : 'a stseq -> 'a Seq.t *)
    () (* assumed *)
  ;
    (* val nth : 'a stseq -> int -> 'a *)
    Test.check "MkSTSequence.nth (weak)"
      (Gen.pair (Gen.list Gen.int, Gen.index), Show.pair (Show.list Show.int, Show.int))
      (Test.compare' op= [RANGE] (fn (l, i) => (STW.wrap2 ST.nth (% l) i,
                                                 DW.wrap2  D.nth    l  i)))
  ;
    (* val update : 'a stseq * (int * 'a) -> 'a seq *)
    Test.check "MkSTSequence.update (weak)"
      (Gen.pair (Gen.list Gen.int, Gen.pair (Gen.index, Gen.int)),
       Show.pair (Show.list Show.int, Show.pair (Show.int, Show.int)))
      (Test.compare' op~= [RANGE] (fn (l, u) => (STW.wrap ST.update (% l, u),
                                                  DW.wrap  D.update   (l, u))))
  ;
    (* val inject : 'a seq * (int * 'a) seq -> 'a seq *)
    Test.check "MkSTSequence.inject (weak)"
      (Gen.pair (Gen.list Gen.int, Gen.list (Gen.pair (Gen.index, Gen.int))),
       Show.pair (Show.list Show.int, Show.list (Show.pair (Show.int, Show.int))))
      (Test.compare' op~= [RANGE] (fn (l, u) => (STW.wrap ST.inject (% l, u),
                                                  DW.wrap  D.inject   (l, u))))
  )

end
