functor MkDumbListSet (structure Key : EQKEY) :>
  SET where type 'a Seq.t = 'a list
        and type Key.t = Key.t
=
struct
  structure Key = Key
  structure Seq = DumbListSequence

  (* invariant: elements are unique *)
  type t = Key.t list
  type set = Key.t list

  val size = List.length
  fun toString s = "{" ^ String.concatWith "," (List.map Key.toString s) ^ "}"

  (* delete and insert implemented first for to/fromSeq *)
  fun delete (l, y) = List.filter (fn x => not (Key.equal (x, y))) l
  fun insert (l, y) = y :: delete (l, y)

  fun toSeq l = l

  fun empty () = []
  fun singleton x = [x]
  fun fromSeq l = List.foldr (fn (x, l') => insert (l', x)) [] l

  fun find l x = List.exists (fn y => Key.equal (x, y)) l

  val filterKey = List.filter

  fun reduceKey f b x = Seq.reduce f b (toSeq x) (* the spec says so *)
  fun iterateKey f b x = Seq.iterate f b (toSeq x) (* the spec says so *)

  fun union (s, t) = List.foldr (fn (x, t') => insert (t', x)) t s
  fun intersection (s, t) = List.filter (find t) s
  fun difference (s, t) = List.foldr (fn (x, t') => delete (t', x)) s t

  val $ = singleton (* the spec says so *)
end
