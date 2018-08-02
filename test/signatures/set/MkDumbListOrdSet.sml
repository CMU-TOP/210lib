functor MkDumbListOrdSet
  (structure Key : ORDKEY
   structure Seq : SEQUENCE) : ORDSET
=
struct
  (* helper function sort *)
  val sort = DumbListSequence.sort Key.compare
  (* unordered set *)
  structure Set = MkDumbListSet (structure Key = Key
                                 structure Seq = Seq)

  structure Key = Key
  structure Seq = Seq

  type t = Set.t
  type set = Set.set

  val size = Set.size
  val toString = Set.toString

  val toSeq = Set.toSeq

  val empty = Set.empty
  val singleton = Set.singleton
  val fromSeq = sort o Set.fromSeq

  val find = Set.find

  val insert = sort o Set.insert
  val delete = sort o Set.delete

  val filterKey = Set.filterKey

  fun reduceKey f b x = Seq.reduce f b (toSeq x) (* hte spec says so *)
  fun iterateKey f b x = Seq.iterate f b (toSeq x) (* hte spec says so *)

  val union = sort o Set.union
  val intersection = sort o Set.intersection
  val difference = sort o Set.difference

  val $ = singleton (* hte spec says so *)

  (* ordered set API *)
  fun first l = SOME (List.hd l) handle List.Empty => NONE
  fun last l = SOME (List.last l) handle List.Empty => NONE

  fun less (l, x) = List.filter (fn y => Key.compare (y, x) = LESS) l
  fun greater (l, x) = List.filter (fn y => Key.compare (y, x) = GREATER) l

  fun prev (l, x) = last (less (l, x))
  fun next (l, x) = first (greater (l, x))
  fun split (l, x) = (less (l, x), find l x, greater (l, x))

  val join = List.@

  fun getRange l (a, b) = less (greater (l, a), b)

  fun rank (l, x) = size (less (l, x))
  fun select (l, i) = SOME (List.nth (l, i)) handle General.Subscript => NONE
  fun splitRank (l, i) = (List.take (l, i), List.drop (l, i))
end
