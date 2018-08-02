structure DumbListSequence :> SEQUENCE where type 'a t = 'a list =
struct
  type 'a t = 'a list
  type 'a seq = 'a t
  type 'a ord = 'a * 'a -> order

  datatype 'a listview = NIL | CONS of 'a * 'a seq
  datatype 'a treeview = EMPTY | ONE of 'a | PAIR of 'a seq * 'a seq

  exception Range
  exception Size

  exception BreaksParametricity

  fun nth s i = List.nth (s, i) handle Subscript => raise Range
  val length = List.length
  fun toList (x : 'a seq) : 'a list = x
  fun toString f s = "<" ^ String.concatWith "," (map f s) ^ ">"
  fun equal eq (l1, l2) = ListPair.allEq eq (l1, l2)

  fun empty () = []
  fun singleton x = [x]
  fun tabulate f n = List.tabulate (n, f) handle General.Size => raise Size
  fun fromList x = x

  val rev = List.rev
  val append = List.@
  val flatten = List.concat

  val filter = List.filter
  val map = List.map
  val zip = ListPair.zip
  fun zipWith f (s, t) = map f (zip (s, t)) (* the spec says so *)

  fun enum s = List.tabulate (List.length s, (fn i => (i, List.nth (s, i))))
  fun filterIdx p l = List.map #2 (List.filter p (enum l))
  fun mapIdx f l = map f (enum l) (* the spec says so *)
  fun update (s, (i, x)) =
    if i < 0 orelse i >= length s then raise Range
    else List.take (s, i) @ [ x ] @ List.drop (s, i + 1)
  (* see below for inject *)

  fun subseq s (i, n) =
    if n < 0 then (print "Dumb: size"; raise Size)
    else List.take (List.drop (s, i), n) handle Subscript => (print "Dumb: Range. "; raise Range)
  fun take s n = subseq s (0, n) (* the spec says so *)
  fun drop s n =
    if n > length s then raise Size (* XXX the new spec to be confirmed *)
    else if n < 0 then raise Range (* XXX the new spec to be confirmed *)
    else subseq s (n, length s - n) (* the spec says so *)
  fun splitHead [] = NIL (* the spec says so *)
    | splitHead l = CONS (nth l 0, drop l 1) (* the spec says so *)
  fun splitMid _ = raise BreaksParametricity

  fun iterate f b [] = b (* the spec says so *)
    | iterate f b s = List.foldl (fn (x,b) => f (b,x)) b s
  fun iteratePrefixes f b s =
    (tabulate (fn i => iterate f b (take s i)) (length s), iterate f b s) (* the spec says so *)
  fun iteratePrefixesIncl f b s =
    tabulate (fn i => iterate f b (take s (i+1))) (length s) (* the spec says so *)
  fun reduce f b [] = b (* the spec says so *)
    | reduce f b [x] = x (* the spec says so *)
    | reduce f b s =
      let val n = length s
      in f (reduce f b (take s (n div 2)), reduce f b (drop s (n div 2))) (* the spec says so *)
      end
  fun scan f b s = (tabulate (fn i => reduce f b (take s i)) (length s), reduce f b s) (* the spec says so *)
  fun scanIncl f b s = tabulate (fn i => reduce f b (take s (i+1))) (length s) (* the spec says so *)
  fun inject (s, u) = iterate update s u (* the spec says so *)

  (* helper functions *)
  fun insert cmp (y, []) = [y]
    | insert cmp (y, x :: xs) =
        case cmp (y, x) of
             GREATER => x :: insert cmp (y, xs)
           | _ => y :: x :: xs
  fun remove cmp (y, l) = List.filter (fn x => cmp (x, y) <> EQUAL) l
  fun unique cmp l = List.foldr (fn (x, l') => x :: remove cmp (x, l')) [] l

  fun sort cmp s = List.foldr (insert cmp) [] s
  fun merge cmp (s, t) = List.foldr (insert cmp) t s
  fun collect (cmp : 'a ord) (s : ('a * 'b) seq)  =
      let
        val sortedUniqKeys = sort cmp (unique cmp (List.map #1 s))
        fun filterByKey key (k, v) =
          case cmp (k, key) of
               EQUAL => SOME v
             | _ => NONE
        fun getVals k = List.mapPartial (filterByKey k) s
      in List.map (fn k => (k, getVals k)) sortedUniqKeys
      end
  val collate = List.collate
  fun argmax _ [] = raise Range
    | argmax cmp s =
        let
          val max = List.last (sort cmp s)
          val maxIdx = List.filter (fn (i, x) => cmp (x, max) = EQUAL) (enum s)
        in
          #1 (List.hd maxIdx)
        end

  val $ = singleton (* the spec says so *)
  val % = fromList (* the spec says so *)
end
