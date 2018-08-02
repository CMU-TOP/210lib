structure Show =
struct
  val int = Int.toString
  val bool = Bool.toString
  fun unit () = "()"
  fun string s = s
  fun list showElem l =
    "[" ^ String.concatWith ", " (List.map showElem l) ^ "]"
  fun pair (showFst, showSnd) (x, y) = "<" ^ showFst x ^ ", " ^ showSnd y ^ ">"
  fun pair3 (showX, showY, showZ) (x, y, z) =
    "<" ^ showX x ^ ", " ^ showY y ^ ", " ^ showZ z ^ ">"
end
