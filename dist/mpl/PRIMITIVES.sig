signature PRIMITIVES =
sig
  val numberOfProcessors : int

  val par : (unit -> 'a) * (unit -> 'b) -> ('a * 'b)
  val par3 : (unit -> 'a) * (unit -> 'b) * (unit -> 'c) -> ('a * 'b * 'c)
  val par4 : (unit -> 'a) * (unit -> 'b) * (unit -> 'c) * (unit -> 'd) -> ('a * 'b * 'c * 'd)

  val for : int * int -> (int -> unit) -> unit
  val forBackwards : int * int -> (int -> unit) -> unit
  val loop : int * int -> 'a -> ('a * int -> 'a) -> 'a
  val parfor : int -> (int * int) -> (int -> unit) -> unit
  val parTab : int * (int -> 'a) -> (int -> 'a)

  val alloc : int -> 'a Array.array

  val arrayUpdateUp : 'a array * int * 'a -> unit
  val refAssignUp : 'a ref * 'a -> unit

  (* cas r (old, new) is equivalent to
   *   let
   *     val x = !r
   *     val _ = if x = old then r := new else ()
   *   in
   *     x
   *   end
   *)
  val cas : int ref -> int * int -> int
  val casArray : int array * int -> int * int -> int

  val communicate : unit -> unit
end
