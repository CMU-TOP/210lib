structure Trace =
struct
  datatype ('a, 'b) trace = VAR of 'b | OP of 'a * ('a, 'b) trace list

  fun genVar g = Gen.map VAR g
  fun show (sn, sv) t =
    let fun loop (VAR x) = sv x
          | loop (OP nl) = "OP " ^ Show.pair (sn, Show.list loop) nl
    in loop t
    end
  fun const name = OP (name, [])
  fun bin name (x, y) = OP (name, [x, y])
end
