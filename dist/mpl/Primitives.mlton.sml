structure Primitives :> PRIMITIVES =
struct

  exception Primitives

  val numberOfProcessors = 1

  fun par (f, g) = (f (), g ())

  fun cas r (old, new) =
    let
      val x = !r
      val _ = if x = old then r := new else ()
    in
      x
    end

  fun casArray (a, i) (old, new) =
    let
      val x = Array.sub (a, i)
      val _ = if x = old then Array.update (a, i, new) else ()
    in
      x
    end

  val alloc = ArrayExtra.arrayUninit

  fun arrayUpdateUp (a, i, x) = Array.update (a, i, x)
  val refAssignUp = op:=

  val communicate = (fn _ => ())

  fun par3 (f, g, h) =
    let val ((a, b), c) = par (fn _ => par (f, g), h)
    in (a, b, c)
    end

  fun par4 (f, g, h, i) =
    let val ((a, b), (c, d)) = par (fn _ => par (f, g), fn _ => par (h, i))
    in (a, b, c, d)
    end

  fun parTab (n, f) = raise Primitives

  fun for (i, j) f = if i = j then () else (f i; for (i+1, j) f)
  fun forBackwards (i, j) f = if i = j then () else (f (j-1); forBackwards (i, j-1) f)

  fun loop (lo, hi) b f =
    if (lo >= hi) then b else loop (lo+1, hi) (f (b, lo)) f

  fun parfor grain (i, j) f =
    let val n = j - i
    in if n <= grain
       then for (i, j) f
       else ( par ( fn _ => parfor grain (i, i + n div 2) f
                  , fn _ => parfor grain (i + n div 2, j) f
                  )
            ; ()
            )
    end

end
