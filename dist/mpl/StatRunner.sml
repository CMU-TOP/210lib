structure StatRunner :>
sig
  val run : (unit -> 'a) -> 'a * string
end =
struct

  val P = Primitives.numberOfProcessors
  val _ = MLton.Rusage.measureGC true

  fun getGCTimes () =
    Vector.tabulate (P, fn p =>
      Time.+ (MLton.GC.Statistics.localGCTimeOfProc p,
              MLton.GC.Statistics.promoTimeOfProc p))

  fun run (f : unit -> 'a) : 'a * string =
    let
      val startTime = Time.now ()
      val startGC = getGCTimes ()

      val result = f ()

      val endGC = getGCTimes ()
      val endTime = Time.now ()

      val wallms = Time.toMilliseconds (Time.- (endTime, startTime))

      val totalgcms = Vector.foldl op+ 0
        (Vector.tabulate (P, fn p =>
          Time.toMilliseconds (Time.- (Vector.sub (endGC, p), Vector.sub (startGC, p)))))

      val percentGC =
        100.0 * (Real.fromLargeInt (totalgcms) / Real.fromLargeInt (wallms * LargeInt.fromInt P))
    in
      (result,
       String.concat [
         "wall ", LargeInt.toString wallms, " ms\n",
         "gc   ", Int.toString (Real.round percentGC) ^ "%\n"
       ])
    end

end
