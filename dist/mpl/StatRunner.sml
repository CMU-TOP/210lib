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

      val elapsedTime = Time.toReal (Time.- (endTime, startTime))

      val totalGCTime = Vector.foldl op+ 0.0
        (Vector.tabulate (P, fn p =>
          Time.toReal (Time.- (Vector.sub (endGC, p), Vector.sub (startGC, p)))))

      val percentGC = 100.0 * totalGCTime / (elapsedTime * Real.fromInt P)
    in
      (result,
       String.concat [
         "wall ", Int.toString (Real.round (1000.0 * elapsedTime)), " ms\n",
         "gc   ", Int.toString (Real.round percentGC) ^ "%\n"
       ])
    end

end
