structure StatRunner :>
sig
  val run : (unit -> 'a) -> 'a * string
end =
struct

  val _ = MLton.Rusage.measureGC true
  fun getGCTime () =
    let
      val {gc={utime,stime}, ...} = MLton.Rusage.rusage ()
    in
      Time.+ (utime, stime)
    end

  fun run (f : unit -> 'a) : 'a * string =
    let
      val startTime = Time.now ()
      val startGC = getGCTime ()

      val result = f ()

      val endGC = getGCTime ()
      val endTime = Time.now ()

      val elapsedTime = Time.toMilliseconds (Time.- (endTime, startTime))
      val elapsedGC = Time.toMilliseconds (Time.- (endGC, startGC))

      val percentGC = 100.0 * (Real.fromLargeInt elapsedGC / Real.fromLargeInt elapsedTime)
    in
      (result,
       String.concat [
         "wall ", LargeInt.toString elapsedTime, " ms\n",
         "gc   ", Int.toString (Real.round percentGC), "%\n"
       ])
    end

end
