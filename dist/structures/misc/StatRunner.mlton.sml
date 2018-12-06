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

  fun rtos r =
    Int.toString (Real.round r)
    handle Domain => "-"

  fun run (f : unit -> 'a) : 'a * string =
    let
      val startTime = Time.now ()
      val startGC = getGCTime ()

      val result = f ()

      val endGC = getGCTime ()
      val endTime = Time.now ()

      val elapsedTime = Time.toReal (Time.- (endTime, startTime))
      val elapsedGC = Time.toReal (Time.- (endGC, startGC))

      val percentGC = 100.0 * elapsedGC / elapsedTime
    in
      (result,
       String.concat [
         "wall ", rtos (1000.0 * elapsedTime), " ms\n",
         "gc   ", rtos percentGC, "%\n"
       ])
    end

end
