structure Main =
struct
  structure Seq = ArraySequence
  structure SerialSeq = SerialArraySequence
  structure G = SequenceGranularity
  structure CLA = CommandLineArgs

  val defaultAlgo = "scan"
  val defaultMode = "parallel"
  val defaultGran = 1000
  val defaultSize = 1000000
  val defaultSeed = 15210

  fun randomIntSeq (lo, hi) n x =
    let val (r, rs) = DotMix.splitTab (DotMix.fromInt x, n)
    in Seq.tabulate (DotMix.boundedInt (lo, hi) o rs) n
    end

  fun choose isParallel (f, g) =
    if isParallel then f() else g()

  fun run () =
    let
      val _ = CLA.init ()
      val size = CLA.parseOrDefaultInt ("size", defaultSize)
      val _ = G.set (CLA.parseOrDefaultInt ("gran", defaultGran))
      val mode = (CLA.parseOrDefaultString ("mode", defaultMode) = "parallel")
      val task =
        case CLA.parseOrDefaultString ("algo", defaultAlgo) of
          "scan" =>
            let
              fun f (x, y) = x + y
              val b = 0
              val s = Seq.tabulate (fn i => 1) size
            in
              fn () => #2 (choose mode (fn () => Seq.scan f b s, fn () => SerialSeq.scan f b s))
            end

        | other => raise Fail ("unknown algo " ^ other)

      val (result, stats) = StatRunner.run task
    in
      print (String.concat ["result ", Int.toString result, "\n",
                            stats])
    end
end

val () = Main.run ()
