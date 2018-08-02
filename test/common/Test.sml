structure Test =
struct
  val _ = QCheck.Settings.gen_target := NONE
  val _ = QCheck.Settings.gen_max := 200
  val _ = QCheck.Settings.outstream := TextIO.stdOut
  val _ = QCheck.Settings.column_width := 35

  val implies = QCheck.implies
  val satisfies = QCheck.pred
  fun check description (gen, show) prop =
    QCheck.checkGen (gen, SOME show) (description, prop)

  (* a data type to hold an value or an exception *)
  datatype ('a, 'b) ret = VAL of 'a | EXCEPTION of 'b
  fun retEq eq _ (VAL x, VAL y) = eq (x, y)
    | retEq _ es (EXCEPTION x, EXCEPTION y) =
        let
          val _ = print "Exception encountered\n"
          val found = List.exists (fn e => x = e) es

          val passed = found andalso x = y
          val _ = print ("Exception encountered: is known?: " ^
                         Bool.toString found ^
                         "passed = " ^ Bool.toString passed ^
                         "\n")
        in
          found andalso x = y
        end
    | retEq _ _ _ =
        (print "Unknown exception encountered\n";         false)

  fun compare eq f = QCheck.pred (eq o f)
  fun compare' eq es f = QCheck.pred (retEq eq es o f)
  fun wrap catch f x = VAL (f x) handle e => EXCEPTION (catch e)
  fun wrap2 catch f x y = wrap catch (fn () => f x y) ()
end
