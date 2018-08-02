fun runTest (name : string) : unit =
    (
      case name of
          "array" => (SequenceGranularity.set 17; TestArraySequence.run ())
        | "list" => TestListSequence.run ()
       (* | "parray" => TestParallelArraySequence.run () *)
        | "stseq" => TestMkSTSequence.run ()
        |  "treap" => TestMkTreapTable.run ()

        | _ => print ("Unknown test name: " ^ name ^ "\n")
    ;
      print ("=========================================" ^ "\n")
    );

map runTest (CommandLine.arguments ());
