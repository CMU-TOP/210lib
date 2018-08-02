fun runTest (name : string) : unit =
  (
    case name of
         "array" => TestArraySequence.run ()
       | "list" => TestListSequence.run ()
       | "stseq" => TestMkSTSequence.run ()
       |  "treap" => TestMkTreapTable.run ()

       | _ => print ("Unknown test name: " ^ name ^ "\n")
  ;
    print ("=========================================" ^ "\n")
  );

map runTest (CommandLine.arguments ());
