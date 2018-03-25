let init () =
  Unit_tests.setup ();
  Numeric.(
    Integers.setup ();
    Relational.setup ();
  );
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
  );
  Heap.Recency.setup ();
  ()

let start () =
  ()
