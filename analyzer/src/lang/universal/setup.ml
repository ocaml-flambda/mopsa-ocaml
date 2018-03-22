let init () =
  Unit_tests.setup ();
  Numeric.(
    Non_relational.setup ();
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
