let init () =
  Numeric.Relational.setup ();
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
  );
  Heap.Recency.setup ();
  ()

let start () =
  ()
