let init () =
  Unit_tests.setup ();
  Numeric.(
    Domains.(
      Boxes.(
        Int.setup ();
        Float.setup ();
        Congruence.setup ();
      );
      Relational.setup ();
    );
  );
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
    Memoisation.setup ();
  );
  Heap.Recency.setup ();
  ()

let start () =
  ()
