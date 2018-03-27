let init () =
  Program.setup ();
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
  );
  Memory.(
    Cell.setup ();
    Machine_integers.setup ();
    Pointer.setup ();
  );
  Libs.(
    Mopsa.setup ();
  );
  ()

let start () =
  ()
