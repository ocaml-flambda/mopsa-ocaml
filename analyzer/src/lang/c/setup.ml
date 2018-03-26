let init () =
  Program.setup ();
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
  );
  Memory.(
    Machine_integers.setup ();
    Cell.Domain.setup ();
  );
  ()

let start () =
  ()
