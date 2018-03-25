open Pp

let init () =
  Program.setup ();
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
  );
  Memory.(
    Machine_integers.setup ();
    Cell.(
      To_cell.setup ();
      To_numeric.setup ();
      Pointer.setup ();
    );
  );
  ()

let start () =
  ()
