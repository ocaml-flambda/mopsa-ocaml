open Pp

let init () =
  Program.setup ();
  Flows.Intraproc.setup ();
  Memory.(
    To_cell.setup ();
    Pointer.setup ();
    Numeric.(
      Cell_functor.setup ();
      Machine_integers.setup ();
    );
  );
  ()

let start () =
  ()
