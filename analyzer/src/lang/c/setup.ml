let init () =
  Program.setup ();
  Alarms.setup ();
  Flows.(
    Interproc.setup ();
    Switch.setup ();
    Goto.setup ();
  );
  Memory.(
    Cell.setup ();
    Machine_integers.setup ();
    Pointer.setup ();
    Array_to_pointer.setup ();
    Record_to_pointer.setup ();
    Var_init.setup ();
  );
  Desugar.(
    Andor.setup ();
    Assign.setup ();
    Loops.setup ();
  );
  Libs.(
    Mopsa.setup ();
    Stdlib.setup ();
  );
  ()

let start () =
  ()
