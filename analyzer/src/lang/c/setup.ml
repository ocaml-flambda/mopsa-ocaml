let init () =
  Program.setup ();
  Alarms.setup ();
  Flows.(
    Intraproc.setup ();
    Interproc.setup ();
    Loops.setup ();
  );
  Memory.(
    Cell.setup ();
    Machine_integers.setup ();
    Pointer.setup ();
    Array_to_pointer.setup ();
    Record_to_pointer.setup ();
    Var_init.setup ();
  );
  Libs.(
    Mopsa.setup ();
  );
  ()

let start () =
  ()
