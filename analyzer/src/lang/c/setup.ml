let init () =
  Program.setup ();
  Alarms.setup ();
  Flows.(
    Interproc.setup ();
    Switch.setup ();
    Goto.setup ();
  );
  Memory.(
    Cell.(
      Expand.setup ();
      Smash.setup ();
    );
    Machine_integers.setup ();
    Pointer.setup ();
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
