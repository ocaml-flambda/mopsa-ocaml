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
      Smashing.setup ();
      Reductions.(
        Expand_smashing.setup ();
      );
      Initzero.setup ();
    );
    Machine_integers.setup ();
    Pointer.setup ();
    Access_path.setup ();
    Structured.setup ();
  );
  Desugar.(
    Andor.setup ();
    Records.setup ();
    Stmt_rvals.setup ();
    Loops.setup ();
  );
  Libs.(
    Mopsa.setup ();
    Stdlib.setup ();
  );
  ()

let start () =
  ()
