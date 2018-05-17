let init () =

  Objects.(
    Object.setup ();
    Class.setup ();
    Function.setup ();
    Number.setup ();
    Data_model.(
      Attribute.setup ();
      Callable.setup ();
      Arith_ops.setup ();
    );
  );

  (* Flows.(
   *
   * ); *)

  Desugar.(
    Andor.setup ();
    Assert.setup ();
    Ifexpr.setup ();
    Import.setup ();
    Iterable_assign.setup ();
  );

  Libs.(
    Mopsa.setup ();
  );

  Memory.(
    Nonrel.setup ();
  );

  Program.setup ();

  ()

let start () =
  Builtins.setup ();
  ()
