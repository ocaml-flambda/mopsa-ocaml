let init () =

  Objects.(
    Object.setup ();
    Class.setup ();
    Function.setup ();
    Number.setup ();
    Containers.(
      Dicts.setup ();
    );
    Data_model.(
      Attribute.setup ();
      Callable.setup ();
      Arith_ops.setup ();
      Subscript.setup ();
    );
  );

  (* Flows.(
   *
   * ); *)

  Desugar.(
    Bool.setup ();
    Assert.setup ();
    If.setup ();
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
