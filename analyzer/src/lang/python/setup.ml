let init () =
  
  Objects.(
    Object.setup ();
    Class.setup ();
    Function.setup ();
    Data_model.(
      Attribute.setup ();
      Callable.setup ();
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

  Programs.(
    Standalone.setup ();
    Unit_test.setup ();
  );

  ()

let start () =
  Builtins.setup ();
  ()
