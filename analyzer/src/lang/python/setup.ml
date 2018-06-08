let init () =

  Memory.(
    Addr_env.setup ();
    Value_env.setup ();
  );

  Objects.(
    Object.setup ();
    Class.setup ();
    Function.setup ();
    Strings.setup ();
    Slices.setup ();
    Numbers.(
      Int.setup ();
      Float.setup ();
      Bool.setup ();
      Complx.setup ();
    );
    Nones.setup ();
    Containers.(
      Lists.setup ();
      Dicts.setup ();
      Ranges.setup ();
      Sets.setup ();
      Tuples.setup ();
    );
  );

  Data_model.(
    Attribute.setup ();
    Callable.setup ();
    Arith_ops.setup ();
    Subscript.setup ();
    Compare_ops.setup ();
    Aug_assign.setup ();
  );

  Flows.(
    Exceptions.setup ();
    Generators.setup ();
  );

  Desugar.(
    Bool.setup ();
    Assert.setup ();
    If.setup ();
    Import.setup ();
    Iterable_assign.setup ();
    With.setup ();
    Loops.setup ();
  );

  Libs.(
    Mopsa.setup ();
    Stdlib.setup ();
    Unittest.setup ();
    Math.setup ();
  );

  Program.setup ();

  ()
