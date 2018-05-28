let init () =

  Objects.(
    Object.setup ();
    Class.setup ();
    Function.setup ();
    Number.setup ();
    Strings.setup ();
    Slices.setup ();
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

  Memory.(
    Nonrel.setup ();
  );

  Program.setup ();

  ()

let start () =
  Builtins.setup ();
  ()
