let all () =
  Cell.(
    NumCell.setup ();
    Pointer.setup ();
    ToCell.setup ();
  );

  ()
