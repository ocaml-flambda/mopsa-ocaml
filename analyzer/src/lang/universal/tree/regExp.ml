include
  AbstractRegularAutomaton.Make
    (Tools.State)
    (struct
      type t = int
      let compare = compare
      let print = Format.pp_print_int
    end)
