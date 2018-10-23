include
  AbstractRegularAutomaton.Make
    (State)
    (struct
      type t = int
      let compare = compare
      let print = Format.pp_print_int
    end)
