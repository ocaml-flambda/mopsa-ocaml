type 'a lattice = {
  bottom:    'a;
  top:       'a;
  is_bottom: 'a -> bool;
  subset:    'a ctx -> 'a -> 'a -> bool;
  join:      'a ctx -> 'a -> 'a -> 'a;
  meet:      'a ctx -> 'a -> 'a -> 'a;
  widen:     'a ctx -> 'a -> 'a -> 'a;
  merge:     'a -> 'a * log -> 'a * log -> 'a;
  print:     Format.formatter -> 'a -> unit;
}

(** Managers provide access to full analyzer *)
type ('a, 't) man = {
  (* Lattice operators of the toplevel abstract element ['a] *)
  lattice : 'a lattice;

  (* Accessors to the domain's abstract element ['t] within ['a] *)
  get : 'a -> 't;
  set : 't -> 'a -> 'a;

  (* Toplevel transfer functions *)
  exec : stmt -> 'a flow -> 'a post;
  eval : expr -> 'a flow -> 'a eval;
  ask : 'r. ('a,'r) query -> 'a flow -> 'r;

  (* Accessors to the domain's logs *)
  get_log : log -> log;
  set_log : log -> log -> log;
}
