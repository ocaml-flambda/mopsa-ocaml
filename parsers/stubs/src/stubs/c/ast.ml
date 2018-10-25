type loc = {
    file: string;
    line: int;
    col: int;
  }
type range = loc * loc

type 'a with_range = 'a * range

type stub = {
    stub_requires : formula with_range list;
    stub_local    : local with_range list;
    stub_assigns  : assigns with_range list;
    stub_case     : case with_range list;
    stub_ensures  : formula with_range list;
  }

and local = {
    local_var : var;
    local_value : local_value;
  }

and local_value =
  | LV_call    of var (** function *) * expr list (* arguments *)
  | LV_new     of resource

and assigns = {
    assign_target : expr with_range;
    assign_range  : (expr with_range * expr with_range) option;
  }

and case = {
    case_label: string;
    case_assumes: formula with_range list;
    case_requires : formula with_range list;
    case_local    : local with_range list;
    case_assigns  : assigns with_range list;
    case_ensures  : formula with_range list;
  }

and formula =
  | F_expr   of expr with_range
  | F_bool   of bool
  | F_binop  of log_binop * formula with_range * formula with_range
  | F_not    of formula with_range
  | F_forall of var * set * formula with_range
  | F_exists of var * set * formula with_range
  | F_in     of var * set
  | F_del    of expr with_range

and expr =
  | E_int of Z.t

and log_binop =
  | AND
  | OR
  | IMPLIES

and set =
  | S_interval of expr * expr
  | S_resource of resource

and resource = string

and var = string
