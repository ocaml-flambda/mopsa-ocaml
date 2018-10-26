type loc = {
    file: string;
    line: int;
    col: int;
  }
type range = loc * loc

type 'a with_range = 'a * range

type stub = {
    stub_requires : formula list;
    stub_local    : local list;
    stub_assigns  : assigns list;
    stub_case     : case list;
    stub_ensures  : formula list;
  }

and formula = formula_kind with_range
and local = local_kind with_range
and assigns = assigns_kind with_range
and case = case_kind with_range
and expr = expr_kind with_range
    
and local_kind = {
    local_var : var;
    local_value : local_value;
  }

and local_value =
  | LV_call    of var (** function *) * expr list (* arguments *)
  | LV_new     of resource

and assigns_kind = {
    assign_target : expr;
    assign_range  : (expr * expr) option;
  }

and case_kind = {
    case_label: string;
    case_assumes: formula list;
    case_requires : formula list;
    case_local    : local list;
    case_assigns  : assigns list;
    case_ensures  : formula list;
  }

and formula_kind =
  | F_expr   of expr
  | F_bool   of bool
  | F_binop  of log_binop * formula * formula
  | F_not    of formula
  | F_forall of var * set * formula
  | F_exists of var * set * formula
  | F_in     of var * set
  | F_free   of expr

and expr_kind =
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
