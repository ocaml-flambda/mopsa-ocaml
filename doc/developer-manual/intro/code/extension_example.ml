type typ +=
  | T_bool                        (** Boolean *)
  | T_int                         (** Mathematical integers *)
  | T_float of float_prec         (** Floating-point real numbers. *)
  | T_string                      (** Strings. *)
  | T_addr                        (** Heap addresses. *)
  | T_array of typ                (** Array of [typ] *)
  | T_unit                        (** Unit type *)
  | T_char                        (** Character type *)

type expr_kind +=
  (** Function calls *)
  | E_call of expr (** Function expression *) * expr list (** List of arguments *)

  (** Array value as a list of expressions *)
  | E_array of expr list

  (** Subscript access to an indexed object (arrays) *)
  | E_subscript of expr * expr
  ...

type prog_kind +=
  | P_universal of {
      universal_gvars   : var list;
      universal_fundecs : fundec list;
      universal_main    : stmt;
    }
