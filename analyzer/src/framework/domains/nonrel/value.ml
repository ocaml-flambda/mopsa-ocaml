(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of values. *)

module type VALUE =
sig

  (*==========================================================================*)
                        (** {2 Lattice structure} *)
  (*==========================================================================*)

  include Lattice.LATTICE

  val of_constant : Ast.constant -> t
  (** Create a singleton abstract value from a constant. *)

  (*==========================================================================*)
                          (** {2 Forward semantics} *)
  (*==========================================================================*)

  val unop : Ast.operator -> t -> t
  (** Forward evaluation of unary operators. *)

  val binop : Ast.operator -> t -> t -> t
  (** Forward evaluation of binary operators. *)

  val filter : t -> bool -> t
  (** Keep values that may represent the argument truth value *)

  (*==========================================================================*)
                          (** {2 Backward operators} *)
  (*==========================================================================*)

  val bwd_unop : Ast.operator -> t -> t -> t
  (** Backward evaluation of unary operators.
      [bwd_unop op x r] returns x':
       - x' abstracts the set of v in x such as op v is in r
       i.e., we fiter the abstract values x knowing the result r of applying
       the operation on x

       it is safe, as first approximation, to implement it as the identity:
       let bwd_unop _ x _ = x
     *)

  val bwd_binop : Ast.operator -> t -> t -> t -> t * t
  (** Backward evaluation of binary operators.
      [bwd_binop op x y r] returns (x',y') where
      - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
      - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
      i.e., we filter the abstract values x and y knowing that, after
      applying the operation op, the result is in r

      it is safe, as first approximation, to implement it as the identity:
      let bwd_binop _ x y _ = (x,y)
  *)


  val compare : Ast.operator -> t -> t -> t * t
  (** Backward evaluation of boolean comparisons. [compare op x y] returns (x',y') where:
       - x' abstracts the set of v  in x such that v op v' is true for some v' in y
       - y' abstracts the set of v' in y such that v op v' is true for some v  in x
       i.e., we filter the abstract values x and y knowing that the test is true

       a safe, but not precise implementation, would be:
       compare _ x y = (x,y)
  *)

  (*==========================================================================*)
  (**                          {2 Value zone}                                 *)
  (*==========================================================================*)

  val zone : Zone.t
  (** Language zone in which the value abstraction is defined *)

  (*==========================================================================*)
  (**                          {2 Printing}                                   *)
  (*==========================================================================*)
  
  val display : string
  (** Domain name displayed in printing *)

end



(*==========================================================================*)
(**                         {2 Registration} *)
(*==========================================================================*)

type _ id = ..

type (_, _) eq = Eq : ('a, 'a) eq

type 'a info = {
  name : string;
  id : 'a id;
  eq : 'b. 'b id -> ('a, 'b) eq option;
  domain : (module VALUE with type t = 'a);
}

type pool =
  | Nil : pool
  | Cons : 'a info * pool -> pool

let values : pool ref = ref Nil

let register_value info = values := Cons (info, !values)

let find_value name =
  let rec aux = function
    | Nil -> raise Not_found
    | Cons(hd, tl) ->
      if hd.name = name then
        let module D = (val hd.domain) in
        (module D : VALUE)
      else aux tl
  in
  aux !values

let rec find_pool (names: string list) : pool =
  match names with
  | [] -> Nil
  | name :: names ->
    let rec aux : pool -> pool =
      fun pool ->
        match pool with
        | Nil -> raise Not_found
        | Cons(hd, tl) ->
          if hd.name = name then Cons(hd, find_pool names)
          else aux tl
    in
    aux !values


(*==========================================================================*)
(**                  {2 Default backward evaluators} *)
(*==========================================================================*)

let default_bwd_unop (op:Ast.operator) (x:'a) (r:'a) : 'a =
  x

let default_bwd_binop (op:Ast.operator) (x:'a) (y:'a) (r:'a) : ('a*'a) =
  (x, y)

let default_compare (op:Ast.operator) (x:'a) (y:'a) : ('a*'a) =
  (x, y)
