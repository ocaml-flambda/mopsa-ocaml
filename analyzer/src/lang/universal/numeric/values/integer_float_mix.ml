(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Mixing float and integer value abstractions. *)

open Framework.Essentials
open Framework.Value
open Ast


module type CAST =
sig
  type float_t
  type int_t
  val float_of_int : float_prec -> int_t -> float_t with_channel
  val int_of_float : float_t -> int_t with_channel
end
(** Signature of functions to convert between float and integer
    abstract values, implementing cast.
 *)
  

(** General functor for arbitrary integer/float abstractions. *)
  
module Make(IV:VALUE)
           (FV:VALUE)
           (C:CAST with type float_t = FV.t and type int_t = IV.t) =
struct
  
  (** Types *)
  
  type t =
    | I of IV.t
    | F of FV.t
    | BOT
    | TOP

  let zone = Zone.Z_u_num

  type _ value += V_mix : IV.t value * FV.t value -> t value

  let id = V_mix (IV.id, FV.id)

  let name = "universal.numeric.values.integer_float_mix",
             (snd IV.name) ^ " ⊎ " ^ (snd FV.name)
             
  let identify : type a. a value -> (t, a) eq option =
    function
    | V_mix (x, y) ->
       (match IV.identify x, IV.identify y with
        | Some Eq, Some Eq -> Some Eq
        | _ -> None
       )
    | _ -> None


  let debug fmt = Debug.debug ~channel:(fst @@ name) fmt


  (** Lattice operations *)
                
  let bottom = BOT

  let top = TOP

  let is_bottom = function
    | BOT -> true
    | I x -> IV.is_bottom x
    | F x -> FV.is_bottom x
    | TOP -> false

  let subset a b =
    match a,b with
    | I x, I y -> IV.subset x y
    | F x, F y -> FV.subset x y
    | BOT,_ | _,TOP -> true
    | _ -> false

  let join annot a b =
    match a,b with
    | I x, I y -> I (IV.join annot x y)
    | F x, F y -> F (FV.join annot x y)
    | BOT,x | x,BOT -> x
    | _ -> TOP

  let widen annot a b =
    match a,b with
    | I x, I y -> I (IV.widen annot x y)
    | F x, F y -> F (FV.widen annot x y)
    | BOT,x | x,BOT -> x
    | _ -> TOP

  let meet annot a b =
    match a,b with
    | I x, I y -> I (IV.meet annot x y)
    | F x, F y -> F (FV.meet annot x y)
    | TOP,x | x,TOP -> x
    | BOT,_ | _,BOT -> BOT
    | _ -> TOP

  let print_kind fmt = function
    | I x -> Format.pp_print_string fmt "int"
    | F x -> Format.pp_print_string fmt "float"
    | BOT -> Format.pp_print_string fmt Bot.bot_string
    | TOP -> Format.pp_print_string fmt Top.top_string
         
  let print fmt = function
    | I x -> IV.print fmt x
    | F x -> FV.print fmt x
    | BOT -> Format.pp_print_string fmt Bot.bot_string
    | TOP -> Format.pp_print_string fmt Top.top_string


  (** Utilities *)
         
  let is_int_op = function
    | O_log_not
    | O_minus
    | O_plus 
    | O_wrap _
    | O_mult
    | O_div
    | O_pow
    | O_log_or
    | O_log_and
    | O_mod
    | O_bit_and
    | O_bit_or
    | O_bit_xor
    | O_bit_rshift
    | O_bit_lshift
    | O_eq
    | O_ne
    | O_lt
    | O_gt
    | O_le
    | O_ge
      -> true
    | _
      -> false

  let is_float_op = function
    | O_float_cast _
    | O_float_minus _
    | O_float_plus _ 
    | O_float_sqrt _
    | O_float_mult _
    | O_float_div _
    | O_float_mod _
    | O_float_eq _
    | O_float_ne _
    | O_float_lt _
    | O_float_le _
    | O_float_gt _
    | O_float_ge _
    | O_float_neg_lt _
    | O_float_neg_le _
    | O_float_neg_gt _
    | O_float_neg_ge _
      -> true
    | _
      -> false

  let to_int : t -> IV.t = function
    | I x -> x
    | BOT -> IV.bottom
    | TOP -> IV.top
    | x -> panic "integer abstract value expected, got %a" print_kind x
         
  let to_float : t -> FV.t = function
    | F x -> x
    | BOT -> FV.bottom
    | TOP -> FV.top
    | x -> panic "float abstract value expected, got %a" print_kind x

  let return_int i =
    { i with value = I i.value; }
         
  let return_float i =
    { i with value = F i.value; }
         
  let return_int_pair i =
    { i with value = I (fst i.value), I (snd i.value); }
         
  let return_float_pair i =
    { i with value = F (fst i.value), F (snd i.value); }


  (** Arithmetic operators *)
    
  let of_constant x =
    match x with
    | C_float _ | C_float_interval _ ->
       F (FV.of_constant x)

    | C_int _ | C_int_interval _ ->
       I (IV.of_constant x)

    | _ -> TOP

  let unop op a =
    if is_int_op op then
      (* pure integer operators *)
      IV.unop op (to_int a) |> return_int
    else if is_float_op op then
      (* pure float operators *)
      FV.unop op (to_float a) |> return_float
    else
      (* conversions *)
      match op with
      | O_int_of_float ->
         return_int (C.int_of_float (to_float a))
      | O_float_of_int p ->
         return_float (C.float_of_int p (to_int a))
      | _ ->
         panic "unhandled operator" pp_operator op

  let binop op a1 a2 =
    if is_int_op op then
      IV.binop op (to_int a1) (to_int a2) |> return_int
    else if is_float_op op then
      FV.binop op (to_float a1) (to_float a2) |> return_float
    else
      panic "unhandled operator" pp_operator op

  let filter a b =
    match a with
    | I x -> return_int (IV.filter x b)
    | F x -> return_float (FV.filter x b)
    | BOT -> return BOT
    | TOP -> return TOP

  let bwd_unop op a r =
    if is_int_op op then
      IV.bwd_unop op (to_int a) (to_int r) |> return_int
    else if is_float_op op then
      FV.bwd_unop op (to_float a) (to_float r) |> return_float
    else
      panic "unhandled operator" pp_operator op

  let bwd_binop op a1 a2 r =
    if is_int_op op then
      IV.bwd_binop op (to_int a1) (to_int a2) (to_int r)
      |> return_int_pair
    else if is_float_op op then
      FV.bwd_binop op (to_float a1) (to_float a2) (to_float r)
      |> return_float_pair
    else
      panic "unhandled operator" pp_operator op
           
  let compare op a1 a2 r =
    if is_int_op op then
      IV.compare op (to_int a1) (to_int a2) r |> return_int_pair
    else if is_float_op op then
      FV.compare op (to_float a1) (to_float a2) r |> return_float_pair
    else
      panic "unhandled operator" pp_operator op



  (** Queries *)


  let is_int_query : type r . r Framework.Query.query -> bool =
    function
    | Intervals.Value.Q_interval _ -> true
    | _ -> false

  let is_float_query : type r . r Framework.Query.query -> bool =
    function
    | Float_intervals.Value.Q_float_interval _ -> true
    | _ -> false
    
  let ask : type r. r Framework.Query.query -> (expr -> t) -> r option =
    fun query eval ->
    if is_int_query query then
      IV.ask query (fun e -> to_int (eval e))
    else if is_float_query query then
      FV.ask query (fun e -> to_float (eval e))
    else
      None

end


(** Application to interval integer/float abstractions *)

module Value = Make(Intervals.Value)
                   (Float_intervals.Value)
                   (Float_intervals.Value (* casts implemented also here *))
  
let () =
  register_value (module Value)
        
