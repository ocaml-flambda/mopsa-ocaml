(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Mixing float and integer value abstractions. *)

open Mopsa
open Framework.Value
open Ast


(** General functor for arbitrary integer/float abstractions. *)
  
module Make(IV:VALUE)(FV:VALUE) =
struct
  
  (** Types *)

  module B = ItvUtils.IntBound
  module FItv = ItvUtils.FloatItv
  module FItvNan = ItvUtils.FloatItvNan

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
    
  let of_constant t x =
     if is_int_type t then
       I (IV.of_constant t x)
     else if is_float_type t then
       F (FV.of_constant t x)
     else
       panic "unhandled constant" pp_constant x

    
  let null_expr = mk_zero (mk_fresh_range ())
         
  let unop t op a =
    match op, t, a with
    | O_cast, T_float p, I v ->
       (* float of int *)
       (match IV.ask (Intervals.Value.Q_interval null_expr) (fun _ -> v) with
        | Some (Nb (B.Finite lo, B.Finite up)) ->
           FV.of_constant t (C_int_interval (lo,up)) |> return |> return_float
        | Some BOT -> return BOT
        | _ -> return TOP
       )
    | O_cast, T_int, F v ->
       (* int of float *)
       (* TODO: in case of Nan, infinities, signal an error *)
       (match FV.ask (Float_intervals.Value.Q_float_interval null_expr) (fun _ -> v) with
        | Some { FItvNan.itv = Nb { FItv.lo = lo; FItv.up = up; }; } ->
           IV.of_constant t (C_float_interval (lo,up)) |> return |> return_int
        | Some { FItvNan.itv = BOT; } -> return BOT
        | _ -> return TOP
       )
    | O_cast, _, TOP -> return TOP
    | O_cast, _, BOT -> return BOT
    | _ ->
       if is_int_type t then
         (* pure integer operators *)
         IV.unop t op (to_int a) |> return_int
       else if is_float_type t then
         (* pure float operators *)
         FV.unop t op (to_float a) |> return_float
       else
         panic "unhandled operator" pp_operator op
      
  let binop t op a1 a2 =
    if is_int_type t then
      IV.binop t op (to_int a1) (to_int a2) |> return_int
    else if is_float_type t then
      FV.binop t op (to_float a1) (to_float a2) |> return_float
    else
      panic "unhandled operator" pp_operator op

  let filter t a b =
    match a with
    | I x -> return_int (IV.filter t x b)
    | F x -> return_float (FV.filter t x b)
    | BOT -> return BOT
    | TOP -> return TOP

  let bwd_unop t op a r =
    if is_int_type t then
      IV.bwd_unop t op (to_int a) (to_int r) |> return_int
    else if is_float_type t then
      FV.bwd_unop t op (to_float a) (to_float r) |> return_float
    else
      panic "unhandled operator" pp_operator op

  let bwd_binop t op a1 a2 r =
    if is_int_type t then
      IV.bwd_binop t op (to_int a1) (to_int a2) (to_int r)
      |> return_int_pair
    else if is_float_type t then
      FV.bwd_binop t op (to_float a1) (to_float a2) (to_float r)
      |> return_float_pair
    else
      panic "unhandled operator" pp_operator op
           
  let compare t op a1 a2 r =
    if is_int_type t then
      IV.compare t op (to_int a1) (to_int a2) r |> return_int_pair
    else if is_float_type t then
      FV.compare t op (to_float a1) (to_float a2) r |> return_float_pair
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

module Value = Make(Intervals.Value)(Float_intervals.Value)
  
let () =
  register_value (module Value)
        
