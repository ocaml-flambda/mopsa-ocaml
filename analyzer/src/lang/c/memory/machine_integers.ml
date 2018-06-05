(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Machine representation of C integers. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Exec
open Framework.Lattice
open Framework.Ast
open Universal.Ast
open Universal.Utils
open Ast

let name = "c.memory.machine_integers"
let debug fmt = Debug.debug ~channel:name fmt

let range_leq (a,b) (c,d) =
  Z.leq c a && Z.leq b d

let wrap_z (z : Z.t) ((l,h) : Z.t * Z.t) : Z.t =
  Z.( l + ((z - l) mod (h-l+one)) )

let is_c_int_op = function
  | O_div t | O_mod t | O_mult t | O_plus t | O_minus t when t |> is_c_int_type -> true
  | _ -> false

let is_c_div = function
  | O_div t | O_mod t when t |> is_c_type -> true
  | _ -> false

let cast_alarm = ref true

let check_overflow typ man ctx range f1 f2 exp =
  let rmin, rmax = rangeof typ in

  let rec fast_check e flow =
    let v = man.ask ctx (Universal.Numeric.Query.QIntInterval e) flow in
    match v with
    | None -> assert false
    | Some itv ->
      debug "overflow interval = %a" Universal.Numeric.Values.Int.print itv;
      if Universal.Numeric.Values.Int.is_bottom itv then oeval_singleton (None, flow, [])
      else
        try
          let l, u = Universal.Numeric.Values.Int.get_bounds itv in
          if Z.geq l rmin && Z.leq u rmax then f1 e flow
          else if Z.lt u rmin || Z.gt l rmax then f2 e flow
          else full_check e flow
        with Universal.Numeric.Values.Int.Unbounded ->
          full_check e flow

  and full_check e flow =
    let cond = range_cond e rmin rmax (erange e) in
    assume_to_eval cond
      (fun tflow -> f1 e flow)
      (fun fflow -> f2 e flow)
      man ctx flow ()
  in
  eval_compose (fun e flow ->
      (* Start with a fast interval check *)
      fast_check e flow
    ) exp

let check_division man ctx range f1 f2 exp =
  let rec fast_check e flow =
    let v = man.ask ctx (Universal.Numeric.Query.QIntInterval e) flow in
    match v with
    | None -> assert false
    | Some itv ->
      debug "div interval = %a" Universal.Numeric.Values.Int.print itv;
      if Universal.Numeric.Values.Int.is_bottom itv then oeval_singleton (None, flow, [])
      else
        try
          let l, u = Universal.Numeric.Values.Int.get_bounds itv in
          if Z.gt l Z.zero || Z.lt u Z.zero then f1 e flow
          else if Z.equal u Z.zero && Z.equal l Z.zero then f2 e flow
          else full_check e flow
        with Universal.Numeric.Values.Int.Unbounded ->
          full_check e flow

  and full_check e' flow =
    let emint' = {e' with etyp = T_int} in
    let cond = {ekind = E_binop(O_eq, emint', mk_z Z.zero (tag_range range "div0"));
                etyp  = T_bool;
                erange = tag_range range "div0cond"
               }
    in
    assume_to_eval cond
      (fun tflow -> f2 e' tflow)
      (fun fflow -> f1 e' fflow)
      man ctx flow ()
  in
  eval_compose (fun e' flow ->
      fast_check e' flow
    ) exp


let is_math_type = function
  | T_int | T_float -> true
  | _ -> false

let to_universal_type t =
  if is_c_scalar_type t then
    if is_c_int_type t then
      T_int
    else
      T_float
  else
    match t with
    | T_int | T_float | T_bool | T_any | T_c_void -> t
    | _ -> Debug.fail "[to_universal_type] in machine_integers called on non scalar type ; %a" Framework.Pp.pp_typ t

let var_machine_integers v =
  {v with vtyp = to_universal_type v.vtyp}


let is_stmt_universal stmt =
  let exception No in
  try
    Framework.Visitor.fold_stmt
      (fun a e -> if e |> etyp |> is_math_type then true else raise No )
      (fun a s -> a)
      true
      stmt
  with
  | No -> false

let acceptable_univ_ekind ek = match ek with
  | E_var(_) | E_binop(_) | E_unop(_) | E_constant(_) -> true
  | _ -> false

let is_expr_universal exp =
  let exception No in
  try
    Framework.Visitor.fold_expr
      (fun a e -> match ekind e with
         | E_var v ->
           if e |> etyp |> is_math_type && v.vtyp |> is_math_type
           then true else raise No
         | _       ->
           if e |> etyp |> is_math_type && e |> ekind |> acceptable_univ_ekind then true else raise No )
      (fun a s -> a)
      true
      exp
  with
  | No -> false

(** Abstract domain. *)

module Domain(N : Framework.Domains.Stateful.DOMAIN) =
struct

  (*==========================================================================*)
  (**                        {2 Transfer functions}                           *)
  (*==========================================================================*)

  include N

  let init man ctx prog flow = ctx, flow

  let rec loc_eval man ctx =
    fun e flow -> match leval man ctx e flow with
      | Some x -> x
      | None   -> Debug.fail "[eval] in machine_integers, local eval yielded back None"

  and leval man ctx exp flow =
    let () = debug "leval : %a" Framework.Pp.pp_expr exp in
    let range = erange exp in
    if is_expr_universal exp then
      oeval_singleton (Some exp, flow, [])
    else
      begin
        match ekind exp with
        | E_binop(op, e, e')
          when op |> is_c_div ->
          loc_eval man ctx e' flow |>
          check_division man ctx range
            (fun e' tflow -> re_eval_singleton (loc_eval man ctx)
                (Some { exp with ekind = E_binop(to_math_op op, e, e')}, tflow, [])
            )
            (fun e' fflow ->
               let cur = man.flow.get TCur fflow in
               let tflow2 = man.flow.add (Alarms.TDivideByZero range) cur fflow
                            |> man.flow.set TCur man.env.bottom
               in
               oeval_singleton (None, tflow2, [])
            )

        | E_unop(op, e) when is_c_int_op op ->
          let () = debug "case 1" in
          let typ = etyp exp in
          let rmin, rmax = rangeof typ in
          let nop = to_math_op op in
          loc_eval man ctx {exp with ekind = E_unop(nop, e) } flow  |>
          check_overflow typ man ctx range
            (fun e tflow -> re_eval_singleton (loc_eval man ctx) (Some e, tflow, []))
            (fun e fflow ->
               let cur = man.flow.get TCur fflow in
               let () = debug "Adding alarm flow : %a" man.env.print cur in
               let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
               in
               re_eval_singleton (loc_eval man ctx)
                 (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                        etyp   = typ;
                        erange = tag_range range "wrap"
                       }), fflow2, []
                 )
            )

        | E_binop(op, e, e') when is_c_int_op op ->
          let () = debug "case 2" in
          let typ = etyp exp in
          let rmin, rmax = rangeof typ in
          let nop = to_math_op op in
          loc_eval man ctx {exp with ekind = E_binop(nop, e, e') } flow |>
          check_overflow typ man ctx range
            (fun e tflow -> re_eval_singleton (loc_eval man ctx) (Some e, tflow, []))
            (fun e fflow ->
               let cur = man.flow.get TCur fflow in
               let () = debug "Adding alarm flow : %a" man.env.print cur in
               let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
               in
               re_eval_singleton (loc_eval man ctx)
                 (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                        etyp   = typ;
                        erange = tag_range range "wrap"
                       }), fflow2, []
                 )
            )

        | E_binop(op, e, e') ->
          let () = debug "case 3" in
          eval_list [e; e'] (loc_eval man ctx) flow |>
          eval_compose (fun el flow ->
              match el with
              | [e; e'] ->
                oeval_singleton
                  (Some {exp with ekind = E_binop(op, e, e') ; etyp = to_universal_type (etyp exp)},
                   flow,
                   []
                  )
              | _ -> assert false
            )

        | E_unop(op, e) ->
          let () = debug "case 4" in
          eval_list [e] (loc_eval man ctx) flow |>
          eval_compose (fun el flow ->
              match el with
              | [e] ->
                oeval_singleton
                  (Some {exp with ekind = E_unop(op, e) ; etyp = to_universal_type (etyp exp)},
                   flow,
                   []
                  )
              | _ -> assert false
            )

        | E_c_cast({ekind = E_constant (C_int z)}, _) when exp |> etyp |> is_c_int_type ->
          let () = debug "case 9" in
          let r = exp |> etyp |> rangeof in
          if range_leq (z,z) r then
            oeval_singleton (Some (mk_z z range), flow, [])
          else
            let cur = man.flow.get TCur flow in
            let flow2 = man.flow.add (Alarms.TIntegerOverflow range) cur flow in
            oeval_singleton (Some (mk_z (wrap_z z r) (tag_range range "wrapped")), flow2, [])


        | E_c_cast(e, b) when exp |> etyp |> is_c_int_type && e |> etyp |> is_c_int_type ->
          let () = debug "case 5" in
          let t  = etyp exp in
          let t' = etyp e in
          let r = rangeof t in
          let r' = rangeof t' in
          if range_leq r' r then
            re_eval_singleton (loc_eval man ctx) (Some e, flow, [])
          else
            let rmin, rmax = rangeof t in
            loc_eval man ctx e flow |>
            check_overflow t man ctx range
              (fun e tflow -> re_eval_singleton (loc_eval man ctx) (Some {e with etyp = t}, tflow, []))
              (fun e fflow ->
                 if b && not (!cast_alarm) then
                   begin
                     debug "false flow : %a" man.flow.print fflow ;
                     re_eval_singleton (loc_eval man ctx)
                       (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                              etyp   = t;
                              erange = tag_range range "wrap"
                             }), fflow, []
                       )
                   end
                 else
                   begin
                     debug "false flow : %a" man.flow.print fflow ;
                     let cur = man.flow.get TCur fflow in
                     let fflow2 = man.flow.add (Alarms.TIntegerOverflow range) cur fflow
                     in
                     re_eval_singleton (loc_eval man ctx)
                       (Some({ekind  = E_unop(O_wrap(rmin, rmax), e);
                              etyp   = t;
                              erange = tag_range range "wrap"
                             }), fflow2, []
                       )
                   end
              )

        | E_constant(C_c_character (c, _)) ->
          let () = debug "case 6" in
          re_eval_singleton (loc_eval man ctx) (Some ({exp with ekind = E_constant (C_int c)}), flow, [])

        | E_var v ->
          let () = debug "case 8" in
          oeval_singleton
            (Some {exp with ekind = E_var({v with vtyp = to_universal_type v.vtyp}); etyp = to_universal_type (etyp exp)},
             flow,
             []
            )

        | E_constant( _ ) ->
          oeval_singleton
            (Some {exp with etyp = to_universal_type (etyp exp)},
             flow,
             []
            )

        | _ ->
          let () = debug "%a could not be dealt with by machine_integers, \
                          giving back to man"
              Framework.Pp.pp_expr exp
          in
          man.eval ctx exp flow |>
          eval_compose
            (fun e flow -> leval man ctx e flow)
      end

  let rec eval man ctx exp flow =
    None

  let lift s flow =
    oeval_singleton (Some s, flow, [])

  let universalize man ctx stmt flow =
    (* if is_stmt_universal stmt then
     *   lift stmt flow
     * else *)
    match skind stmt with
    | S_assign(e, e', m) ->
      eval_list [e; e'] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          eval_list el (loc_eval man ctx) flow |>
          eval_compose (fun el flow ->
              match el with
              | [e; e'] ->
                lift {stmt with skind = S_assign(e, e', m)} flow
              | _ -> assert false
            )
        )

    | S_assume(e) ->
      let () = debug "got stmt %a" Framework.Pp.pp_stmt stmt in
      eval_list [e] (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          eval_list el (loc_eval man ctx) flow |>
          eval_compose (fun el flow ->
              match el with
              | [e] ->
                lift {stmt with skind = S_assume(e)} flow
              | _ -> assert false
            )
        )


    | S_rename_var(v, v') ->
      lift {stmt with skind = S_rename_var(var_machine_integers v,
                                           var_machine_integers v')} flow

    | S_project_vars vars ->
      lift {stmt with skind = S_project_vars(List.map var_machine_integers vars)} flow

    | S_remove_var v ->
      lift {stmt with skind = S_remove_var (var_machine_integers v)} flow

    | _ -> None

  let exec man ctx stmt flow =
    let () = debug "machine_integers input exec : %a" Framework.Pp.pp_stmt stmt in
    universalize man ctx stmt flow |>
    oeval_to_oexec (fun s flow ->
        let () = debug "passing : %a" Framework.Pp.pp_stmt stmt in
        N.exec man ctx s flow
      ) (man.exec ctx) man.flow

  let pass_on query man ctx el flow builder =
    begin
      let xl = eval_list el (loc_eval man ctx) flow in
      eval_substitute (fun (x, f, sl) ->
          match x with
          | Some el ->
            let () =
              match el with
              | p::q -> debug "eval is : %a" Framework.Pp.pp_expr p
              | [] -> ()
            in
            N.ask man ctx (builder el) f
          | None -> None
        ) (Framework.Query.join query) (Framework.Query.meet query) xl
    end

  let ask : type r. ('a, t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a Framework.Flow.flow -> r option =
    fun man ctx query flow ->
      match query with
      | Universal.Numeric.Query.QIntStepInterval exp ->
        pass_on query man ctx [exp] flow (fun l -> Universal.Numeric.Query.QIntStepInterval (List.nth l 0))
      | Universal.Numeric.Query.QIntInterval exp ->
        begin
          let rep = pass_on query man ctx [exp] flow (fun l -> Universal.Numeric.Query.QIntInterval (List.nth l 0)) in
          let () = match rep with
            | Some itv -> debug "I am asked bounds on %a answer is %a in@\n%a" Framework.Pp.pp_expr exp Universal.Numeric.Values.Int.print itv man.flow.print flow
            | None -> debug "I am asked bounds on %a answer is None" Framework.Pp.pp_expr exp
          in rep
        end

      | _ ->
        None

end


(*==========================================================================*)
(**                            {2 Setup}                                    *)
(*==========================================================================*)

let setup () =
  Framework.Domains.Fun.register_domain name (module Domain);
  Framework.Options.register (
    "-cast-alarm",
    Arg.Bool(fun b -> cast_alarm := b),
    "cast overflow alarms (default: true)"
  );
