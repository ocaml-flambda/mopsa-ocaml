(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Inter-procedural iterator by inlining.  *)

open Framework.Essentials
open Ast


(** {2 Return flow token} *)
(** ===================== *)

type token +=
  | T_return of range * expr option
  (** [T_return(l, Some e)] represents flows reaching a return
     statement at location [l] returning an expression [e]. The
     expression is [None] when the function returns nothing
      (i.e. case of a procedure). *)

let () =
  register_token {
    compare = (fun next tk1 tk2 ->
      match tk1, tk2 with
      | T_return(r1, _), T_return(r2, _) -> compare_range r1 r2
      | _ -> next tk1 tk2
    );
  print = (fun next fmt -> function
        | T_return(r, Some e) -> Format.fprintf fmt "return %a" pp_expr e
        | T_return(r, None) -> Format.fprintf fmt "return"
        | tk -> next fmt tk
      );
  }




(** {2 Domain definition} *)
(** ===================== *)

module Domain : Framework.Domains.Stateless.S =
struct

  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_universal_intraproc : unit domain
  let id = D_universal_intraproc
  let name = "universal.iterators.intraproc"
  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_universal_intraproc -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let zone = Zone.Z_universal
  let import_exec = []
  let import_eval = []


  (** Initialization *)
  (** ============== *)

  let init prog man flow = None


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec stmt man flow =
    match skind stmt with
    | S_return e ->
      Some (
        let cur = Flow.get T_cur man flow in
        Flow.add (T_return (stmt.srange, e)) cur man flow |>
        Flow.remove T_cur man |>
        Post.of_flow
      )

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_call({ekind = E_function f}, args) ->
      (* Clear all return flows *)
      let flow0 = Flow.filter (fun tk env ->
          match tk with
          | T_return _ -> false
          | _ -> true
        ) man flow
      in

      (* Assign arguments to parameters *)
      let parameters_assign = List.mapi (fun i (param, arg) ->
          mk_assign (mk_var param range) arg range
        ) (List.combine f.fun_parameters args) in

      let init_block = mk_block parameters_assign range in

      (* Execute body *)
      let flow1 = man.exec init_block flow0 |>
                  man.exec f.fun_body
      in

      (* Create a temporary variable to store return expressions *)
      let tmp = mk_tmp ~vtyp:f.fun_return_type () in

      (* Iterate over return flows and assign the returned value to tmp *)
      let flow2 =
        Flow.fold (fun acc tk env ->
            match tk with
            | T_return(_, None) -> Flow.add T_cur env man acc

            | T_return(_, Some e) ->
              Flow.set T_cur env man acc |>
              man.exec (mk_assign (mk_var tmp range) e range) |>
              Flow.join man acc

            | _ -> Flow.add tk env man acc
          )
          (Flow.remove T_cur man flow)
          man flow1
      in

      (* Remove parameters and local variables from the environment *)
      let ignore_stmt_list =
        List.mapi (fun i v ->
            mk_remove_var v range
          ) (f.fun_parameters @ f.fun_locvars)
      in

      let ignore_block = mk_block ignore_stmt_list range in

      let flow3 = man.exec ignore_block flow2 in

      (* Re-evaluate the expression [tmp] from the top-level *)
      Some (
        man.eval (mk_var tmp range) flow3 |>
        Eval.bind @@ fun e' flow ->
        Eval.singleton e' flow ~cleaners:[mk_remove_var tmp range]
      )

    | _ -> None

  let ask _ _ _ = None

end


let () =
  Framework.Domains.Stateless.register_domain (module Domain)
