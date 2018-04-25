(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Control flow abstraction for switch statements. *)

open Framework.Flow
open Framework.Domains
open Framework.Manager
open Framework.Domains.Stateless
open Framework.Eval
open Framework.Context
open Framework.Lattice
open Framework.Ast
open Universal.Ast
open Ast

let name = "c.flows.goto"
let debug fmt = Debug.debug ~channel:name fmt


(*==========================================================================*)
(**                            {2 Flow tokens}                              *)
(*==========================================================================*)

type Framework.Flow.token +=
  | TGoto of string
  (** Goto environments *)


(*==========================================================================*)
(**                        {2 Abstract domain}                              *)
(*==========================================================================*)


module Domain =
struct

  (*==========================================================================*)
                        (** {2 Transfer functions} *)
  (*==========================================================================*)

  let init man ctx prog flow = ctx, flow

  let exec man ctx stmt flow =
    match skind stmt with
    | S_c_goto s ->
      (* Save TCur env in TGoto s token, then set TCur to bottom. *)
      let cur = man.flow.get TCur flow in
      let flow0 = man.flow.add (TGoto s) cur flow |>
                  man.flow.remove TCur
      in
      return flow0
    | S_c_label s ->
      let fromlbl = man.flow.get (TGoto s) flow in
      let flow0 = man.flow.add TCur fromlbl flow |>
                  man.flow.remove (TGoto s)
      in
      return flow0
    | S_c_goto_stab stmt' ->
      begin
        let nogotos,gotos = man.flow.fold (fun (nogotos, gotos) v k -> match k with
            | TGoto s -> (nogotos, man.flow.add k v gotos)
            | _       -> (man.flow.add k v nogotos, gotos)
          ) (man.flow.bottom, man.flow.bottom) flow in
        let next f f' i wid_limit =
          let get_gotos f = man.flow.filter
              (fun e t -> match t with | TGoto s -> true | _ -> false) f
          in
          let f1, f1' = get_gotos f, get_gotos f' in
          if man.flow.leq f1' f1 then
            None
          else if i >= wid_limit then
            Some (man.flow.join f1' (man.flow.widening ctx f1
                                       (man.flow.join f1 f1')))
          else Some (man.flow.join f1' (man.flow.join f1 f1'))
        in
        let rec stabilization f i wid_limit =
          let f' = man.exec ctx stmt' f in
          match next f f' i wid_limit with
          | None -> f'
          | Some f'' -> stabilization f'' (i+1) wid_limit
        in
        let flow1 = stabilization nogotos 0 3 in
        let flow1_minus_gotos = man.flow.filter (fun v k -> match k with
            | TGoto s -> false | _ -> true) flow1 in
        let flow2 = man.flow.join flow1_minus_gotos gotos in
        return flow2
      end
    | _ -> None

  let eval man ctx exp flow = None

  let ask _ _ _ _  = None

  end

let setup () =
  Stateless.register_domain name (module Domain);
  Framework.Flow.register_pp_token (fun next fmt -> function
      | TGoto str -> Format.fprintf fmt "goto %s" str
      | tk -> next fmt tk
    );
  Framework.Flow.register_token_compare (fun next -> fun a b ->
      match a,b with
      | TGoto x, TGoto y -> compare x y
      | _ -> next a b
    )
