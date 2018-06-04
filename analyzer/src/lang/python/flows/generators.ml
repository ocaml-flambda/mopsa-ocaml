(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of generators *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Lattice
open Framework.Flow
open Framework.Ast
open Framework.Pp
open Framework.Eval
open Framework.Exec
open Framework.Alarm
open Universal.Ast
open Ast
open Addr

let name = "python.flows.generators"
let debug fmt = Debug.debug ~channel:name fmt


type token +=
  | TGenStart of addr
  (** Initial generator flows *)

  | TGenNext of addr * range
  (** Flows starting from a call to __next__ that should resume
      execution at the given location point *)

  | TGenYield of addr * expr * range
  (** Flow starting from a yield expression and suspended until
      reaching the calling next statement *)

  | TGenStop of addr
  (** Flows reaching the end of the generator *)



(** Generator framing: tag local variables with the address of the
   generator in order to keep separate variables of different
   instances, while allowing the inference of relations. *)
type var_kind +=
  | V_gen_frame of addr (** address of the instance *)

let mk_framed_var v addr =
  match vkind v with
  | V_orig -> {v with vkind = V_gen_frame addr}
  | V_gen_frame _ -> v
  | _ -> assert false


(** The current generator being analyzed is stored in the context. *)
type _ Framework.Context.key +=
  | KCurGenerator: addr Framework.Context.key


module Domain = struct

  let eval man ctx exp flow =
    let range = erange exp in
    match ekind exp with
    (* E⟦ g(e1, e2, ...) | is_generator(g) ⟧ *)
    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_user func)})}, args, [])
      when func.py_func_is_generator = true
      ->
      eval_list args (man.eval ctx) flow |>
      eval_compose (fun el flow ->
          (* Create the generator instance *)
          eval_alloc_instance man ctx (Addr.find_builtin "generator") (Some (Generator func)) range flow |>
          oeval_compose (fun addr flow ->
              let flow0 = flow in
              (* Assign arguments to parameters in a new flow *)

              (* FIXME: default arguments in generators are not supported yet *)
              if List.length args <> List.length func.py_func_parameters then
                Framework.Exceptions.panic_at range "generators: only calls with correct number of arguments is supported"
              else
                (* Change all parameters into framed variables *)
                let params = List.map (fun v -> mk_framed_var v addr) func.py_func_parameters in

                (* Perform assignments to arguments *)
                let flow1 = List.fold_left (fun flow (v, e) ->
                    man.exec ctx (mk_assign (mk_var v range) e range) flow
                  ) flow (List.combine params args)
                in

                (* Save the projected cur env in the initial flow of the generator *)
                let cur' = man.exec ctx (mk_project_vars params range) flow1 |>
                           man.flow.get TCur
                in
                let flow2 = man.flow.add (TGenStart addr) cur' flow0 in
                oeval_singleton (Some (mk_addr addr range), flow2, [])
            )
        )

    (* E⟦ generator.__iter__(self) | isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__iter__")})},
        [{ekind = E_addr {addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "generator", _)}, _)}} as self],
        []
      ) ->
      oeval_singleton (Some self, flow, [])

    (* E⟦ generator.__iter__(self) | ¬ isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__iter__")})},
        _,
        []
      ) ->
      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
      oeval_singleton (None, flow, [])


    (* E⟦ generator.__next__(self) | isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__next__")})},
        [{ekind = E_addr ({addr_kind = A_py_instance(_, Some (Generator func))} as addr)}],
        []
      ) ->
      (* Keep the input cur environment *)
      let cur = man.flow.get TCur flow in

      (* Compute the next tokens *)
      let flow1 = man.flow.fold (fun acc env -> function
          | TCur -> acc
          | TGenStart(g) when compare_addr g addr = 0 -> man.flow.add TCur env acc
          | TGenNext _ -> acc

          | TGenYield(g, _, r) when compare_addr g addr = 0 -> man.flow.add (TGenNext(g, r)) env acc
          | TGenYield _ -> acc

          | TGenStop(g) -> acc (* this case is handled later *)
          | Universal.Flows.Interproc.TReturn _ -> acc
          | tk -> man.flow.add tk env acc
        ) man.flow.bottom flow
      in

      (* Filter flow by location reachability *)
      (* FIXME: add relational counter *)
      let flow2 = man.flow.map (fun env -> function
          | TCur -> man.env.meet env cur
          | TGenNext(g, r) -> man.env.meet env cur
          | _ -> env
        ) flow1
      in

      (* Modify the body of the generator by changing local variables into framed variables *)
      let is_local v = List.exists (fun v' -> compare_var v v' = 0) (func.py_func_locals @ func.py_func_parameters) in
      let body' = Framework.Visitor.map_stmt
          (fun expr ->
             match ekind expr with
             | E_var v when is_local v -> {expr with ekind = E_var (mk_framed_var v addr)}
             | _ -> expr
          )
          (fun stmt -> stmt)
          func.py_func_body
      in
      debug "body after variable renaming:@\n @[%a@]" pp_stmt body';
      let locals' = List.map (fun v -> mk_framed_var v addr) (func.py_func_locals @ func.py_func_parameters) in

      (* Execute the body statement *)
      let ctx' = Framework.Context.add KCurGenerator addr ctx in
      let flow3 = man.exec ctx' body' flow2 in

      (* Add the input stop flows  *)
      let flow3 = man.flow.fold (fun acc env tk ->
          match tk with
          | TGenStop(g) when compare_addr g addr = 0 -> man.flow.add tk env acc
          | _ -> acc
        ) flow3 flow
      in


      (* Restore the input flows *)
      let flow4 = man.flow.fold (fun acc env tk ->
          match tk with
          | Universal.Flows.Interproc.TReturn _ -> man.flow.add tk env acc
          | TGenStart(g) when compare_addr g addr <> 0 -> man.flow.add tk env acc
          | TGenYield(g, _, _) when compare_addr g addr <> 0 -> man.flow.add tk env acc
          | TGenStop(g) when compare_addr g addr <> 0 -> man.flow.add tk env acc
          | _ -> acc
        ) man.flow.bottom flow
      in

      (* Process the resulting yield, return and exception flows *)
      man.flow.fold (fun acc env -> function
          | TGenYield(g, e, r) when compare_addr g addr = 0 ->
            (* Assign the yielded value to a temporary return variable *)
            let tmp = mktmp () in
            let flow = man.flow.set TCur env flow4 |>
                       man.exec ctx (mk_assign (mk_var tmp range) e range)
            in
            (* Clean the cur environment by removing the generator local variables *)
            let flow = List.fold_left (fun acc v ->
                man.exec ctx (mk_remove_var v range) acc
              ) flow locals'
            in
            (* Clean the yield frame by projecting on the generator local variables *)
            let cur' = man.flow.set TCur env flow4 |>
                       man.exec ctx (mk_project_vars locals' range) |>
                       man.flow.get TCur
            in
            let flow = man.flow.set (TGenYield(g, e, r)) cur' flow in
            re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow, [mk_remove_var tmp range]) |>
            oeval_join acc

          | Exceptions.TExn exn ->
            (* Save env in the token TGenStop and re-raise the exception *)
            let flow = man.flow.add (TGenStop(addr)) env flow4 |>
                       man.flow.set (Exceptions.TExn exn) env
            in
            oeval_singleton (None, flow, []) |>
            oeval_join acc

          | TGenStop _
          | Universal.Flows.Interproc.TReturn _ ->
            (* Save env in the token TGenStop and raise a StopIteration exception *)
            let flow = man.flow.add (TGenStop(addr)) env flow4 |>
                       man.flow.set TCur env |>
                       man.exec ctx (Utils.mk_builtin_raise "StopIteration" range)
            in
            oeval_singleton (None, flow, []) |>
            oeval_join acc

          | _ -> acc
        ) None flow3

    (* E⟦ generator.__next__(self) | ¬ isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__next__")})},
        _,
        []
      ) ->
      let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
      oeval_singleton (None, flow, [])

    (* E⟦ yield e ⟧ *)
    | E_py_yield e ->
      let g = Framework.Context.find KCurGenerator ctx in
      let flow = man.flow.fold (fun acc env tk ->
          match tk with
          | TCur -> man.flow.add (TGenYield(g, e, range)) env acc
          | TGenNext(g, r) -> man.flow.add TCur env acc
          | _ -> man.flow.add tk env acc
        ) man.flow.bottom flow
      in
      oeval_singleton (None, flow, [])

    (* E⟦ x for x in g | isinstance(g, generator) ⟧ *)
    | E_py_generator_comprehension _ ->
      Framework.Exceptions.panic "Generator comprehension not supported"

    | _ -> None

  let init man ctx prog flow = ctx, flow
  let exec man ctx stmt flow = None
  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain);
  (* Flow tokens *)
  let open Format in
  let open Framework.Pp in
  let open Universal.Pp in
  register_pp_token (fun next fmt -> function
      | TGenStart(gen) -> fprintf fmt "gstart(%a)" pp_addr gen
      | TGenNext(gen, r) -> fprintf fmt "gnext(%a) -> %a" pp_addr gen pp_range r
      | TGenYield(gen, e, r) -> fprintf fmt "gyield(%a) <- %a" pp_addr gen pp_range r
      | TGenStop(gen) -> fprintf fmt "gstop(%a)" pp_addr gen
      | tk -> next fmt tk
    );
  register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TGenStart(g1), TGenStart(g2) -> compare_addr g1 g2
      | TGenNext(g1, r1), TGenNext(g2, r2) ->
        compare_composer [
          (fun () -> compare_addr g1 g2);
          (fun () -> compare_range r1 r2);
        ]
      | TGenYield(g1, _, r1), TGenYield(g2, _, r2) ->
        compare_composer [
          (fun () -> compare_addr g1 g2);
          (fun () -> compare_range r1 r2);
        ]
      | TGenStop(g1), TGenStop(g2) -> compare_addr g1 g2
      | _ -> next tk1 tk2
    );
  (* Variable kinds *)
  register_pp_var (fun next fmt v ->
      match vkind v with
      | V_gen_frame(g) -> fprintf fmt "∥%s~%a∥" v.vname pp_addr g
      | _ -> next fmt v
    );
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_gen_frame g1, V_gen_frame g2 -> compare_addr g1 g2
      | _ -> next vk1 vk2
    );
  (* Context *)
  let open Framework.Context in
  register_key_equality {
    case = (let f : type a b. chain -> a key -> b key -> (a, b) eq option =
              fun chain k1 k2 ->
                match k1, k2 with
                | KCurGenerator, KCurGenerator -> Some Eq
                | _ -> chain.check k1 k2
            in
            f);
  }
