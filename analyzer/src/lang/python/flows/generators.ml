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
  | TGenStart of addr (** Initial generator flows *)
  | TGenNext of addr * range (** Flows starting from a call to
                                __next__ that should resume execution
                                 at the given location point *)
  | TGenYield of addr * expr * range (** Flow starting from a yield
                                        expression and suspended until
                                        reaching the calling next
                                         statement *)
  | TGenStop of addr (** Flows reaching the end of the generator *)



(* Generator framing: encode a generator local variable into a
     uniquely named variable depending on the address of the instance
     generator. *)
type var_kind +=
  | V_gen_frame of addr (** address of the instance *)

let mk_framed_var v addr =
  match vkind v with
  | V_orig -> {v with vkind = V_gen_frame addr}
  | V_gen_frame _ -> v
  | _ -> assert false


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
      assert false

    (* E⟦ generator.__next__(self) | ¬ isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__next__")})},
        [{ekind = E_addr ({addr_kind = A_py_instance(_, Some (Generator func))} as addr)}],
        []
      ) ->
      assert false

    (* E⟦ yield e ⟧ *)
    | E_py_yield e ->
      assert false

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
    )
