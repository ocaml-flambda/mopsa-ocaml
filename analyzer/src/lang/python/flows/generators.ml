(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of generators *)

open Framework.Essentials
open Framework.Domains.Stateless
open Universal.Ast
open Ast
open Addr

let name = "python.flows.generators"
let debug fmt = Debug.debug ~channel:name fmt


type Flow.token +=
  | TGenStart of py_object
  (** Initial generator flows *)

  | TGenNext of py_object * range
  (** Flows starting from a call to __next__ that should resume
      execution at the given location point *)

  | TGenYield of py_object * expr * range
  (** Flow starting from a yield expression and suspended until
      reaching the calling next statement *)

  | TGenStop of py_object
  (** Flows reaching the end of the generator *)



let mk_framed_var v obj =
  let vname =
    Format.fprintf Format.str_formatter "∥%s~%a∥" v.vname pp_py_object obj;
    Format.flush_str_formatter ()
  in
  {v with vname}


(** The current generator being analyzed is stored in the context. *)
type _ Framework.Context.key +=
  | KCurGenerator: py_object Framework.Context.key


module Domain = struct

  let get_generator_function obj =
    match kind_of_object obj with
    | A_py_instance(_, Some (Generator f)) -> f
    | _ -> assert false

  let import_exec = []
  let export_exec = [Zone.Z_py]

  let import_eval = [Zone.Z_py, Zone.Z_py_object; Universal.Zone.Z_heap, Universal.Zone.Z_heap]
  let export_eval = []

  let eval zpath exp man ctx flow =
    let range = erange exp in
    match ekind exp with
    (* E⟦ g(e1, e2, ...) | is_generator(g) ⟧ *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_user func)}, _)}, args, [])
      when func.py_func_is_generator = true
      ->
      bind_eval_list (Zone.Z_py, Zone.Z_py_object) args man ctx flow
      @@ fun el flow ->
      (* Create the generator instance *)
      bind_eval
        (Universal.Zone.Z_heap, Universal.Zone.Z_heap)
        (mk_alloc_instance (Addr.find_builtin "generator") ~params:(Some (Generator func)) range)
        man ctx flow
      @@ fun alloc flow ->
      begin match ekind alloc with
        | E_addr addr ->
          let obj = (addr, mk_py_empty range) in
          let flow0 = flow in
          (* Assign arguments to parameters in a new flow *)

          (* FIXME: default arguments in generators are not supported yet *)
          if List.length args <> List.length func.py_func_parameters then
            Framework.Utils.Exceptions.panic_at range "generators: only calls with correct number of arguments is supported"
          else
            (* Change all parameters into framed variables *)
            let params = List.map (fun v -> mk_framed_var v obj) func.py_func_parameters in

            (* Perform assignments to arguments *)
            let flow1 = List.fold_left (fun flow (v, e) ->
                man.exec (mk_assign (mk_var v range) e range) ctx flow
              ) flow (List.combine params args)
            in

            (* Save the projected cur env in the initial flow of the generator *)
            let cur' = man.exec (mk_project_vars params range) ctx flow1 |>
                       man.flow.get Flow.TCur
            in
            let flow2 = man.flow.add (TGenStart obj) cur' flow0 in
            Eval.singleton (Some (mk_py_object obj range)) flow2 |>
            return

        | _ -> assert false
      end

    (* E⟦ generator.__iter__(self) | isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "generator.__iter__")}, _)},
        [{ekind = E_py_object self} as arg], []
      ) when Addr.isinstance self (Addr.find_builtin "generator") ->
      Eval.singleton (Some arg) flow |>
      return

    (* E⟦ generator.__iter__(self) | ¬ isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__iter__")})},
        _,
        []
      ) ->
      let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) ctx flow in
      Eval.singleton None flow |>
      return


    (* E⟦ generator.__next__(self) | isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_py_object ({addr_kind = A_py_function (F_builtin "generator.__next__")}, _)},
        [{ekind = E_py_object self}], []
      ) when Addr.isinstance self (Addr.find_builtin "generator") ->

      let func = get_generator_function self in

      (* Keep the input cur environment *)
      let cur = man.flow.get Flow.TCur flow in

      (* Compute the next tokens *)
      let flow1 = man.flow.fold (fun tk env acc ->
          match tk with
          | Flow.TCur -> acc
          | TGenStart(g) when compare_py_object g self = 0 -> man.flow.add Flow.TCur env acc
          | TGenNext _ -> acc

          | TGenYield(g, _, r) when compare_py_object g self = 0 -> man.flow.add (TGenNext(g, r)) env acc
          | TGenYield _ -> acc

          | TGenStop(g) -> acc (* this case is handled later *)
          | Universal.Flows.Interproc.TReturn _ -> acc
          | tk -> man.flow.add tk env acc
        ) flow man.flow.bottom
      in

      (* Filter flow by location reachability *)
      (* FIXME: add relational counter *)
      let flow2 = man.flow.map (fun tk env ->
          match tk with
          | Flow.TCur -> man.env.meet env cur
          | TGenNext(g, r) -> man.env.meet env cur
          | _ -> env
        ) flow1
      in

      (* Modify the body of the generator by changing local variables into framed variables *)
      let is_local v = List.exists (fun v' -> compare_var v v' = 0) (func.py_func_locals @ func.py_func_parameters) in
      let body' = Framework.Visitor.map_stmt
          (fun expr ->
             match ekind expr with
             | E_var v when is_local v -> {expr with ekind = E_var (mk_framed_var v self)}
             | _ -> expr
          )
          (fun stmt -> stmt)
          func.py_func_body
      in
      debug "body after variable renaming:@\n @[%a@]" pp_stmt body';
      let locals' = List.map (fun v -> mk_framed_var v self) (func.py_func_locals @ func.py_func_parameters) in

      (* Execute the body statement *)
      let ctx' = Framework.Context.add KCurGenerator self ctx in
      let flow3 = man.exec body' ctx' flow2 in

      (* Add the input stop flows  *)
      let flow3 = man.flow.fold (fun tk env acc ->
          match tk with
          | TGenStop(g) when compare_py_object g self = 0 -> man.flow.add tk env acc
          | _ -> acc
        ) flow flow3
      in


      (* Restore the input flows *)
      let flow4 = man.flow.fold (fun tk env acc ->
          match tk with
          | Universal.Flows.Interproc.TReturn _ -> man.flow.add tk env acc
          | TGenStart(g) when compare_py_object g self <> 0 -> man.flow.add tk env acc
          | TGenYield(g, _, _) when compare_py_object g self <> 0 -> man.flow.add tk env acc
          | TGenStop(g) when compare_py_object g self <> 0 -> man.flow.add tk env acc
          | _ -> acc
        ) flow man.flow.bottom
      in

      (* Process the resulting yield, return and exception flows *)
      man.flow.fold (fun tk env acc ->
          match tk with
          | TGenYield(g, e, r) when compare_py_object g self = 0 ->
            (* Assign the yielded value to a temporary return variable *)
            let tmp = mktmp () in
            let flow = man.flow.set Flow.TCur env flow4 |>
                       man.exec (mk_assign (mk_var tmp range) e range) ctx
            in
            (* Clean the cur environment by removing the generator local variables *)
            let flow = List.fold_left (fun acc v ->
                man.exec (mk_remove_var v range) ctx acc
              ) flow locals'
            in
            (* Clean the yield frame by projecting on the generator local variables *)
            let cur' = man.flow.set Flow.TCur env flow4 |>
                       man.exec (mk_project_vars locals' range) ctx |>
                       man.flow.get Flow.TCur
            in
            let flow = man.flow.set (TGenYield(g, e, r)) cur' flow in
            let evl = man.eval ~zpath:(Zone.Z_py, Zone.Z_py_object) (mk_var tmp range) ctx flow |>
                      Eval.add_cleaners [mk_remove_var tmp range]
            in
            evl :: acc

          | Exceptions.TExn exn ->
            (* Save env in the token TGenStop and re-raise the exception *)
            let flow = man.flow.add (TGenStop(self)) env flow4 |>
                       man.flow.set (Exceptions.TExn exn) env
            in
            Eval.singleton None flow :: acc

          | TGenStop _
          | Universal.Flows.Interproc.TReturn _ ->
            (* Save env in the token TGenStop and raise a StopIteration exception *)
            let flow = man.flow.add (TGenStop(self)) env flow4 |>
                       man.flow.set Flow.TCur env |>
                       man.exec (Utils.mk_builtin_raise "StopIteration" range) ctx
            in
            Eval.singleton None flow :: acc

          | _ -> acc
        ) flow3 [] |>
      Eval.of_list

    (* E⟦ generator.__next__(self) | ¬ isinstance(self, generator) ⟧ *)
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "generator.__next__")})},
        _,
        []
      ) ->
      let flow = man.exec (Utils.mk_builtin_raise "TypeError" range) ctx flow in
      Eval.singleton None flow |>
      return

    (* E⟦ yield e ⟧ *)
    | E_py_yield e ->
      let g = Framework.Context.find KCurGenerator ctx in
      let flow = man.flow.fold (fun tk env acc ->
          match tk with
          | Flow.TCur -> man.flow.add (TGenYield(g, e, range)) env acc
          | TGenNext(g, r) -> man.flow.add Flow.TCur env acc
          | _ -> man.flow.add tk env acc
        ) flow man.flow.bottom
      in
      Eval.singleton None flow |>
      return

    (* E⟦ x for x in g | isinstance(g, generator) ⟧ *)
    | E_py_generator_comprehension _ ->
      Framework.Utils.Exceptions.panic_at range "Generator comprehension not supported"

    | _ -> None

  let init prog man ctx flow = None
  let exec zone stmt man ctx flow = None
  let ask query man ctx flow = None

end

let setup () =
  register_domain name (module Domain);
  (* Flow tokens *)
  let open Format in
  Flow.register_pp_token (fun next fmt -> function
      | TGenStart(gen) -> fprintf fmt "gstart(%a)" pp_py_object gen
      | TGenNext(gen, r) -> fprintf fmt "gnext(%a) -> %a" pp_py_object gen pp_range r
      | TGenYield(gen, e, r) -> fprintf fmt "gyield(%a) <- %a" pp_py_object gen pp_range r
      | TGenStop(gen) -> fprintf fmt "gstop(%a)" pp_py_object gen
      | tk -> next fmt tk
    );
  Flow.register_token_compare (fun next tk1 tk2 ->
      match tk1, tk2 with
      | TGenStart(g1), TGenStart(g2) -> compare_py_object g1 g2
      | TGenNext(g1, r1), TGenNext(g2, r2) ->
        Framework.Utils.Compare.compose [
          (fun () -> compare_py_object g1 g2);
          (fun () -> compare_range r1 r2);
        ]
      | TGenYield(g1, _, r1), TGenYield(g2, _, r2) ->
        Framework.Utils.Compare.compose [
          (fun () -> compare_py_object g1 g2);
          (fun () -> compare_range r1 r2);
        ]
      | TGenStop(g1), TGenStop(g2) -> compare_py_object g1 g2
      | _ -> next tk1 tk2
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
