(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python dictionaries. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Domains.Stateful
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr
open Bot

let name = "python.objects.containers.dicts"
let debug fmt = Debug.debug ~channel:name fmt


module V = Memory.Value


(*==========================================================================*)
(**                           {2 Key Abstraction}                           *)
(*==========================================================================*)

(** An abstract key maintains a lower and upper approximation of the values
    of the concrete keys of the dict.
*)
module K =
struct

  type v = {
    inf: V.t;
    sup: V.t;
  }

  type t = v with_bot

  let bottom = BOT

  let top = Nb {
      inf = V.bottom;
      sup = V.top;
    }

  let empty = Nb {
      inf = V.bottom;
      sup = V.bottom;
    }

  let init = empty

  let leq abs1 abs2 =
    bot_included (fun abs1 abs2 ->
        V.leq abs2.inf abs1.inf && V.leq abs1.sup abs2.sup
      ) abs1 abs2

  let join abs1 abs2 =
    bot_neutral2 (fun abs1 abs2 ->
        {
          inf = V.meet abs1.inf abs2.inf;
          sup = V.join abs1.sup abs2.sup;
        }
      ) abs1 abs2

  let meet abs1 abs2 =
    bot_absorb2 (fun abs1 abs2 ->
        let abs = {
          inf = V.join abs1.inf abs2.inf;
          sup = V.meet abs1.sup abs2.sup;
        } in
        if V.leq abs.inf abs.sup then
          Nb abs
        else
          BOT
      ) abs1 abs2

  let widening ctx abs1 abs2 =
    bot_neutral2 (fun abs1 abs2 ->
        {
          inf = V.meet abs2.inf abs1.inf;
          sup = V.widening ctx abs1.sup abs2.sup;
        }
      ) abs1 abs2

  let print fmt abs =
    bot_fprint (fun fmt abs ->
        Format.fprintf fmt "{| @[%a@] , @[%a@] |}"
          V.print abs.inf
          V.print abs.sup
      ) fmt abs

  let is_bottom abs = (abs = BOT)

  let is_top abs =
    bot_dfl1 false (fun abs -> V.is_bottom abs.inf && V.is_top abs.sup) abs

  let may_add v abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = V.meet v abs.inf;
          sup = V.join v abs.sup;
        }
      ) abs

  let must_add v abs =
    bot_absorb1 (fun abs ->
        Nb {
          inf = V.join v abs.inf;
          sup = V.join v abs.sup;
        }
      ) abs


  let remove v abs =
    (** FIXME: removing an abstract value from another requires a complement operator,
        which is missing for numeric abstractions for example.
        So we just return the abstraction not modifed for the moment !
    *)
    abs


  let may_mem v abs =
    bot_dfl1 false (fun abs -> not @@ V.is_bottom @@ V.meet v abs.sup) abs

  let must_mem v abs =
    bot_dfl1 false (fun abs -> not @@ V.is_bottom @@ V.meet v abs.inf) abs

  let singleton v =
    Nb {inf = v; sup = v}
end



(*==========================================================================*)
(**                           {2 Abstract Domain}                           *)
(*==========================================================================*)

(* This abstract domain maintains for each reachable address @ of type dict:
   - an abstract key representing an under and upper approximation of concrete
   keys values.
   - an abstract value @.$dv representing an over approximation of the concrete entries
values.
   - an abstract length @.$dl approximating the number of entries.
*)

module Domain =
struct

  include Framework.Lattices.Partial_map.Make
      (struct type t = addr let compare = compare_addr let print = Universal.Pp.pp_addr end)
      (K)

  let is_bottom _ = false
  let print fmt a =
      Format.fprintf fmt "dicts keys:@ @[<h>  %a@]@\n" print a

  let mk_dl addr = mk_py_addr_attr ~etyp:T_int addr "$dl"
  let mk_dv addr = mk_py_addr_attr  addr "$dv"
  let mk_iter_counter addr = mk_py_addr_attr ~etyp:T_int  addr "$counter"

  let eval_key man ctx k flow =
    match ekind k with
    | E_constant c -> Memory.Value.of_constant c
    | _ ->
      man.ask ctx (Memory.Nonrel.Domain.QEval k) flow |> Option.none_to_exn

  let eval man ctx exp flow =
    let range = exp.erange in

    match ekind exp with
    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin "dict.__init__")})}, _, [])  ->
      panic "dict() not supported"

    | E_py_dict (keys, values) ->
      eval_alloc_instance man ctx (Addr.find_builtin "dict") None range flow |>
      oeval_compose (fun addr flow ->
          let dl = mk_dl addr range and dv = mk_dv addr range in
          eval_list (keys @ values) (man.eval ctx) flow |>
          eval_compose (fun el flow ->
            let keys, values = partition_list_by_length (List.length keys) el in
            match keys, values with
            | [], [] ->
              let flow = map_domain_cur (add addr K.empty) man flow  |>
                         man.exec ctx (mk_assign dl (mk_zero range) range) |>
                         man.exec ctx (mk_assign dv (mk_empty range) range)
              in
              oeval_singleton (Some (mk_addr addr range), flow, [])

            | _ ->
              let keyset =
                keys |>
                List.fold_left (fun acc k ->
                    let kv = eval_key man ctx k flow in
                    K.must_add kv acc
                  ) K.empty
              in

              (** FIXME: what to do if the addr is weak?! *)
              let flow = map_domain_cur (add addr keyset) man flow in

              let flow =
                values |>
                List.fold_left (fun acc v ->
                    man.exec ctx (mk_assign dv v range) flow |>
                    man.flow.join acc
                  ) man.flow.bottom
              in
              (* FIXME: this is incorrect in the case when PyDict contains duplicate keys *)
              let flow = man.exec ctx (mk_assign dl (mk_int (List.length keys) range) range) flow in
              oeval_singleton (Some (mk_addr addr range), flow, [])
            )
        )

    | E_py_dict_comprehension(k, v, comprhs) ->
      panic "dict comprehension not supported"


    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "dict.__getitem__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict", _)}, _)} as addr)};
          key
        ],
        []
      )  ->
      begin
        let dv = mk_dv addr range in

        let tmp = mktmp () in
        let ok_exp = mk_var tmp range in

        let ok_stmt = mk_assign (mk_var tmp range) dv range ~mode:EXPAND in
        let clean_stmt = mk_remove_var tmp range in

        let kv = eval_key man ctx key flow in
        let keyset = get_domain_cur man flow |> find addr in

        match K.must_mem kv keyset, K.may_mem kv keyset with
        | true, _ ->
          let flow = man.exec ctx ok_stmt flow in
          re_eval_singleton (man.eval ctx) (Some ok_exp, flow, [clean_stmt])

        | false, true ->
          let ok_flow =
            let keyset' = K.must_add kv keyset in
            map_domain_cur (add addr keyset') man flow |>
            man.exec ctx ok_stmt
          in

          let error_flow =
            let keyset' = K.remove kv keyset in
            map_domain_cur (add addr keyset') man flow |>
            man.exec ctx (Utils.mk_builtin_raise "KeyError" range)
          in
          oeval_join
            (re_eval_singleton (man.eval ctx) (Some ok_exp, ok_flow, [clean_stmt]))
            (oeval_singleton (None, error_flow, []))

        | false, false -> assert false

      end


    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "dict.__setitem__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict", _)}, _)} as dict)};
          key;
          value
        ],
        []
      ) ->

      let dv = mk_dv dict range in
      let dl = mk_dl dict range in
      let kv = eval_key man ctx key flow in
      let keyset = get_domain_cur man flow |> find dict in

      let empty_case =
        let flow = man.exec ctx (mk_assume (mk_binop dl O_eq (mk_zero range) range) range) flow in
        if man.flow.is_cur_bottom flow then
          None
        else
          let flow = man.exec ctx (mk_assign dl (mk_one range) range) flow |>
                     man.exec ctx (mk_assign dv value range) |>
                     map_domain_cur (add dict (K.singleton kv)) man
          in
          oeval_singleton (Some (mk_py_none range), flow, [])
      in

      let non_empty_case =
        let flow = man.exec ctx (mk_assume (mk_binop dl O_ge (mk_one range) range) range) flow in
        if man.flow.is_cur_bottom flow then
          None
        else
          let flow = man.exec ctx (mk_assign dv value range ~mode:WEAK) flow |>
                     map_domain_cur (add dict (K.may_add kv keyset)) man
          in
          let flow =
            match K.must_mem kv keyset, K.may_mem kv keyset with
            | true, _ ->
              flow
            | false, true ->
              man.exec ctx (mk_assign dl (mk_binop dl math_plus (mk_one range) range) range ~mode:WEAK) flow
            | false, false ->
              man.exec ctx (mk_assign dl (mk_binop dl math_plus (mk_one range) range) range) flow
          in
          oeval_singleton (Some (mk_py_none range), flow, [])
      in

      oeval_join empty_case non_empty_case


    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("dict.__values__"))})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict", _)}, _)}  as addr)}) :: [],
        []
      ) ->
      eval_alloc_instance man ctx (Addr.find_builtin "dict_values") (Some (Dict addr)) range flow |>
      oeval_compose (fun addr flow ->
          oeval_singleton (Some (mk_addr addr range), flow, [])
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("dict_value.__iter__"))})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict_values", _)}, Some (Dict dict))})}) :: [],
        []
      ) ->
      eval_alloc_instance man ctx (Addr.find_builtin "dict_valueiterator") (Some (Dict dict)) range flow |>
      oeval_compose (fun iter flow ->
          let flow = man.exec ctx (mk_assign (mk_iter_counter iter range) (mk_zero range) range) flow in
          oeval_singleton (Some (mk_addr iter range), flow, [])
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("dict_valueiterator.__next__"))})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict_valueiterator", _)}, Some (Dict dict))}  as iter)}) :: [],
        []
      ) ->
      let dl = mk_dl dict range in
      let dv = mk_dv dict range in
      let counter = mk_iter_counter iter range in

      let in_cond = mk_binop counter O_lt dl range in
      let in_flow = man.exec ctx (mk_assume in_cond range) flow in
      let in_case =
        if man.flow.is_cur_bottom in_flow then
          None
        else
          let tmp = mktmp () in
          let flow =
            man.exec ctx (mk_assign (mk_var tmp range) dv range ~mode:EXPAND) flow |>
            man.exec ctx (mk_assign counter (mk_binop counter math_plus (mk_one range) range) range)
          in
          re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow, [mk_remove_var tmp range])

      in

      let out_cond = mk_binop counter O_eq dl range in
      let out_flow = man.exec ctx (mk_assume out_cond range) flow in
      let out_case =
        if man.flow.is_cur_bottom out_flow then
          None
        else
          let flow = man.exec ctx (Utils.mk_builtin_raise "StopIteration" range) out_flow in
          oeval_singleton (None, flow, [])
      in

      oeval_join in_case out_case

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "dict.keys")})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict", _)}, _)})}) :: [],
        []
      ) ->
      panic "dict.keys not supported"


    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin("dict.__len__"))}},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "dict", _)}, _)})}],
        []
      ) ->
      panic "dict.__len__ not supported"


    | _ -> None

  let exec man ctx stmt flow =
    match skind stmt with
    | S_rebase_addr(a1, a2, mode) ->
      let abs = get_domain_cur man flow in
      let keyset1 = find a1 abs and keyset2 = find a2 abs in
      let keyset = K.join keyset1 keyset2 in
      let abs' = remove a1 abs |> add a2 keyset in
      let abs'' = match mode with
        | STRONG | EXPAND -> abs'
        | WEAK -> join abs abs'
      in
      let flow = set_domain_cur abs'' man flow in
      return flow

    | _ -> None


  let init man ctx prog flow =
    ctx, set_domain_cur empty man flow

  let ask _ _ _ _ = None


end

let setup () =
  register_domain name (module Domain)
