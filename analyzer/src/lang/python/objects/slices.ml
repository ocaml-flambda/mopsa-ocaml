(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of slice objects. *)

open Framework.Domains.Stateless
open Framework.Domains
open Framework.Manager
open Framework.Flow
open Framework.Eval
open Framework.Ast
open Universal.Ast
open Ast
open Addr

let name = "python.objects.slices"

let mk_stop obj range = mk_py_object_attr obj "stop" ~etyp:T_int range
let mk_start obj range = mk_py_object_attr obj "start" ~etyp:T_int range
let mk_step obj range = mk_py_object_attr obj "step" range


let fold_increasing_slice_length_cases man ctx f x0 estart eend ll range flow =
  debug "increasing slice case";
  let start_cases =
    if is_none estart then [mk_zero range, mk_one range]
    else [
      (ll, mk_binop estart O_ge ll range);
      (mk_zero range, mk_binop (mk_binop ll O_plus estart ~etyp:T_int range) O_le (mk_zero range) range);
      (mk_binop ll O_plus estart ~etyp:T_int range, mk_in ~strict:true estart (mk_unop O_minus ll ~etyp:T_int range) (mk_zero range) range);
      (estart, mk_in estart (mk_zero range) ll range)
    ]
  in

  let start_cases = start_cases |>
                    List.map (fun (e, cond) -> (e, man.exec ctx (mk_assume cond range) flow)) |>
                    List.filter (fun (_, flow) -> not (man.flow.is_cur_bottom flow))
  in

  let end_cases =
    if is_none eend then [ll, mk_one range]
    else [
      (ll, mk_binop eend O_ge ll range);
      (mk_zero range, mk_binop (mk_binop ll O_plus eend ~etyp:T_int range) O_le (mk_zero range) range);
      (mk_binop ll O_plus eend ~etyp:T_int range, mk_in ~strict:true eend (mk_unop O_minus ll ~etyp:T_int range) (mk_zero range) range);
      (eend, mk_in eend (mk_zero range) ll range)
    ]
  in

  let end_cases = end_cases |>
                  List.map (fun (e, cond) -> (e, man.exec ctx (mk_assume cond range) flow)) |>
                  List.filter (fun (_, flow) -> not (man.flow.is_cur_bottom flow))
  in


  let cases =
    start_cases |> List.fold_left (fun acc (estart, start_flow) ->
        end_cases |> List.fold_left (fun acc (eend, end_flow) ->
            let flow = man.flow.meet start_flow end_flow in
            if man.flow.is_cur_bottom flow then
              acc
            else
              ((mk_binop eend O_minus estart ~etyp:T_int range), flow) :: acc
          ) acc
      ) []
  in
  List.fold_left f x0 cases

let fold_decreasing_slice_length_cases man ctx f x0 estart eend ll range flow =
  debug "decreasing slice case";
  let start_cases =
    if is_none estart then [mk_binop ll O_minus (mk_one range) range, mk_one range]
    else [
      (mk_binop ll O_minus (mk_one range) ~etyp:T_int range, mk_binop estart O_ge (mk_binop ll O_minus (mk_one range) ~etyp:T_int range) range);
      (mk_zero range, mk_binop (mk_binop ll O_plus estart ~etyp:T_int range) O_le (mk_zero range) range);
      (mk_binop ll O_plus estart ~etyp:T_int range, mk_in ~strict:true estart (mk_unop O_minus ll ~etyp:T_int range) (mk_zero range) range);
      (estart, mk_in estart (mk_zero range) (mk_binop ll O_minus (mk_one range) ~etyp:T_int range) range)
    ]
  in
  debug "start cases";
  let start_cases = start_cases |>
                    List.map (fun (e, cond) -> (e, man.exec ctx (mk_assume cond range) flow)) |>
                    List.filter (fun (_, flow) -> not (man.flow.is_cur_bottom flow))
  in

  let end_cases =
    if is_none eend then [mk_zero range, mk_one range]
    else [
      (ll, mk_binop eend O_ge (mk_binop ll O_minus (mk_one range) ~etyp:T_int range) range);
      (mk_zero range, mk_binop (mk_binop ll O_plus eend ~etyp:T_int range) O_le (mk_zero range) range);
      (mk_binop (mk_binop ll O_plus eend ~etyp:T_int range) O_plus (mk_one range) range, mk_in ~strict:true eend (mk_unop O_minus ll ~etyp:T_int range) (mk_zero range) range);
      (mk_binop eend O_plus (mk_one range) ~etyp:T_int range, mk_in eend (mk_zero range) ll range)
    ]
  in
  debug "end cases";
  let end_cases = end_cases |>
                  List.map (fun (e, cond) -> (e, man.exec ctx (mk_assume cond range) flow)) |>
                  List.filter (fun (_, flow) -> not (man.flow.is_cur_bottom flow))
  in

  debug "combining";
  let cases =
    start_cases |> List.fold_left (fun acc (estart, start_flow) ->
        end_cases |> List.fold_left (fun acc (eend, end_flow) ->
            let flow = man.flow.meet start_flow end_flow in
            if man.flow.is_cur_bottom flow then
              acc
            else
              (mk_binop (mk_binop estart O_minus eend ~etyp:T_int range) O_plus (mk_one range) ~etyp:T_int range, flow) :: acc
          ) acc
      ) []
  in
  List.fold_left f x0 cases

let fold_slice_length_cases man ctx f x0 slice ll range flow =
  eval_list [mk_start slice range; mk_stop slice range; mk_step slice range] (man.eval ctx) flow |>
  eval_fold (fun acc (el, flow, cleaners) ->
      (* FIXME: what do we do with cleaners? *)
      let start, stop, step = match el with Some [start; stop; step] -> start, stop, step | _ -> assert false in
      match type_of_object @@ object_of_expr step with
      | T_py_none ->
        fold_increasing_slice_length_cases man ctx f x0 start stop ll range flow

      | T_int ->
        let itv = man.ask ctx (Memory.Query.QInt step) flow |> Option.none_to_exn in
        if Memory.Value.I.is_constant itv then
          let a, _ = Memory.Value.I.get_bounds itv in
          if Z.(equal a one) then
            fold_increasing_slice_length_cases man ctx f x0 start stop ll range flow
          else
          if Z.(equal a (- one)) then
            fold_decreasing_slice_length_cases man ctx f x0 start stop ll range flow
          else
            Debug.fail "Unsupported slice step"
        else
          Debug.fail "Unsupported slice step"
      | _ -> Debug.fail "Unsupported slice step"
    ) x0

module Domain = struct

  let eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    (* Instantiating a slice *)
    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "slice.__new__")}, _)}, cls :: args, []) ->
      if List.length args = 0 || List.length args > 3 then
        let flow = man.exec ctx (Utils.mk_builtin_raise "TypeError" range) flow in
        oeval_singleton (None, flow, [])
      else
        let start, stop, step =
          match args with
          | [stop] -> (mk_py_none range), stop, (mk_py_none range)
          | [start; stop] -> start, stop, (mk_py_none range)
          | [start; stop; step] -> start, stop, step
          | _ -> assert false
        in
        man.eval ctx (mk_py_call (mk_py_object_attr (Addr.find_builtin "object") "__new__" range) [cls] range) flow |>
        eval_compose (fun eself flow ->
            let self = object_of_expr eself in
            let flow = man.exec ctx (mk_assign (mk_start self range) start range) flow |>
                       man.exec ctx (mk_assign (mk_stop self range) stop range) |>
                       man.exec ctx (mk_assign (mk_step self range) step range)
            in
            oeval_singleton (Some eself, flow, [])
          )

    | _ -> None


  let init _ ctx _ flow = ctx, flow

  let exec man ctx stmt flow = None

  let ask _ _ _ _ = None

end

let setup () =
  Stateless.register_domain name (module Domain)
