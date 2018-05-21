(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Abstraction of Python lists. *)

open Framework.Domains
open Framework.Ast
open Framework.Manager
open Framework.Pp
open Framework.Eval
open Framework.Domains.Stateless
open Framework.Flow
open Framework.Exceptions
open Universal.Ast
open Universal.Ast
open Utils
open Ast
open Addr
open Bot

let name = "python.objects.containers.lists"
let debug fmt = Debug.debug ~channel:name fmt
    

module Domain = struct

  let mk_ll addr range = mk_py_addr_attr addr "$ll" ~etyp:T_int range
  let mk_lv addr range = mk_py_addr_attr addr "$lv" range
      
  let set_list_length man ctx addr el range flow =
    man.exec ctx
      (mk_assign
         (mk_ll addr range)
         (mk_int (List.length el) range)
         range
      ) flow
  
  let set_list_value man ctx addr el range flow =
    match el with
    | [] ->
      man.exec ctx
        (mk_assign
           (mk_lv addr range)
           (mk_empty range)
           range
        ) flow

    | hd :: tl ->
      let stmt e = mk_assign (mk_lv addr range) e range in
      tl |> List.fold_left (fun acc e ->
          man.exec ctx (stmt e) flow |>
          man.flow.join acc
        ) (man.exec ctx (stmt hd) flow)

  let create_post_ignore_stmtl vars range =
    vars |> List.map (fun v -> mk_remove_var v range)

  let mk_assign_next_loop iter assign_stmt range =
    Utils.mk_try_stopiteration
      (mk_while
         (mk_true range)
         (assign_stmt (Builtins.mk_builtin_call "next" [mk_addr iter range] range))
         range
      )
      (mk_block [] range)
      range
      
  let mk_iter_counter addr range = mk_py_addr_attr addr "$counter" ~etyp:T_int range

  let add_element man ctx addr e range flow =
    let flow = man.exec ctx
        (mk_assign 
           (mk_ll addr range)
           (mk_binop (mk_ll addr range) math_plus (mk_one range) range)
           range
        ) flow
    in

    let empty_list_flow =
      man.exec ctx
        (mk_assume
           (mk_binop (mk_ll addr range) O_eq (mk_one range) range)
           range
        ) flow
      in
    
      let nonempty_list_flow =
        man.exec ctx
          (mk_assume
             (mk_binop (mk_ll addr range) O_gt (mk_one range) range)
             range
          ) flow
      in

      debug "empty_list_flow = @\n@[  %a@]@\n nonempty_list_flow = @\n@[  %a@]"
        man.flow.print empty_list_flow
        man.flow.print nonempty_list_flow
      ;
        
      man.flow.join (
        debug "add1";
        let flow' = man.exec ctx
          (mk_assign (mk_lv addr range) e range ~mode:WEAK)
          nonempty_list_flow
        in
        debug "add1 flow = @\n@[  %a@]" man.flow.print flow';
        flow'
      ) ( 
        let flow' = man.exec ctx
          (mk_assign (mk_lv addr range) e range)
          empty_list_flow in
        debug "add2 flow = @\n@[  %a@]" man.flow.print flow';
        flow'
      ) 

  let check_index_access man ctx alist index range flow =
    let ll = mk_ll alist range in
    let cond_safe = mk_binop
        (mk_binop index O_ge (mk_neg ll range) range)
        O_log_and
        (mk_binop index O_lt ll range)
        range
    in
    let flow_safe = man.exec ctx
        (mk_assume cond_safe range) flow
    in
    let flow_fail = man.exec ctx
      (mk_assume (mk_not cond_safe range) range) flow
    in
    debug "index access:@ list: %a@ index: %a@, flow: @[%a@]@, safe case:@ @[%a@]@ fail case:@ @[%a@]"
      Universal.Pp.pp_addr alist
      Framework.Pp.pp_expr index
      man.flow.print flow
      man.flow.print flow_safe
      man.flow.print flow_fail
    ;
    flow_safe, flow_fail


  let create_list man ctx el range flow =
    eval_alloc_instance man ctx (Addr.from_string "list") None range flow |>
    oeval_compose (fun addr flow ->
        let flow = set_list_value man ctx addr el range flow |>
                   set_list_length man ctx addr el range
        in
        oeval_singleton (Some addr, flow, [])
      )
 
  let rec eval man ctx exp flow =
    let range = exp.erange in
    match ekind exp with
    | E_py_list (el) ->
      create_list man ctx el range flow  |>
      oeval_compose (fun addr flow -> oeval_singleton (Some (mk_addr addr range), flow, []))
      
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("list.__iter__"))})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as alist)}) :: [],
        []
      ) ->
      eval_alloc_instance man ctx (Addr.from_string "listiter") (Some (List alist)) range flow |>
      oeval_compose (fun aiter flow ->
          let flow = man.exec ctx (mk_assign (mk_iter_counter aiter range) (mk_zero range) range) flow in
          oeval_singleton (Some (mk_addr aiter range), flow, [])
        )
        
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin("listiter.__next__"))})},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "listiter", _)}, Some (List alist))}  as aiter)}],
        []
      ) ->
      let ll = mk_ll alist range in
      let lv = mk_lv alist range in
      let counter = mk_iter_counter aiter range in

      let in_cond = mk_binop counter O_lt ll range in
      let in_flow = man.exec ctx (mk_assume in_cond range) flow in
      let in_case =
        if man.flow.is_cur_bottom in_flow then
          None
        else
          let tmp = mktmp () in
          let flow =
            man.exec ctx (mk_assign (mk_var tmp range) lv ~mode:EXPAND range) flow |>
            man.exec ctx (mk_assign counter (mk_binop counter math_plus (mk_one range) range) range)
          in
          re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow, [mk_remove_var tmp range])
            
      in

      let out_cond = mk_binop counter O_eq ll range in
      let out_flow = man.exec ctx (mk_assume out_cond range) flow in
      let out_case =
        if man.flow.is_cur_bottom out_flow then
          None
        else
          let flow = man.exec ctx (Builtins.mk_builtin_raise "StopIteration" range) out_flow in
          oeval_singleton (None, flow, [])
      in

      oeval_join in_case out_case

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin("list.__contains__"))}},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as list)}; value],
        []
      ) ->
      begin
        let ll = mk_ll list range and lv = mk_lv list range in

        (* x in l => ll >= 1 && lv == x *)
        let can_be_true =
          let flow' = man.exec ctx (mk_assume (mk_binop ll O_ge (mk_one range) range) range) flow in
          not @@ man.flow.is_cur_bottom flow' &&
          not @@ man.flow.is_cur_bottom @@ man.exec ctx (mk_assume (mk_binop lv O_eq value range) range) flow'
        in

        debug "can be true = %b" can_be_true;

        debug "check false";

        (* x not in l => ll == 0 || lv != x *)
        let can_be_false =
          not @@ man.flow.is_cur_bottom @@ man.exec ctx (mk_assume (mk_binop ll O_eq (mk_zero range) range) range) flow ||
          not @@ man.flow.is_cur_bottom @@ man.exec ctx (mk_assume (mk_binop lv O_ne value range) range) flow
        in

        debug "can be false = %b" can_be_false;

        match can_be_true, can_be_false with
        | true, false -> oeval_singleton (Some (mk_true range), flow, [])
        | false, true -> oeval_singleton (Some (mk_false range), flow, [])
        | true, true -> oeval_join
                          (oeval_singleton (Some (mk_true range), flow, []))
                          (oeval_singleton (Some (mk_false range), flow, []))
        | false, false -> oeval_singleton (None, flow, [])

      end  
      
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__getitem__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as alist)};
          index
        ],
        []
      ) ->
      let flow_safe, flow_fail = check_index_access man ctx alist index range flow in

      let safe_case =
        if man.flow.is_cur_bottom flow_safe then
          None
        else
          let lv = mk_lv alist range in
          let tmp = mktmp () in
          let flow_safe = man.exec ctx (mk_assign (mk_var tmp range) lv ~mode:EXPAND range) flow_safe in
          re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow_safe, [mk_remove_var tmp range])
      in

      let fail_case = 
        if man.flow.is_cur_bottom flow_fail then
          None
        else
          let flow_fail =  man.exec ctx (Builtins.mk_builtin_raise "IndexError" range) flow_fail in
          oeval_singleton (None, flow_fail, [])
      in

      oeval_join safe_case fail_case

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__setitem__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as alist)};
          index;
          exp'
        ],
        []
      ) ->


      let safe_flow, fail_flow = check_index_access man ctx alist index range flow in

      let safe_case =
        if man.flow.is_cur_bottom safe_flow then
          man.flow.bottom
        else
          let ll = mk_ll alist range in
          let lv = mk_lv alist range in

          let singleton_cond = mk_binop ll O_eq (mk_one range) range in
          let singleton_flow = man.exec ctx (mk_assume singleton_cond range) safe_flow in
          let singleton_flow =
            if man.flow.is_cur_bottom singleton_flow then
              man.flow.bottom
            else
              man.exec ctx (mk_assign lv exp' range) singleton_flow
          in

          let otherwise_cond = mk_binop ll O_ge (mk_int 2 range) range in
          let otherwise_flow = man.exec ctx (mk_assume otherwise_cond range) safe_flow in
          let otherwise_flow =
            if man.flow.is_cur_bottom otherwise_flow then
              man.flow.bottom
            else
              man.exec ctx (mk_assign lv exp' range ~mode:WEAK) otherwise_flow
          in

          man.flow.join singleton_flow otherwise_flow
      in

      let fail_case = 
        if man.flow.is_cur_bottom fail_flow then
          man.flow.bottom
        else
          man.exec ctx (Builtins.mk_builtin_raise "IndexError" range) fail_flow
      in

      let flow = man.flow.join safe_case fail_case in
      oeval_singleton (Some (mk_py_none range), flow, [])
               
    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin("list.__len__"))}},
        [{ekind = E_addr ({addr_kind = A_py_instance(({addr_kind = A_py_class (C_builtin "list", _)}), _)} as alist)}],
        []
      ) ->
      let exp' = mk_ll alist range in
      re_eval_singleton (man.eval ctx) (Some exp', flow, [])

    | E_py_call(
        {ekind = E_addr {addr_kind = A_py_function (F_builtin "list.__init__")}},
        [
          {ekind = E_addr alist} as elist;
          arg
        ],
        []
      )
      ->
      let ll = mk_ll alist range in
      let lv = mk_lv alist range in

      let flow = man.exec ctx (mk_assign ll (mk_zero range) range) flow |>
                 man.exec ctx (mk_assign lv (mk_empty range) range)
      in

      man.eval ctx (Builtins.mk_builtin_call "iter" [arg] range) flow |>
      eval_compose (fun iter flow ->
          match ekind iter with
          | E_addr iter ->
            let flow = man.exec ctx
                (mk_assign_next_loop
                  iter
                   (fun value ->
                      mk_stmt (S_expression (Builtins.mk_builtin_call "list.append" [elist; value] range)) range
                   )
                   range
                ) flow
            in

            oeval_singleton (Some (mk_py_none range), flow, [])
              
          | _ -> assert false
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.append")})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)}  as alist)}) :: [e],
        []
      )
      ->
      let flow = add_element man ctx alist e range flow in
      oeval_singleton (Some (mk_py_none range), flow, [])

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.insert")})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)}  as alist)}) :: args,
        []
      )
      ->
      begin
        match args with
        | [idx; e] ->
          (* FIXME: check that idx has an integer type *)
          let flow = add_element man ctx alist e range flow in
          oeval_singleton (Some (mk_py_none range), flow, [])

        | _ ->
          let flow = man.exec ctx (Builtins.mk_builtin_raise "TypeError" range) flow in
          oeval_singleton (None, flow, [])
      end
      
    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.pop")})},
        ({ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)}  as alist)}) :: args,
        []
      )
      ->
      begin
        match args with
        | [] | [_] ->
          begin
            let idx =
              match args with
              | [] -> mk_zero range
              | [x] -> x
              | _ -> assert false
            in

            let ll = mk_ll alist range in
            let lv = mk_lv alist range in

            let ok_cond =
              mk_binop
                (mk_binop idx O_ge (mk_unop math_minus ll range) range)
                O_log_and
                (mk_binop idx O_lt ll range)
                range
            in
            let ok_flow = man.exec ctx (mk_assume ok_cond range) flow in
            let ok_case =
              if man.flow.is_cur_bottom ok_flow then
                None
              else
                let flow = man.exec ctx (mk_assign ll (mk_binop ll math_minus (mk_one range) range) range) ok_flow in
                let tmp = mktmp () in
                let flow = man.exec ctx (mk_assign (mk_var tmp range) lv ~mode:EXPAND range) flow in
                re_eval_singleton (man.eval ctx) (Some (mk_var tmp range), flow, [mk_remove_var tmp range])
            in

            let error_cond = mk_not ok_cond range in
            let error_flow = man.exec ctx (mk_assume error_cond range) flow in
            let error_case =
              if man.flow.is_cur_bottom error_flow then
                None
              else
                let flow =  man.exec ctx (Builtins.mk_builtin_raise "IndexError" range) error_flow in
                oeval_singleton (None, flow, [])
            in

            oeval_join ok_case error_case
            
          end

        | _ ->
          let flow = man.exec ctx (Builtins.mk_builtin_raise "TypeError" range) flow in
          oeval_singleton (None, flow, [])
      end


    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__add__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as l1)};
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as l2)}
        ],
        []
      )
      ->
      let ll1 = mk_ll l1 range in
      let lv1 = mk_lv l1 range in

      let ll2 = mk_ll l2 range in
      let lv2 = mk_lv l2 range in

      eval_alloc_instance man ctx (Addr.from_string "list") None range flow |>
      oeval_compose (fun addr flow ->
          let ll = mk_ll addr range in
          let lv = mk_lv addr range in

          let flow = man.exec ctx (mk_assign ll (mk_binop ll1 math_plus ll2 range) range) flow in
          (* FIXME: improve precision by checing if l1 or l2 are empty *)
          let flow1 = man.exec ctx (mk_assign lv lv1 ~mode:EXPAND range) flow in
          let flow2 = man.exec ctx (mk_assign lv lv2 ~mode:EXPAND range) flow in
          let flow = man.flow.join flow1 flow2 in

          oeval_singleton (Some (mk_addr addr range), flow, [])
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__iadd__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as l1)};
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as l2)}
        ],
        []
      )
      ->
      let ll1 = mk_ll l1 range in
      let lv1 = mk_lv l1 range in

      let ll2 = mk_ll l2 range in
      let lv2 = mk_lv l2 range in

      let flow = man.exec ctx (mk_assign ll1 (mk_binop ll1 math_plus ll2 range) range) flow in
      (* FIXME: improve precision by checing if l1 or l2 are empty *)
      let flow1 = man.exec ctx (mk_assign lv1 lv2 ~mode:EXPAND range) flow in
      let flow = man.flow.join flow1 flow in

      oeval_singleton (Some (mk_addr l1 range), flow, [])

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__mul__")})},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)}  as alist)};
         {etyp = T_int} as arg],
        []
      ) ->
      let ll = mk_ll alist range in
      let lv = mk_lv alist range in

      eval_alloc_instance man ctx (Addr.from_string "list") None range flow |>
      oeval_compose (fun addr' flow ->
          let ll' = mk_ll addr' range in
          let lv' = mk_lv addr' range in

          let flow = man.exec ctx (mk_assign ll' (mk_binop ll math_mult arg range) range) flow in
          let flow = man.exec ctx (mk_assign lv' lv ~mode:EXPAND range) flow in
          
          oeval_singleton (Some (mk_addr addr' range), flow, [])
        )

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__imul__")})},
        [{ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)}  as alist)};
         {etyp = T_int} as arg],
        []
      )

      ->

      let ll = mk_ll alist range in

      let flow = man.exec ctx (mk_assign ll (mk_binop ll math_mult arg range) range) flow in
      
      oeval_singleton (Some (mk_addr alist range), flow, [])

    | E_py_list_comprehension(e, comprhs) ->
      create_list man ctx [] range flow |>
      oeval_compose (fun addr flow ->
          (* Iterate over comprehensions of the form (target, iter, conds) *)
          let rec aux = function
            | [] ->
              mk_stmt (S_expression (Builtins.mk_builtin_call "list.append" [mk_addr addr range; e] range)) range

            | (target, iter, []) :: tl ->
              mk_stmt (S_py_for (
                  target,
                  iter,
                  aux tl,
                  mk_block [] range
                ))
                range

            | (target, iter, [cond]) :: tl ->
              mk_stmt (S_py_for (
                  target,
                  iter,
                  mk_if cond (aux tl) (mk_block [] range) range,
                  mk_block [] range
                ))
                range

            | _ -> assert false
            
          in

          let stmt = aux comprhs in
          let flow = man.exec ctx stmt flow in

          oeval_singleton (Some (mk_addr addr range), flow, [])
        )


    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__eq__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as l1)};
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)} as l2)}
        ],
        []
      )
      ->
      begin
        let ll1 = mk_ll l1 range and ll2 = mk_ll l2 range in
        let lv1 = mk_lv l1 range and lv2 = mk_lv l2 range in

        let l1_empty_flow = man.exec ctx (mk_assume (mk_binop ll1 O_eq (mk_zero range) range) range) flow in
        let l2_empty_flow = man.exec ctx (mk_assume (mk_binop ll2 O_eq (mk_zero range) range) range) flow in

        let can_be_true, can_be_false =
          match man.flow.is_cur_bottom l1_empty_flow, man.flow.is_cur_bottom l2_empty_flow with
          | false, false -> true, false
          | false, true -> false, true
          | true, false -> false, true
          | true, true -> true, true
        in
        
        debug "check true";

        (* (l1 == l2) => (ll1 == ll2 == 0) || ( (ll1 == ll2 >= 1) && (lv1 == lv2) ) *)
        let can_be_true =
          can_be_true ||
          (
            let flow = man.exec ctx (mk_assume (mk_binop ll1 O_ge (mk_one range) range) range) flow |>
                       man.exec ctx (mk_assume (mk_binop ll2 O_ge (mk_one range) range) range) |>
                       man.exec ctx (mk_assume (mk_binop ll1 O_eq ll2 range) range)
            in
            not @@ man.flow.is_cur_bottom flow &&
            not @@ man.flow.is_cur_bottom @@ man.exec ctx (mk_assume (mk_binop lv1 O_eq lv2 range) range) flow
          )
        in

        debug "can be true = %b" can_be_true;

        debug "check false";

        (* (l1 != l2) => (ll1 == 0 xor ll2 == 0) || ( ll1 >= 1 && ll2 >= 1 && (ll1 != ll2 || lv1 != lv2) ) *)
        let can_be_false =
          can_be_false || (
            let flow = man.exec ctx (mk_assume (mk_binop ll1 O_ge (mk_one range) range) range) flow |>
                       man.exec ctx (mk_assume (mk_binop ll2 O_ge (mk_one range) range) range)
            in
            not @@ man.flow.is_cur_bottom @@ man.exec ctx (mk_assume (mk_binop ll1 O_ne ll2 range) range) flow ||
            not @@ man.flow.is_cur_bottom @@ man.exec ctx (mk_assume (mk_binop lv1 O_ne lv2 range) range) flow
          )
        in

        debug "can be false = %b" can_be_false;

        match can_be_true, can_be_false with
        | true, false -> oeval_singleton (Some (mk_true range), flow, [])
        | false, true -> oeval_singleton (Some (mk_false range), flow, [])
        | true, true -> oeval_join
                          (oeval_singleton (Some (mk_true range), flow, []))
                          (oeval_singleton (Some (mk_false range), flow, []))
        | false, false -> oeval_singleton (None, flow, [])
      end

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__eq__")})},
        [
          {ekind = E_addr ({addr_kind = A_py_instance({addr_kind = A_py_class (C_builtin "list", _)}, _)})};
          _
        ],
        []
      )
      ->
      oeval_singleton (Some (mk_false range), flow, [])

    | E_py_call(
        {ekind = E_addr ({addr_kind = A_py_function (F_builtin "list.__ne__")})},
        [e1; e2],
        []
      )
      ->
      let exp' = Builtins.mk_builtin_call "list.__eq__" [e1; e2] range in
      eval man ctx exp' flow |>
      oeval_compose (fun res flow ->
          match ekind res with
          | E_constant (C_true) -> oeval_singleton (Some (mk_false range), flow, [])
          | E_constant (C_false) -> oeval_singleton (Some (mk_true range), flow, [])
          | _ -> assert false
        )


    | E_py_call({ekind = E_addr ({addr_kind = A_py_function (F_builtin f)})}, _, _)
        when Addr.is_builtin_class_function "list" f ->
        panic "List function %s not implemented" f

    | _ -> None

  let init man ctx prog flow = ctx, flow
  
  let exec man ctx stmt flow = None

  let ask man ctx query flow = None

end

let setup () =
  register_domain name (module Domain)
