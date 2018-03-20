open Framework.Domain
open Framework.Ast
open Framework.Manager
open Universal.Ast
open Universal.Ast
open Ast
open Addr
open XAst
open Utils
    
let name = "python.objects.dicts"

module Make(SubLayer: Framework.Layer.S) =
struct

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt

  module Abstract =
  struct
    include Universal.Utils.AddrPartialMap(KeySet)
    let is_bottom _ = false
    let print fmt a =
      Format.fprintf fmt "dicts keys:@ @[<h>  %a@]" print a
  end
  module Flow = Framework.Lattice.EmptyLattice

  let has_abstract = true
  let has_flow = false
  
  type t = Abstract.t * Flow.t
  type st = SubLayer.t

  module SubAbstract = SubLayer.Abstract

  let mk_dl addr = mk_exp ~etyp:TInt (PyAttribute(mk_addr addr, "$dl"))
  let mk_dv addr = mk_exp (PyAttribute(mk_addr addr, "$dv"))
  let mk_iter_counter addr = mk_exp ~etyp:TInt (PyAttribute(mk_addr addr, "$counter"))

  let eval_key k subax gabs =
    match ekind k with
    | Constant c -> Memory.Value.of_constant c
    | _ ->
      SubLayer.ask (Memory.NonRelational.NR.QEval k) (subax.get gabs) |>
      Framework.Query.flatten |>
      List.map fst |>
      List.fold_left Memory.Value.join Memory.Value.bottom

  let eval (exp : exp) manager ctx ax_ subax_ gabs =
    let ax = mk_domain_ax ax_ and subax = mk_domain_ax subax_ in
    match ekind exp with
     | PyCall(
        {ekind = Constant (Addr ({akind = B_function "dict.__init__"}))},
        _,
        []
      )  ->
       panic "dict() not supported"


   
    | PyDict (keys, values) ->
      let addr, gabs =
        Addr.alloc_instance (Builtins.builtin_address "dict") exp.erange manager ctx gabs
      in
      let dl = mk_dl addr and dv = mk_dv addr in
      begin
        let gabs = eval_exec_list (fun el gabs ->
            let keys, values = partition_list_by_length (List.length keys) el in
            match keys, values with
            | [], [] ->
              let abs = ax.get_abs gabs in
              let abs = Abstract.add addr KeySet.empty abs in
              ax.set_abs gabs abs |>
              manager.exec (mk_assign dl mk_zero) ctx |>
              manager.exec (mk_assign dv (mk_exp (Constant PyEmptyValue))) ctx

            | _ ->
              let keyset =
                keys |>
                List.fold_left (fun acc k ->
                    let kv = eval_key k subax gabs in
                    KeySet.must_add kv acc
                  ) KeySet.empty
              in

              let abs = ax.get_abs gabs in
              (** FIXME: what to do if the addr is weak?! *)
              let abs = Abstract.add addr keyset abs in
              let gabs = ax.set_abs gabs abs in

              let gabs =
                values |>
                List.fold_left (fun acc v ->
                    manager.exec (mk_assign dv v) ctx gabs |>
                    manager.join acc
                  ) manager.bottom
              in
              (* FIXME: this is incorrect in the case when PyDict contains duplicate keys *)
              manager.exec (mk_assign dl (mk_int (List.length keys))) ctx gabs
          ) (keys @ values) manager ctx gabs 
        in
        let exp' = mk_addr ~erange:exp.erange addr in
        [(exp', (gabs, []))]
      end


    | PyDictComprehension(k, v, comprhs) ->
      panic "dict comprehension not supported"


    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "dict.__getitem__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict"}, _)} as addr))};
          key
        ],
        []
      )  ->
      begin
        let dv = mk_dv addr in

        let tmp = mktmp () in
        let ok_exp = mk_var ~erange:exp.erange tmp in
        let ok_stmt = mk_stmt (Expand(mk_var tmp, dv)) in

        let error_stmt = mk_stmt (PyRaise (Some (mk_exp ~erange:exp.erange (Constant (Addr (Builtins.builtin_address "KeyError")))))) in

        let clean_stmt = mk_stmt (RemoveAll tmp) in

        let kv = eval_key key subax gabs in
        let keyset = Abstract.find addr @@ ax.get_abs gabs in

        match KeySet.must_mem kv keyset, KeySet.may_mem kv keyset with
        | true, _ ->
          let gabs = manager.exec ok_stmt ctx gabs in
          [ok_exp, (gabs, [clean_stmt])]

        | false, true ->
          let ok_gabs =
            KeySet.must_add kv keyset |>
            Abstract.add addr |>
            (fun add -> add (ax.get_abs gabs)) |>
            (fun abs -> ax.set_abs gabs abs) |>
            manager.exec ok_stmt ctx
          in

          let error_gabs =
            KeySet.remove kv keyset |>
            Abstract.add addr |>
            (fun add -> add (ax.get_abs gabs)) |>
            (fun abs -> ax.set_abs gabs abs) |>
            manager.exec error_stmt ctx
          in

          [(ok_exp, (ok_gabs, [clean_stmt])); (exp, (error_gabs, []))]

        | false, false -> assert false

      end


    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "dict.__setitem__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict"}, _)} as dict))};
          key;
          value
        ],
        []
      ) ->

      let dv = mk_dv dict in
      let dl = mk_dl dict in
      let kv = eval_key key subax gabs in
      let keyset = Abstract.find dict @@ ax.get_abs gabs in


      let empty_case =
        let gabs = manager.exec (mk_stmt (Assume (mk_binop dl Eq mk_zero))) ctx gabs in
        if SubAbstract.is_bottom @@ subax.get_abs gabs then
          []
        else
          let gabs = manager.exec (mk_assign dl mk_one) ctx gabs |>
                     manager.exec (mk_assign dv value) ctx |>
                     (fun gabs ->
                        ax.set_abs gabs @@ Abstract.add dict (KeySet.singleton kv) @@ ax.get_abs gabs
                     )
          in
          [mk_constant PyNone ~etyp:TNone, (gabs, [])]
      in

      let non_empty_case =
        let gabs = manager.exec (mk_stmt (Assume (mk_binop dl Ge mk_one))) ctx gabs in
        if SubAbstract.is_bottom @@ subax.get_abs gabs then
          []
        else
          let gabs = manager.exec (mk_assign dv value) ctx gabs |>
                     manager.join gabs |>
                     (fun gabs ->
                        ax.set_abs gabs @@ Abstract.add dict (KeySet.may_add kv keyset) @@ ax.get_abs gabs
                     )
          in
          let gabs =
            match KeySet.must_mem kv keyset, KeySet.may_mem kv keyset with
            | true, _ ->
              gabs
            | false, true ->
              manager.exec (mk_assign dl (mk_binop dl Plus mk_one)) ctx gabs |>
              manager.join gabs
            | false, false ->
              manager.exec (mk_assign dl (mk_binop dl Plus mk_one)) ctx gabs
          in
          [mk_constant PyNone ~etyp:TNone, (gabs, [])]
      in

      empty_case @ non_empty_case
  
      
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function(fname)}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict"}, _)}  as addr))}) :: [],
        []
      )
      when fname = Builtins.class_method_function "dict" "values"
      ->
      let addr', gabs =
        Addr.alloc_instance (Builtins.builtin_address "dict_values") ~param:(Some (Dict addr)) exp.erange manager ctx gabs
      in

      [mk_addr addr' ~erange:exp.erange, (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function(fname)}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict_values"}, Some (Dict dict))}))}) :: [],
        []
      )
      when fname = Builtins.class_method_function "dict_values" "__iter__"
      ->

      let iter, gabs =
        Addr.alloc_instance (Builtins.builtin_address "dict_valueiterator") ~param:(Some (Dict dict)) exp.erange manager ctx gabs
      in

      let gabs = manager.exec (
          mk_assign
            (mk_iter_counter iter)
            (mk_int 0)
        ) ctx gabs
      in

      [mk_addr iter ~erange:exp.erange, (gabs, [])]
  

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function(fname)}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict_valueiterator"}, Some (Dict dict))}  as iter))}) :: [],
        []
      )
      when (fname = Builtins.class_method_function "dict_valueiterator" "__next__") 
      ->
      let dl = mk_dl dict in
      let dv = mk_dv dict in
      let counter = mk_iter_counter iter in

      let in_cond = mk_binop counter Lt dl in
      let in_gabs = manager.exec (mk_stmt (Assume in_cond)) ctx gabs in
      let in_case =
        if SubAbstract.is_bottom (subax.get_abs in_gabs) then
          []
        else
          let tmp = mktmp () in
          let gabs =
            manager.exec (mk_stmt (Expand ((mk_var tmp), dv))) ctx gabs |>
            manager.exec (mk_assign counter (mk_binop counter Plus (mk_int 1))) ctx
          in
          let exp' = {exp with ekind = Var tmp} in
          [exp', (gabs, [mk_stmt (RemoveAll tmp)])]

      in

      let out_cond = mk_binop counter Eq dl in
      let out_gabs = manager.exec (mk_stmt (Assume out_cond)) ctx gabs in
      let out_case =
        if SubAbstract.is_bottom (subax.get_abs out_gabs) then
          []
        else
          let gabs = manager.exec (
              mk_stmt (PyRaise (Some (mk_exp ~erange:exp.erange (Constant (Addr (Builtins.builtin_address "StopIteration"))))))
            ) ctx out_gabs
          in
          [(exp, (gabs, []))]
      
      in

      in_case @ out_case

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function(fname)}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict"}, _)}))}) :: [],
        []
      )
      when fname = Builtins.class_method_function "dict" "keys"
      ->
      panic "dict.keys not supported"

                
    | PyCall(
        {ekind = Constant (Addr {akind = B_function("dict.__len__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "dict"}, _)}))}],
        []
      ) ->
      panic "dict.__len__ not supported"
              

    | _ -> []

  let exec stmt manager ctx ax_ subax_ gabs =
    let ax = mk_domain_ax ax_ in
    match skind stmt with
    | Assign({ekind = PyIndexSubscript(
        {ekind = Constant (Addr ({akind = U_instance ({akind = B_class "dict"}, _)}))},
        index
      )}, exp') ->
      panic "direct assign to dict not supported"

    | RenameAddr(a1, a2) ->
      let abs = ax.get_abs gabs in
      let keyset1 = Abstract.find a1 abs and keyset2 = Abstract.find a2 abs in
      let keyset = KeySet.join keyset1 keyset2 in
      let abs = Abstract.remove a1 abs |>
                Abstract.add a2 keyset
      in
      ax.set_abs gabs abs |>
      continue
        
        
    | _ ->
      continue gabs


  let ask _ _ = Framework.Query.top

  
end

let setup () =
  register name (module Make)
