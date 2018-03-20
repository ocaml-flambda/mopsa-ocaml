open Framework.Domain
open Framework.Ast
open Framework.Query
open Framework.Manager
open Framework.Accessor
open Universal.Ast
open Ast
open Addr
open XAst
open Utils
    
let name = "python.objects.tuples"

module Make(SubLayer : Framework.Layer.S) = struct

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt
  
  module SubAbstract = SubLayer.Abstract

  module Abstract = Framework.Lattice.EmptyLattice
  module Flow = Framework.Lattice.EmptyLattice

  let has_abstract = false
  let has_flow = false
  
  type t = Abstract.t * Flow.t
  type st = SubLayer.t

  let mk_tl addr = mk_exp ~etyp:TInt (PyAttribute(mk_addr addr, "$tl"))
  let mk_tv addr i = mk_exp (PyAttribute(mk_addr addr, "$tv[" ^ (string_of_int i) ^ "]"))
  let mk_tv_z addr z = mk_exp (PyAttribute(mk_addr addr, "$tv[" ^ (Z.to_string z) ^ "]"))
  let mk_iter_counter addr = mk_exp ~etyp:TInt (PyAttribute (mk_addr addr, "$counter"))
          
  let eval (exp : exp) manager ctx ax_ subax_ gabs =
    let subax = mk_domain_ax subax_ in
    match ekind exp with
    | PyTuple(el)  ->
      let addr, gabs =
        Addr.alloc_instance (Builtins.builtin_address "tuple") exp.erange manager ctx gabs
      in

      let tl = mk_tl addr in
      let gabs = manager.exec
          (mk_assign tl (mk_int @@ List.length el))
          ctx gabs
      in

      let rec aux i gabs = function
        | [] -> gabs
        | e :: tail ->
          let tv = mk_tv addr i in
          let gabs = manager.exec
              (mk_assign tv e)
              ctx gabs
          in
          aux (i + 1) gabs tail
      in

      let gabs = eval_exec_list (fun el gabs ->
          aux 0 gabs el
        ) el manager ctx gabs in

      let exp' = mk_addr ~erange:exp.erange addr in

      [exp', (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "tuple.__getitem__"}))},
        [
          {ekind = Constant (Addr {akind = U_instance({akind = B_class "tuple"}, _)})};
          idx
        ],
        []
      ) ->
      panic "tuple.__getitem__ not supported"

    | PySliceSubscript(
        {ekind = Constant (Addr {akind = U_instance({akind = B_class "tuple"}, _)})},
        estart,
        eend,
        estep
      ) ->
      panic "tuple slice not supported"
  
    | PyCall(
        {ekind = Constant (Addr {akind = B_function ("tuple.__init__")})},
        (
          ({ekind = Constant (Addr _)}) ::
          args
        ),
        []
      )
      ->
      panic "tuple() not supported"

    | PyCall(
        {ekind = Constant (Addr {akind = B_function("tuple.__len__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class("tuple")}, _)}))}],
        []
      ) ->
      panic "tuple.__len__ not supported"

        
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function("tuple.__iter__")}))},
        [({ekind = Constant (Addr ({akind = U_instance({akind = B_class "tuple"}, _)}  as tuple))})],
        []
      )
      ->

      let aiter, gabs =
        Addr.alloc_instance (Builtins.builtin_address "tupleiter") ~param:(Some (Tuple tuple)) exp.erange manager ctx gabs
      in
      
      let gabs = manager.exec (
          mk_assign
            (mk_iter_counter aiter)
            (mk_int 0)
        ) ctx gabs
      in

      [mk_addr aiter ~erange:exp.erange, (gabs, [])]
      
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function("tupleiter.__next__")}))},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "tupleiter"}, Some (Tuple tuple))}  as iter))}],
        []
      )
      ->
            let tl = mk_tl tuple in
            let counter = mk_iter_counter iter in
            let counter_var = Universal.Heap.Domain.attribute_var iter "$counter" TInt |> mk_var ~erange:counter.erange in

            let in_cond = mk_in ~right_strict:true counter mk_zero tl in
            debug "in_cond = %a" Framework.Pp.pp_exp in_cond;
            let in_gabs = manager.exec (mk_stmt (Assume in_cond)) ctx gabs in
            debug "in_gabs = %a" manager.print in_gabs;


            let itv = Universal.Numeric.Values.Query.integer_interval counter_var SubLayer.ask subax_ in_gabs in
            debug "itv = %a" Universal.Numeric.Values.Integers.print itv;
            let in_gabs =
              if Universal.Numeric.Values.Integers.is_bottom itv then in_gabs
              else if Universal.Numeric.Values.Integers.is_bounded itv then in_gabs
              else
                manager.exec (mk_assign counter (mk_top TInt)) ctx gabs |>
                manager.exec (mk_stmt (Assume (mk_binop counter Ge mk_zero))) ctx |>
                manager.exec (mk_stmt (Assume (mk_binop counter Lt tl))) ctx
            in
            
            let in_case =
              if SubAbstract.is_bottom (subax.get_abs in_gabs) then
                []
              else
                let counter_var = Universal.Heap.Domain.attribute_var iter "$counter" TInt |> mk_var ~erange:counter.erange in

                try
                  let itv = Universal.Numeric.Values.Query.integer_interval counter_var SubLayer.ask subax_ in_gabs in
                  debug "itv = %a" Universal.Numeric.Values.Integers.print itv;
                  let _ = eval_exec (fun tl gabs ->
                      let itv' = Universal.Numeric.Values.Query.integer_interval tl SubLayer.ask subax_ gabs in
                      debug "itv'(%a) = %a" Framework.Pp.pp_exp tl Universal.Numeric.Values.Integers.print itv';
                      gabs
                    ) tl manager ctx in_gabs
                  in
                  Universal.Numeric.Values.Integers.fold (fun acc i ->
                      debug "assign counter";
                      let gabs = manager.exec (mk_assign counter (mk_z (Z.succ i))) ctx in_gabs in
                      let exp' = mk_tv_z tuple i in
                      ({exp' with erange = exp.erange}, (gabs, [])) :: acc
                    ) [] itv |>
                  re_eval_list manager ctx

                with Universal.Numeric.Values.Integers.Unbounded ->
                  (
                    debug "unbounded interval";
                    let gabs = manager.exec (mk_assign counter (mk_binop counter Plus mk_one)) ctx in_gabs in
                    [{(mk_top TAny) with erange = exp.erange}, (gabs, [])]
                  ) @
                  (
                    let gabs = manager.exec (
                        mk_stmt (PyRaise (Some (mk_exp ~erange:exp.erange (Constant (Addr (Builtins.builtin_address "StopIteration"))))))
                      ) ctx in_gabs
                    in
                    [(exp, (gabs, []))]
                  )
            in

            let out_cond = mk_binop counter Eq tl in
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

    | PyCall({ekind = Constant (Addr ({akind = B_function f}))}, _, _)
      when Builtins.is_class_dot_method "tuple" f ->

      panic "Tuple function %s not implemented" f



    | _ -> []


  let exec stmt manager ctx ax_ subax_ gabs =
    continue gabs
    
  let ask _ _ = Framework.Query.top


end

let setup () =
  register name (module Make)
