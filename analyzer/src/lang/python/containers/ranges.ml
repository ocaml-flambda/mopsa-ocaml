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
    
let name = "python.objects.ranges"

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
          
  let eval (exp : exp) manager ctx ax_ subax_ gabs =
    let subax = mk_domain_ax subax_ in
    match ekind exp with
    | PyCall(
        {ekind = Constant (Addr {akind = B_function (name)})},
        (
          ({ekind = Constant (Addr arange)} as erange) ::
          args
        ),
        []
      )
      when name = (Builtins.class_method_function "range" "__init__")
      ->

      let start, stop=
        match args with
        | [{etyp = TInt} as start; {etyp = TInt} as stop] -> start, stop
        | [{etyp = TInt} as stop] -> mk_int 0, stop
        | _ -> panic "range not supported"
      in
    
      let gabs = manager.exec (
          mk_assign
            (mk_exp (PyAttribute (erange, "$start")))
            start
        ) ctx gabs
      in

      let gabs = manager.exec (
          mk_assign
            (mk_exp (PyAttribute (erange, "$stop")))
            stop
        ) ctx gabs
      in
        
      let exp' = {exp with ekind = Constant PyNone; etyp = TNone} in
      [exp', (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr {akind = B_function("range.__len__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "range"}, _)}))} as rexp],
        []
      ) ->

      let start = mk_exp (PyAttribute (rexp, "$start")) in
      let stop = mk_exp (PyAttribute (rexp, "$stop")) in      
      let exp' = mk_exp (BinOp(Minus, stop, start)) in
      let cond = mk_exp (BinOp (Ge, exp', mk_int 0)) in
      let gabs_pos = manager.exec (
          mk_stmt (Assume cond)
        ) ctx gabs in
      let gabs_neg = manager.exec (
          mk_stmt (Assume (negate cond))
        ) ctx gabs in

      [(exp', (gabs_pos, [])); (mk_int 0, (gabs_neg, []))] |>
      re_eval_list manager ctx

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function("range.__iter__")}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "range"}, _)}  as range))}) :: [],
        []
      )
      ->

      let aiter, gabs =
        Addr.alloc_instance (Builtins.builtin_address "rangeiter") ~param:(Some (Range range)) exp.erange manager ctx gabs
      in

      let iter = mk_addr ~erange:exp.erange aiter in
      
      let gabs = manager.exec (
          mk_assign
            (mk_exp (PyAttribute (iter, "$counter")))
            (mk_int 0)
        ) ctx gabs
      in

      [(iter, (gabs, []))]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function(fname)}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "rangeiter"}, Some (Range range))}))} as eiter) :: [],
        []
      )
      when (fname = Builtins.class_method_function "rangeiter" "__next__")
      ->
      let erange = mk_addr range in
      
      let cond = 
        mk_in ~right_strict:true
          (mk_binop (mk_exp (PyAttribute (eiter, "$counter"))) Plus (mk_exp (PyAttribute (erange, "$start"))))
          (mk_exp (PyAttribute (erange, "$start")))
          (mk_exp (PyAttribute (erange, "$stop")))
      in

      let cond' = negate cond in

      let gabs_in = manager.exec (
          mk_stmt (Assume cond)
        ) ctx gabs in

      let gabs_out = manager.exec (
          mk_stmt (Assume cond')
        ) ctx gabs in


      let tmp' = Universal.Ast.mktmp () in
      let gabs_in' =
        manager.exec (
          mk_assign
            (mk_exp (Var tmp'))
            (mk_binop
               (mk_exp (PyAttribute (erange, "$start")))
               Plus
               (mk_exp (PyAttribute (eiter, "$counter")))
            )
        ) ctx gabs_in
        |>
        manager.exec (
          mk_assign
            (mk_exp (PyAttribute (eiter, "$counter")))
            (mk_binop
               (mk_exp (PyAttribute (eiter, "$counter")))
               Plus
               (mk_int 1)
            )
        ) ctx  
      in

      let gabs_out' =
        manager.exec (
          mk_stmt (PyRaise (Some (mk_addr ~erange:exp.erange (Builtins.builtin_address "StopIteration"))))
        ) ctx gabs_out in

      let ignore_stmt = [mk_stmt (RemoveAll tmp')] in

      ((mk_exp (Var tmp'), (gabs_in', ignore_stmt)) |> re_eval manager ctx) @
      [(exp, (gabs_out', ignore_stmt))]


    | PyCall(
        {ekind = Constant (Addr {akind = B_function("range.__contains__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "range"}, _)}))}  as range; {etyp = TInt} as value],
        []
      ) ->
      begin

        let start = mk_exp (PyAttribute (range, "$start")) in
        let stop = mk_exp (PyAttribute (range, "$stop")) in      

        let can_be_true =
          not @@ SubAbstract.is_bottom @@
          subax.get_abs @@
          manager.exec (mk_stmt (Assume (mk_in ~right_strict:true value start stop))) ctx gabs
        in

        debug "can be true = %b" can_be_true;

        debug "check false";

        let can_be_false =
          not @@ SubAbstract.is_bottom @@
          subax.get_abs @@
          manager.exec (mk_stmt (Assume (negate (mk_in ~right_strict:true value start stop)))) ctx gabs
        in

        debug "can be false = %b" can_be_false;

        match can_be_true, can_be_false with
        | true, false -> [mk_true, (gabs, [])]
        | false, true -> [mk_false, (gabs, [])]
        | true, true -> [(mk_true, (gabs, [])); (mk_false, (gabs, []))] (* FIXME: imporove precision by partitioning gabs *)
        | false, false -> [exp, (subax.set_abs gabs SubAbstract.bottom, [])]

      end  

    | PyCall(
        {ekind = Constant (Addr {akind = B_function("range.__contains__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "range"}, _)}))}; _],
        []
      ) ->
      panic "range.__contains__ on non-ints not supported"
  

    | PyCall({ekind = Constant (Addr ({akind = B_function f}))}, _, _)
      when Builtins.is_class_dot_method "range" f ->
      
      panic "Range function %s not implemented" f


    | _ -> []

  let exec _ _ _ _ _ gabs = continue gabs
  let ask _ _ = Framework.Query.top


end

let setup () =
  register name (module Make)
