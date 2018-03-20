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
    
let name = "python.objects.lists"

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

  let set_list_length manager ctx addr el gabs =
    manager.exec {
      skind = Assign (
          {ekind = PyAttribute(
               mk_addr addr,
               "$ll"
             ); etyp = TAny; erange = unknown_range},
          (mk_int (List.length el))
        );
      srange = unknown_range
    } ctx gabs
  
  let set_list_value manager ctx addr el gabs =
    match el with
    | [] ->
      manager.exec
        (mk_assign
           (mk_exp (PyAttribute(mk_addr addr,"$lv")))
           (mk_exp (Constant (PyEmptyValue)))
        ) ctx gabs

    | hd :: tl ->
      let stmt e = {
        skind = Assign (
            {ekind = PyAttribute(
                 mk_addr addr,
                 "$lv"
               ); etyp = TAny; erange = unknown_range},
            e
          );
        srange = unknown_range
      }
      in
      tl |> List.fold_left (fun acc e ->
          manager.exec (stmt e) ctx gabs |>
          manager.join acc
        ) (manager.exec (stmt hd) ctx gabs)

  let create_post_ignore_stmtl vars =
    vars |> List.map (fun v -> {
          skind = RemoveAll v;
          srange = unknown_range;
        })

  let mk_assign_next_loop iter vval assign_stmt =
    mk_stmt (
      PyTry (
        mk_stmt (While (
            mk_int 1,
            mk_stmt (Block [
                mk_assign (mk_var vval) (mk_exp (PyCall(
                    mk_addr (Builtins.builtin_address "next"),
                    [mk_exp (Var iter)],
                    []
                  )));
                assign_stmt (mk_var vval)
              ])
          )),
        [mk_except
           (Some (mk_addr (Builtins.builtin_address "StopIteration")))
           None
           (mk_stmt nop)
        ],
        mk_stmt nop,
        mk_stmt nop
      )
    )

  let mk_ll addr = mk_exp ~etyp:TInt (PyAttribute(mk_addr addr, "$ll"))
  let mk_lv addr = mk_exp (PyAttribute(mk_addr addr, "$lv"))

  let mk_iter_counter addr = mk_exp ~etyp:TInt (PyAttribute (mk_addr addr, "$counter"))

  let add_element addr e manager ctx gabs =
    let gabs = manager.exec
          (mk_stmt (Assign (
                (mk_exp (PyAttribute(mk_addr addr, "$ll"))),
                (mk_binop
                   (mk_exp (PyAttribute(mk_addr addr, "$ll")))
                   Plus
                   (mk_int 1)
                )
              ))) ctx gabs
      in

      let empty_list_gabs =
        manager.exec
          (mk_stmt (Assume
                      (mk_binop
                         (mk_exp (PyAttribute(mk_addr addr, "$ll")))
                         Eq
                         (mk_int 1)
                      )
                   )) ctx gabs
      in
    
      let nonempty_list_gabs =
        manager.exec
                    (mk_stmt (Assume
                      (mk_binop
                         (mk_exp (PyAttribute(mk_addr addr, "$ll")))
                         Gt
                         (mk_int 1)
                      )
                   )) ctx gabs
      in

      debug "empty_list_gabs = @\n@[  %a@]@\n nonempty_list_gabs = @\n@[  %a@]"
        manager.print empty_list_gabs
        manager.print nonempty_list_gabs
      ;
        
      manager.join (
        debug "add1";
        let gabs' = manager.exec
          (mk_assign (mk_exp (PyAttribute(mk_addr addr, "$lv"))) e)
          ctx nonempty_list_gabs
        in
        debug "add1 gabs = @\n@[  %a@]" manager.print gabs';
        manager.join nonempty_list_gabs gabs' |>
        (fun gabs -> debug "add1' gabs = @\n@[  %a@]" manager.print gabs; gabs)
      ) ( 
        let gabs' = manager.exec
          (mk_assign (mk_exp (PyAttribute(mk_addr addr, "$lv"))) e)
          ctx empty_list_gabs in
        debug "add2 gabs = @\n@[  %a@]" manager.print gabs';
        gabs'
      ) |>
      (fun gabs -> debug "add gabs = @\n@[  %a@]" manager.print gabs; gabs)

  let check_index_access alist index manager ctx gabs =
    let ll = mk_ll alist in
    let cond_safe = mk_binop
        (mk_binop index Ge (mk_unop Minus ll))
        LAnd
        (mk_binop index Lt ll)
    in  
    let gabs_safe = manager.exec
        (mk_stmt (Assume cond_safe)) ctx gabs
    in
    let gabs_fail = manager.exec
      (mk_stmt (Assume (negate cond_safe))) ctx gabs
    in
    debug "index access:@ list: %a@ index: %a@, gabs: @[%a@]@, safe case:@ @[%a@]@ fail case:@ @[%a@]"
      Universal.Pp.pp_addr alist
      Framework.Pp.pp_exp index
      manager.print gabs
      manager.print gabs_safe
      manager.print gabs_fail
    ;
    gabs_safe, gabs_fail


  let mult addr n range manager ctx subax gabs =
    let addr', gabs =
      Addr.alloc_instance (Builtins.builtin_address "list") range manager ctx gabs
    in

    let ll = mk_ll addr in
    let lv = mk_lv addr in

    let ll' = mk_ll addr' in
    let lv' = mk_lv addr' in

    let gabs = manager.exec (mk_assign ll' (mk_binop ll Mult n)) ctx gabs in
    let gabs = manager.exec (mk_stmt (Expand (lv', lv))) ctx gabs in
    addr', gabs

  
  let fold_increasing_slice_length_cases f x0 estart eend estep ll manager ctx subax gabs =
    debug "increasing slice case";
    let start_cases = 
      match ekind estart with
      | Constant PyNone -> [mk_zero, mk_one]
      | _ ->
        [
          (ll, mk_binop estart Ge ll);
          (mk_zero, mk_binop (mk_binop ll Plus estart) Le mk_zero);
          (mk_binop ll Plus estart, mk_in ~strict:true estart (mk_neg ll) mk_zero);
          (estart, mk_in estart mk_zero ll)
        ]
    in

    let start_cases = start_cases |>
                      List.map (fun (e, cond) -> (e, manager.exec (mk_stmt (Assume cond)) ctx gabs)) |>
                      List.filter (fun (_, gabs) -> not (SubAbstract.is_bottom (subax.get_abs gabs)))
    in
    
    let end_cases =
      match ekind eend with
      | Constant PyNone -> [ll, mk_one]
      | _ ->
        [
          (ll, mk_binop eend Ge ll);
          (mk_zero, mk_binop (mk_binop ll Plus eend) Le mk_zero);
          (mk_binop ll Plus eend, mk_in ~strict:true eend (mk_neg ll) mk_zero);
          (eend, mk_in eend mk_zero ll)
        ]
    in

    let end_cases = end_cases |>
                      List.map (fun (e, cond) -> (e, manager.exec (mk_stmt (Assume cond)) ctx gabs)) |>
                      List.filter (fun (_, gabs) -> not (SubAbstract.is_bottom (subax.get_abs gabs)))
    in

    
    let cases =
      start_cases |> List.fold_left (fun acc (estart, start_gabs) ->
          end_cases |> List.fold_left (fun acc (eend, end_gabs) ->
              let gabs = manager.meet start_gabs end_gabs in
              if SubAbstract.is_bottom (subax.get_abs gabs) then
                acc
              else
                ((mk_binop eend Minus estart), gabs) :: acc
            ) acc
        ) []
    in
    List.fold_left f x0 cases

  let fold_decreasing_slice_length_cases f x0 estart eend estep ll manager ctx subax gabs =
    debug "decreasing slice case";
    let start_cases = 
      match ekind estart with
      | Constant PyNone -> [mk_binop ll Minus mk_one, mk_one]
      | _ ->
        [
          (mk_binop ll Minus mk_one, mk_binop estart Ge (mk_binop ll Minus mk_one));
          (mk_zero, mk_binop (mk_binop ll Plus estart) Le mk_zero);
          (mk_binop ll Plus estart, mk_in ~strict:true estart (mk_neg ll) mk_zero);
          (estart, mk_in estart mk_zero (mk_binop ll Minus mk_one))
        ]
    in
    debug "start cases";
    let start_cases = start_cases |>
                      List.map (fun (e, cond) -> (e, manager.exec (mk_stmt (Assume cond)) ctx gabs)) |>
                      List.filter (fun (_, gabs) -> not (SubAbstract.is_bottom (subax.get_abs gabs)))
    in
    
    let end_cases =
      match ekind eend with
      | Constant PyNone -> [mk_zero, mk_one]
      | _ ->
        [
          (ll, mk_binop eend Ge (mk_binop ll Minus mk_one));
          (mk_zero, mk_binop (mk_binop ll Plus eend) Le mk_zero);
          (mk_binop (mk_binop ll Plus eend) Plus mk_one, mk_in ~strict:true eend (mk_neg ll) mk_zero);
          (mk_binop eend Plus mk_one, mk_in eend mk_zero ll)
        ]
    in
    debug "end cases";
    let end_cases = end_cases |>
                      List.map (fun (e, cond) -> (e, manager.exec (mk_stmt (Assume cond)) ctx gabs)) |>
                      List.filter (fun (_, gabs) -> not (SubAbstract.is_bottom (subax.get_abs gabs)))
    in

    debug "combining";
    let cases =
      start_cases |> List.fold_left (fun acc (estart, start_gabs) ->
          end_cases |> List.fold_left (fun acc (eend, end_gabs) ->
              let gabs = manager.meet start_gabs end_gabs in
              if SubAbstract.is_bottom (subax.get_abs gabs) then
                acc
              else
                (mk_binop (mk_binop estart Minus eend) Plus mk_one, gabs) :: acc
            ) acc
        ) []
    in
    List.fold_left f x0 cases
  
  let fold_slice_length_cases f x0 estart eend estep ll manager ctx cax gabs =
    match ekind estep with
    | Constant PyNone -> fold_increasing_slice_length_cases f x0 estart eend estep ll manager ctx cax gabs

    |  _ ->
      let n = exp_to_int estep in
      match n with
      | Some n when n = 1 ->
        fold_increasing_slice_length_cases f x0 estart eend estep ll manager ctx cax gabs

      | Some n when n = -1 ->
        fold_decreasing_slice_length_cases f x0 estart eend estep ll manager ctx cax gabs

      | _ -> Debug.fail "Unsupported slice step"


  let create_list el range manager ctx subax gabs =
    let addr, gabs =
      Addr.alloc_instance (Builtins.builtin_address "list") range manager ctx gabs
    in

    let gabs = eval_exec_list (fun el gabs ->
        set_list_value manager ctx addr el gabs
      ) el manager ctx gabs
    in

    let gabs = set_list_length manager ctx addr el gabs in
    
    let exp = mk_addr addr ~erange:range in
    exp, gabs, []
 
  let eval (exp : exp) manager ctx ax subax_ gabs =
    let subax = mk_domain_ax subax_ in
    match ekind exp with
    | PyList (el)
      ->
      let exp, gabs, stmts = create_list el exp.erange manager ctx subax gabs in
      [(exp, (gabs, stmts))]
      
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function("list.__iter__")}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))}) :: [],
        []
      ) ->
      let aiter, gabs =
        Addr.alloc_instance (Builtins.builtin_address "listiter") ~param:(Some (List alist)) exp.erange manager ctx gabs
      in


      let gabs = manager.exec (
          mk_assign
            (mk_iter_counter aiter)
            (mk_int 0)
        ) ctx gabs
      in

      [mk_addr aiter ~erange:exp.erange, (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function("listiter.__next__")}))},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "listiter"}, Some (List alist))}  as aiter))}],
        []
      )
      ->
            let ll = mk_ll alist in
            let lv = mk_lv alist in
            let counter = mk_iter_counter aiter in

            let in_cond = mk_binop counter Lt ll in
            let in_gabs = manager.exec (mk_stmt (Assume in_cond)) ctx gabs in
            let in_case =
              if SubAbstract.is_bottom (subax.get_abs in_gabs) then
                []
              else
                let tmp = mktmp () in
                let gabs =
                  manager.exec (mk_stmt (Expand ((mk_var tmp), lv))) ctx gabs |>
                  manager.exec (mk_assign counter (mk_binop counter Plus (mk_int 1))) ctx
                in
                let exp' = {exp with ekind = Var tmp} in
                (exp', (gabs, [mk_stmt (RemoveAll tmp)])) |>
                re_eval manager ctx

            in

            let out_cond = mk_binop counter Eq ll in
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
        {ekind = Constant (Addr {akind = B_function("list.__contains__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as list))}; value],
        []
      ) ->
      begin
        let ll = mk_ll list and lv = mk_lv list in

        let can_be_true =
          let gabs = manager.exec (mk_stmt (Assume (mk_binop ll Ge mk_one))) ctx gabs in
          not @@ SubAbstract.is_bottom @@ subax.get_abs gabs &&
          not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop lv Eq value))) ctx gabs
        in

        debug "can be true = %b" can_be_true;

        debug "check false";

        let can_be_false =
          not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop ll Eq mk_zero))) ctx gabs ||
          not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop lv Ne value))) ctx gabs
        in

        debug "can be false = %b" can_be_false;

        match can_be_true, can_be_false with
        | true, false -> [mk_true, (gabs, [])]
        | false, true -> [mk_false, (gabs, [])]
        | true, true -> [(mk_true, (gabs, [])); (mk_false, (gabs, []))] (* FIXME: imporove precision by partitioning gabs *)
        | false, false -> [exp, (subax.set_abs gabs SubAbstract.bottom, [])]

      end  
      
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__getitem__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as alist))};
          index
        ],
        []
      ) ->
      let gabs_safe, gabs_fail = check_index_access alist index manager ctx gabs in

      let safe_case =
        if SubAbstract.is_bottom (subax.get_abs gabs_safe) then
          []
        else
          let lv = mk_lv alist in
          let tmp = mktmp () in
          let gabs_safe = manager.exec (mk_stmt (Expand ((mk_var tmp), lv))) ctx gabs_safe in
          let exp' = {exp with ekind = Var tmp} in
          (exp', (gabs_safe, [mk_stmt (RemoveAll tmp)])) |>
          re_eval manager ctx
      in

      let fail_case = 
        if SubAbstract.is_bottom (subax.get_abs gabs_fail) then
          []
        else
          let gabs_fail =  manager.exec
            (mk_stmt (PyRaise (Some (mk_exp ~erange:exp.erange (Constant (Addr (Builtins.builtin_address "IndexError")))))))
            ctx gabs_fail
          in
          [(exp, (gabs_fail, []))]
      in

      safe_case @ fail_case



    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__setitem__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as alist))};
          index;
          exp'
        ],
        []
      ) ->


      let safe_gabs, fail_gabs = check_index_access alist index manager ctx gabs in

      let safe_case =
        if SubAbstract.is_bottom (subax.get_abs safe_gabs) then
          manager.bottom
        else
          let ll = mk_ll alist in
          let lv = mk_lv alist in

          let singleton_cond = mk_binop ll Eq (mk_int 1) in
          let singleton_gabs = manager.exec (mk_stmt (Assume singleton_cond)) ctx safe_gabs in
          let singleton_gabs =
            if SubAbstract.is_bottom (subax.get_abs singleton_gabs) then
              manager.bottom
            else
              manager.exec (mk_assign lv exp') ctx singleton_gabs
          in

          let otherwise_cond = mk_binop ll Ge (mk_int 2) in
          let otherwise_gabs = manager.exec (mk_stmt (Assume otherwise_cond)) ctx safe_gabs in
          let otherwise_gabs =
            if SubAbstract.is_bottom (subax.get_abs otherwise_gabs) then
              manager.bottom
            else
              manager.exec (mk_assign lv exp') ctx otherwise_gabs |>
              manager.join otherwise_gabs
          in

          manager.join singleton_gabs otherwise_gabs
      in

      let fail_case = 
        if SubAbstract.is_bottom (subax.get_abs fail_gabs) then
          manager.bottom
        else
          manager.exec
            (mk_stmt (PyRaise (Some (mk_addr ~erange:exp.erange (Builtins.builtin_address "IndexError")))))
            ctx fail_gabs
      in

      let gabs = manager.join safe_case fail_case in
      [mk_constant PyNone ~etyp:TNone, (gabs, [])]

               
    | PyCall(
        {ekind = Constant (Addr {akind = B_function("list.__len__")})},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}))} as elist],
        []
      ) ->
      ({exp with ekind = PyAttribute(elist, "$ll")}, (gabs, [])) |>
      re_eval manager ctx

    | PyCall(
        {ekind = Constant (Addr {akind = B_function "list.__init__"})},
        [
          {ekind = Constant (Addr alist)} as elist;
          {ekind = Constant (Addr aarg)} as earg
        ],
        []
      )
      ->

      let gabs = manager.exec
          (mk_assign
             (mk_exp (PyAttribute(elist,"$ll")))
             (mk_int 0)
          ) ctx gabs
      in
      
      let gabs = manager.exec
          (mk_assign
             (mk_exp (PyAttribute(elist,"$lv")))
             (mk_exp (Constant (PyEmptyValue)))
          ) ctx gabs
      in

      
      let iter_tmp = mktmp () in
      let gabs = manager.exec (
          mk_assign
            (mk_exp (Var iter_tmp))
            (mk_exp ~erange:elist.erange(PyCall (
                 (mk_exp (PyAttribute (earg, "__iter__")),
                  [],
                  []
                 )
               )))
        ) ctx gabs in
      
      let list_vval = mktmp () in
      let gabs = manager.exec
          (mk_assign_next_loop
             iter_tmp
             list_vval
             (fun value ->
                (mk_stmt (Expression (mk_exp (PyCall(
                     (mk_addr (Builtins.class_method_function "list" "append" |> Builtins.builtin_address)),
                     elist :: [value],
                     []
                   )))))
             )
          ) ctx gabs
      in

      let gabs = manager.exec (mk_stmt (RemoveAddrExp (mk_var iter_tmp))) ctx gabs in
      
      let exp' = {exp with ekind = Constant PyNone; etyp = TNone} in
      [(exp', (gabs, [mk_stmt (RemoveAll iter_tmp); mk_stmt (RemoveAll list_vval)]))]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.append"}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))}) :: [e],
        []
      )
      ->
      let gabs = add_element alist e manager ctx gabs in
      [{exp with ekind = Constant PyNone; etyp = TNone}, (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.insert"}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))}) :: args,
        []
      )
      ->
      begin
        match args with
        | [idx; e] ->
          (* FIXME: check that idx has an integer type *)
          let gabs = add_element alist e manager ctx gabs in
          [{exp with ekind = Constant PyNone; etyp = TNone}, (gabs, [])]
        | _ ->
          let gabs = manager.exec
              (mk_stmt (PyRaise (Some (mk_addr ~erange:exp.erange (Builtins.builtin_address "TypeError")))))
              ctx gabs
          in
          [(exp, (gabs, []))]
      end
      
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.pop"}))},
        ({ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))}) :: args,
        []
      )
      ->
      begin
        match args with
        | [] | [_] ->
          begin
            let idx =
              match args with
              | [] -> mk_int 0
              | [x] -> x
              | _ -> assert false
            in

            let ll = mk_ll alist in
            let lv = mk_lv alist in

            let ok_cond =
              mk_binop
                (mk_binop idx Ge (mk_unop Minus ll))
                LAnd
                (mk_binop idx Lt ll)
            in
            let ok_gabs = manager.exec (mk_stmt (Assume ok_cond)) ctx gabs in
            let ok_case =
              if SubAbstract.is_bottom (subax.get_abs ok_gabs) then
                []
              else
                let gabs = manager.exec (mk_assign ll (mk_binop ll Minus (mk_int 1))) ctx ok_gabs in
                let tmp = mktmp () in
                let gabs = manager.exec (mk_stmt (Expand ((mk_var tmp), lv))) ctx gabs in
                let exp' = {exp with ekind = Var tmp} in
                (exp', (gabs, [mk_stmt (RemoveAll tmp)])) |>
                re_eval manager ctx
            in

            let error_cond = negate ok_cond in
            let error_gabs = manager.exec (mk_stmt (Assume error_cond)) ctx gabs in
            let error_case =
              if SubAbstract.is_bottom (subax.get_abs error_gabs) then
                []
              else
                let gabs =  manager.exec
                    (mk_stmt (PyRaise (Some (mk_addr ~erange:exp.erange (Builtins.builtin_address "IndexError")))))
                    ctx error_gabs
                in
                [(exp, (gabs, []))]
            in

            ok_case @ error_case
            
          end
        | _ ->
          let gabs = manager.exec
              (mk_stmt (PyRaise (Some (mk_addr ~erange:exp.erange (Builtins.builtin_address "TypeError")))))
              ctx gabs
          in
          [(exp, (gabs, []))]
      end

    | PySliceSubscript(
        {ekind = Constant (Addr ({akind = U_instance ({akind = B_class "list"}, _)} as alist))},
        estart,
        eend,
        estep
      ) ->
      begin

        let alist', gabs =
          Addr.alloc_instance (Builtins.builtin_address "list") exp.erange manager ctx gabs
        in

        let ll = mk_ll alist in
        let lv = mk_lv alist in

        let exp' = {exp with ekind = Constant (Addr alist'); etyp = TAddr} in
        let ll' = mk_ll alist' in
        let lv' = mk_lv alist' in

        
        fold_slice_length_cases (fun acc (lle', gabs) ->
            debug "slice length %a in @ @[%a@]" Framework.Pp.pp_exp lle' SubAbstract.print (subax.get_abs gabs);
            let in_cond =
              mk_binop
                (mk_binop (mk_int 0) Lt lle')
                LAnd
                (mk_binop lle' Le ll)
            in
            let in_gabs = manager.exec (mk_stmt (Assume in_cond)) ctx gabs in
            let in_case =
              if SubAbstract.is_bottom (subax.get_abs in_gabs) then
                []
              else
                let gabs = manager.exec (mk_assign ll' lle') ctx in_gabs in
                let gabs = manager.exec (mk_stmt (Expand (lv', lv))) ctx gabs in
                [exp', (gabs, [])]
            in

            let empty_cond = mk_binop lle' Le (mk_int 0) in
            let empty_gabs = manager.exec (mk_stmt (Assume empty_cond)) ctx gabs in
            let empty_case =
              if SubAbstract.is_bottom (subax.get_abs empty_gabs) then
                []
              else
                let gabs = manager.exec (mk_assign ll' (mk_int 0)) ctx empty_gabs in
                let gabs = manager.exec (mk_assign lv' (mk_exp (Constant PyEmptyValue))) ctx gabs in
                [(exp', (gabs, []))]
            in

            let sup_cond = mk_binop lle' Gt ll in
            let sup_gabs = manager.exec (mk_stmt (Assume sup_cond)) ctx gabs in
            let sup_case =
              if SubAbstract.is_bottom (subax.get_abs sup_gabs) then
                []
              else
                let gabs = manager.exec (mk_assign ll' ll) ctx sup_gabs in
                let gabs = manager.exec (mk_stmt (Expand (lv', lv))) ctx gabs in
                let exp' = {exp with ekind = Constant (Addr alist'); etyp = TAddr} in
                [exp', (gabs, [])]
            in
            acc @ in_case @ empty_case @ sup_case 
          ) [] estart eend estep ll manager ctx subax gabs

      end



    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__add__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l1))};
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l2))}
        ],
        []
      )
      ->
      let addr, gabs =
        Addr.alloc_instance (Builtins.builtin_address "list") exp.erange manager ctx gabs
      in

      let ll = mk_ll addr in
      let lv = mk_lv addr in

      let ll1 = mk_ll l1 in
      let lv1 = mk_lv l1 in

      let ll2 = mk_ll l2 in
      let lv2 = mk_lv l2 in

      let gabs = manager.exec (mk_assign ll (mk_binop ll1 Plus ll2)) ctx gabs in
      (* FIXME: improve precision by checing if l1 or l2 are empty *)
      let gabs1 = manager.exec (mk_stmt (Expand (lv, lv1))) ctx gabs in
      let gabs2 = manager.exec (mk_stmt (Expand (lv, lv2))) ctx gabs in
      let gabs = manager.join gabs1 gabs2 in

      [mk_addr ~erange:exp.erange addr, (gabs, [])]

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__iadd__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l1))};
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l2))}
        ],
        []
      )
      ->
      let ll1 = mk_ll l1 in
      let lv1 = mk_lv l1 in

      let ll2 = mk_ll l2 in
      let lv2 = mk_lv l2 in

      let gabs = manager.exec (mk_assign ll1 (mk_binop ll1 Plus ll2)) ctx gabs in
      (* FIXME: improve precision by checing if l1 or l2 are empty *)
      let gabs1 = manager.exec (mk_stmt (Expand (lv1, lv2))) ctx gabs in
      let gabs = manager.join gabs1 gabs in

      [mk_addr ~erange:exp.erange l1, (gabs, [])]


    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__mul__"}))},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))};
         {etyp = TInt} as arg],
        []
      )
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__rmul__"}))},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))};
         {etyp = TInt} as arg],
        []
      )
    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__imul__"}))},
        [{ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}  as alist))};
         {etyp = TInt} as arg],
        []
      )

      ->

      let addr', gabs = mult alist arg exp.erange manager ctx subax gabs in
      [{exp with ekind = Constant (Addr addr'); etyp = TAddr}, (gabs, [])]
        

    | PyListComprehension(e, comprhs) ->
      let exp, gabs, stmts = create_list [] exp.erange manager ctx subax gabs in
      let addr = exp_to_addr exp in
      let rec aux = function
        | [] ->
          mk_stmt (Expression (mk_exp (PyCall(
              (mk_addr (Builtins.class_method_function "list" "append" |> Builtins.builtin_address)),
              (mk_addr addr) :: [e],
              []
            ))))

        | (target, iter, []) :: tl ->
          mk_stmt (PyFor (
              target,
              iter,
              aux tl,
              mk_stmt nop
            ))

        | (target, iter, [cond]) :: tl ->
          mk_stmt (PyFor (
              target,
              iter,
              mk_stmt (If (cond, aux tl, mk_stmt nop)),
              mk_stmt nop
            ))

        | _ -> assert false
            
      in

      let stmt = aux comprhs in
      let gabs = manager.exec stmt ctx gabs in

      let exp' = {exp with ekind = Constant (Addr addr); etyp = TAddr} in
      [exp', (gabs, stmts)]



    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__eq__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l1))};
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l2))}
        ],
        []
      )
      ->
      begin
        let ll1 = mk_ll l1 and ll2 = mk_ll l2 in
        let lv1 = mk_lv l1 and lv2 = mk_lv l2 in

        let l1_empty_gabs = manager.exec (mk_stmt (Assume (mk_binop ll1 Eq mk_zero))) ctx gabs in
        let l2_empty_gabs = manager.exec (mk_stmt (Assume (mk_binop ll2 Eq mk_zero))) ctx gabs in

        let can_be_true, can_be_false =
          match SubAbstract.is_bottom @@ subax.get_abs l1_empty_gabs, SubAbstract.is_bottom @@ subax.get_abs l2_empty_gabs with
          | false, false -> true, false
          | false, true -> false, true
          | true, false -> false, true
          | true, true -> true, true
        in
        
        debug "check true";

        let can_be_true =
          can_be_true ||
          (
            let gabs = manager.exec (mk_stmt (Assume (mk_binop ll1 Ge mk_one))) ctx gabs |>
                       manager.exec (mk_stmt (Assume (mk_binop ll2 Ge mk_one))) ctx |>
                       manager.exec (mk_stmt (Assume (mk_binop ll1 Eq ll2))) ctx
            in
            not @@ SubAbstract.is_bottom @@ subax.get_abs gabs &&
            not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop lv1 Eq lv2))) ctx gabs
          )
        in

        debug "can be true = %b" can_be_true;

        debug "check false";

        let can_be_false =
          can_be_false || (
            let gabs = manager.exec (mk_stmt (Assume (mk_binop ll1 Ge mk_one))) ctx gabs |>
                       manager.exec (mk_stmt (Assume (mk_binop ll2 Ge mk_one))) ctx
            in
            not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop ll1 Ne ll2))) ctx gabs ||
            not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop lv1 Ne lv2))) ctx gabs
          )
        in

        debug "can be false = %b" can_be_false;

        match can_be_true, can_be_false with
        | true, false -> [mk_true, (gabs, [])]
        | false, true -> [mk_false, (gabs, [])]
        | true, true -> [(mk_true, (gabs, [])); (mk_false, (gabs, []))] (* FIXME: imporove precision by partitioning gabs *)
        | false, false -> [exp, (subax.set_abs gabs SubAbstract.bottom, [])]
      end

    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__eq__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}))};
          _
        ],
        []
      )
      ->
      [mk_false, (gabs, [])]


    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__ne__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l1))};
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as l2))}
        ],
        []
      )
      ->

      begin
        let ll1 = mk_ll l1 and ll2 = mk_ll l2 in
        let lv1 = mk_lv l1 and lv2 = mk_lv l2 in

        let l1_empty_gabs = manager.exec (mk_stmt (Assume (mk_binop ll1 Eq mk_zero))) ctx gabs in
        let l2_empty_gabs = manager.exec (mk_stmt (Assume (mk_binop ll2 Eq mk_zero))) ctx gabs in

        let can_be_true, can_be_false =
          match SubAbstract.is_bottom @@ subax.get_abs l1_empty_gabs, SubAbstract.is_bottom @@ subax.get_abs l2_empty_gabs with
          | false, false -> false, true
          | false, true -> true, false
          | true, false -> true, false
          | true, true -> true, true
        in

        let can_be_true =
          can_be_true ||
          (
            let gabs = manager.exec (mk_stmt (Assume (mk_binop ll1 Ge mk_one))) ctx gabs |>
                       manager.exec (mk_stmt (Assume (mk_binop ll2 Ge mk_one))) ctx
            in
            not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop ll1 Ne ll2))) ctx gabs ||
            not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop lv1 Ne lv2))) ctx gabs
          )
        in        
        
        let can_be_false =
          can_be_false ||
          (
            let gabs = manager.exec (mk_stmt (Assume (mk_binop ll1 Ge mk_one))) ctx gabs |>
                       manager.exec (mk_stmt (Assume (mk_binop ll2 Ge mk_one))) ctx |>
                       manager.exec (mk_stmt (Assume (mk_binop ll1 Eq ll2))) ctx
            in
            not @@ SubAbstract.is_bottom @@ subax.get_abs gabs &&
            not @@ SubAbstract.is_bottom @@ subax.get_abs @@ manager.exec (mk_stmt (Assume (mk_binop lv1 Eq lv2))) ctx gabs
          )
        in

        match can_be_true, can_be_false with
        | true, false -> [mk_true, (gabs, [])]
        | false, true -> [mk_false, (gabs, [])]
        | true, true -> [(mk_true, (gabs, [])); (mk_false, (gabs, []))] (* FIXME: imporove precision by partitioning gabs *)
        | false, false -> [exp, (subax.set_abs gabs SubAbstract.bottom, [])]
      end


    | PyCall(
        {ekind = Constant (Addr ({akind = B_function "list.__ne__"}))},
        [
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}))};
          _
        ],
        []
      )
      ->
      [mk_true, (gabs, [])]
  

    | PyCall({ekind = Constant (Addr ({akind = B_function f}))}, _, _)
        when Builtins.is_class_dot_method "list" f ->

        panic "List function %s not implemented" f


    | _ -> []

  let exec stmt manager ctx ax_ subax_ gabs =
    let subax = mk_domain_ax subax_ in
    match skind stmt with
    | Assign
        (
          {ekind = PyIndexSubscript(
               {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}))},
               index
             )},
          exp'
        ) ->

      panic "assign to list not done via __setitem__"

    | Assign
        (
          {ekind = PySliceSubscript(
               {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as alist))},
               estart,
               eend,
               estep
             )},
          {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)} as alist2))}
        ) ->

      let ll2 = mk_ll alist2 in
      let lv2 = mk_lv alist2 in

      let ll = mk_ll alist in
      let lv = mk_lv alist in


      fold_slice_length_cases (fun acc (lle', gabs) ->
          debug "slice length %a in @ @[%a@]" Framework.Pp.pp_exp lle' SubAbstract.print (subax.get_abs gabs);
          let in_cond =
            mk_binop
              (mk_binop (mk_int 0) Lt lle')
              LAnd
              (mk_binop lle' Le ll)
          in
          let in_gabs = manager.exec (mk_stmt (Assume in_cond)) ctx gabs in
          let in_case =
            if SubAbstract.is_bottom (subax.get_abs in_gabs) then
              manager.bottom
            else
              let ll' = mk_binop (mk_binop ll Minus lle') Plus ll2 in
              let gabs = manager.exec (mk_assign ll ll') ctx in_gabs in
              manager.exec (mk_stmt (Expand (lv, lv2))) ctx gabs |>
              manager.join gabs
          in
            
          let empty_cond = mk_binop lle' Le (mk_int 0) in
          let empty_gabs = manager.exec (mk_stmt (Assume empty_cond)) ctx gabs in
          let empty_case =
            if SubAbstract.is_bottom (subax.get_abs empty_gabs) then
              manager.bottom
            else
              empty_gabs
          in

          let sup_cond = mk_binop lle' Gt ll in
          let sup_gabs = manager.exec (mk_stmt (Assume sup_cond)) ctx gabs in
          let sup_case =
            if SubAbstract.is_bottom (subax.get_abs sup_gabs) then
              manager.bottom
            else
              let gabs = manager.exec (mk_assign ll ll2) ctx sup_gabs in
              manager.exec (mk_stmt (Expand (lv, lv2))) ctx gabs |>
              manager.join gabs
          in
          manager.join acc @@ manager.join in_case @@ manager.join empty_case sup_case
        ) manager.bottom estart eend estep ll manager ctx subax gabs |>
      stop


    | Assign
        (
          {ekind = PySliceSubscript(
               {ekind = Constant (Addr ({akind = U_instance({akind = B_class "list"}, _)}))},
               estart,
               eend,
               estep
             )},
          ({ekind = Constant (Addr aiter)})
        ) ->
      panic "assign to list slice not supported"

  
    | _ ->
      continue gabs

  let ask _ _ = Framework.Query.top

end

let setup () =
  register name (module Make)
