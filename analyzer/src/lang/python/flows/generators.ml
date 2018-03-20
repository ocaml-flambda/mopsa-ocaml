open Framework.Domain
open Framework.Ast
open Framework.Manager
open Universal.Ast
open Framework.Accessor
open Framework.Context
open Universal.Ast
open Ast
open Addr
open XAst
open Utils

let name = "python.flows.generators"

module Make(SubLayer : Framework.Layer.S) = struct

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt

  module SubAbstract = SubLayer.Abstract
  
  module YieldMap =
    Framework.Lattices.PartialMap.Make
      (struct
        type t = Framework.Ast.range
        let compare = Framework.Ast.compare_range
        let print fmt range =
          Framework.Pp.pp_range fmt range
      end)
      (SubAbstract)

  module DeadMap = Universal.Utils.AddrPartialMap(SubAbstract)
  
  module GenInfo =
  struct
    type t = {
      init: SubAbstract.t;
      yields: YieldMap.t;
      dead: DeadMap.t;
    }

    let bottom = {
      init = SubAbstract.bottom;
      yields = YieldMap.bottom;
      dead = DeadMap.bottom;
    }

    let top = {
      init = SubAbstract.top;
      yields = YieldMap.top;
      dead = DeadMap.top;
    }

    let init = {
      init = SubAbstract.bottom;
      yields = YieldMap.bottom;
      dead = DeadMap.bottom;
    }

    let is_bottom abs =
      SubAbstract.is_bottom abs.init &&
      YieldMap.is_bottom abs.yields &&
      DeadMap.is_bottom abs.dead

    let is_top abs =
      SubAbstract.is_top abs.init &&
      YieldMap.is_top abs.yields &&
      DeadMap.is_top abs.dead

    
    let leq abs1 abs2 =
      SubAbstract.leq abs1.init abs2.init &&
      YieldMap.leq abs1.yields abs2.yields &&
      DeadMap.leq abs1.dead abs2.dead
      
    let join abs1 abs2 = {
      init = SubAbstract.join abs1.init abs2.init;
      yields = YieldMap.join abs1.yields abs2.yields;
      dead = DeadMap.join abs1.dead abs2.dead;
    }

    let meet abs1 abs2 = {
      init = SubAbstract.meet abs1.init abs2.init;
      yields = YieldMap.meet abs1.yields abs2.yields;
      dead = DeadMap.meet abs1.dead abs2.dead;
    }

    let widening abs1 abs2 = {
      init = SubAbstract.widening abs1.init abs2.init;
      yields = YieldMap.widening abs1.yields abs2.yields;
      dead = DeadMap.widening abs1.dead abs2.dead;
    }

    let print fmt abs =
      Format.fprintf fmt "init: @[%a@]@\nyields:@\n@[<h 2>  %a@]@\ndead:@\n@[<h 2>  %a@]"
        SubAbstract.print abs.init
        YieldMap.print abs.yields
        DeadMap.print abs.dead
  end


  module GenMap = Universal.Utils.AddrPartialMap(GenInfo)
      
  module Abstract =
  struct
    include GenMap

    let is_bottom _ = false
    
    let print fmt abs =
      Format.fprintf fmt "generators:@ @[  %a@]" GenMap.print abs
  end

  module NextMap =
    Framework.Lattices.PartialMap.Make
      (struct
        type t = Framework.Ast.range * Framework.Ast.exp
        let compare (r1, _) (r2, _) = Framework.Ast.compare_range r1 r2
        let print fmt (range, exp) =
          Framework.Pp.pp_range fmt range
      end)
      (SubAbstract)

  module Flow =
  struct
    type t = {
      input_yields: YieldMap.t;
      next_yields: NextMap.t;
    }

    let bottom = {
      input_yields = YieldMap.bottom;
      next_yields = NextMap.bottom;
    }

    let top = {
      input_yields = YieldMap.top;
      next_yields = NextMap.top;
    }

    let init = {
      input_yields = YieldMap.empty;
      next_yields = NextMap.empty;
    }

    let is_bottom abs = false
    (* YieldMap.is_bottom abs.input_yields &&
     * NextMap.is_bottom abs.next_yields *)

    let is_top abs = false

    let leq abs1 abs2 = 
      YieldMap.leq abs1.input_yields abs2.input_yields &&
      NextMap.leq abs1.next_yields abs2.next_yields

    let join abs1 abs2 = {
      input_yields = YieldMap.join abs1.input_yields abs2.input_yields;
      next_yields = NextMap.join abs1.next_yields abs2.next_yields;
    }

    let meet abs1 abs2 = {
      input_yields = YieldMap.meet abs1.input_yields abs2.input_yields;
      next_yields = NextMap.meet abs1.next_yields abs2.next_yields;
    }

    let widening abs1 abs2 = {
      input_yields = YieldMap.widening abs1.input_yields abs2.input_yields;
      next_yields = NextMap.widening abs1.next_yields abs2.next_yields;
    }

    let print fmt abs =
      Format.fprintf fmt "gen yields:@   @[%a@]@\ngen nexts:@   @[%a@]"
        YieldMap.print abs.input_yields
        NextMap.print abs.next_yields
  end

  open Flow
  open GenInfo

  let has_abstract = true
  let has_flow = true
  
  type t = Abstract.t * Flow.t
  type st = SubLayer.t
  
  let local_counter_var = {
    orgname = "%counter";
    unname = "%counter";
    vtyp = TInt;
  }

  let local_counter = mk_var local_counter_var
      
  let outside_counter addr =
    mk_exp (PyAttribute (mk_addr addr, "$counter")) ~etyp:TInt


  let create_generator exp func args manager ctx ax subax gabs =
    debug "creation of a generator";
    let addr, gabs =
      let cls = Builtins.builtin_address "generator" in
      let param = Some (Generator func) in
      alloc_instance cls ~param exp.erange manager ctx gabs
    in
    (* FIXME: 
       1. __next__ is actually a read-only attribute. 
             So we should find another way to say that this assignment is safe.
       2. process arguments
    *)

    let gabs =manager.exec (mk_assign (outside_counter addr) mk_zero) ctx gabs in

    let gmap = ax.get_abs gabs in

    let init =
      subax.set_abs gabs SubAbstract.init |>
      (fun gabs ->
         List.fold_left (fun acc (arg, param) ->
             manager.exec (mk_assign (mk_var param) arg) ctx acc
           ) gabs (List.combine args func.pyfunc_parameters)
      ) |>
      manager.exec (mk_assign local_counter mk_zero) ctx |>
      subax.get_abs
    in

    let gmap = gmap |> GenMap.add addr {
        init;
        yields = YieldMap.empty;
        dead = DeadMap.bottom;
      }
    in

    let gabs = ax.set_abs gabs gmap in

    [(mk_addr addr ~erange:exp.erange, (gabs, []))]

    
  let rec exec
      stmt (manager: 'a Framework.Manager.t) ctx
      (ax_: ('a, t) Framework.Accessor.t)
      (subax_: ('a, SubLayer.t) Framework.Accessor.t) (gabs: 'a) : 'a Framework.Domain.action
    =
    let ax = Framework.Domain.mk_domain_ax ax_ and subax = Framework.Domain.mk_domain_ax subax_ in
    match skind stmt with
    | Expression({ekind = PyYield e}) ->
      debug "Eval yield";
      let range = stmt.srange in
      let gabs = manager.exec
          (mk_assign
             local_counter (mk_binop local_counter Plus mk_one)
          ) ctx gabs 
      in
      
      let flow = ax.get_flow gabs in
      debug "flow =@ @[%a@]" Flow.print flow;
      let old_next_abs = NextMap.find (range, e) flow.next_yields in

      let new_next_abs = subax.get_abs gabs in

      let next_abs = SubAbstract.join old_next_abs new_next_abs in
      (* let next_abs = new_next_abs in *)

      let new_flow = { flow with next_yields = NextMap.add (range, e) next_abs flow.next_yields } in
      debug "new flow =@ @[%a@]" Flow.print new_flow;

      let new_abs = YieldMap.find range flow.input_yields in

      let gabs = subax.set_abs gabs new_abs in
      let gabs = ax.set_flow gabs new_flow in

      stop gabs

    | KillGenerator(addr, exn) ->
      debug "kill %a with %a" Universal.Pp.pp_addr addr Framework.Pp.pp_exp exn;
      let abs = subax.get_abs gabs in
      if SubAbstract.is_bottom abs then
        stop gabs
      else
        eval_exec (fun exn gabs ->
            match ekind exn with
            | Constant (Addr exn) ->
              let gmap = ax.get_abs gabs in
              let ginfo = GenMap.find addr gmap in

              let dead_abs =
                subax.get_abs gabs |>
                SubAbstract.join (DeadMap.find exn ginfo.dead)
              in
              let dead = DeadMap.add exn dead_abs ginfo.dead in

              debug "dead computed@\n@[%a@]" DeadMap.print dead;

              let gmap = gmap |> GenMap.add addr {
                  init = SubAbstract.bottom;
                  yields = YieldMap.bottom;
                  dead;
                }
              in
              ax.set_abs gabs gmap 

            | _ ->
              assert false
                
          ) exn manager ctx gabs |>
        stop
    

    | _ ->
      continue gabs


  let eval exp manager ctx ax_ subax_ gabs =
    let ax = mk_domain_ax ax_ and subax = mk_domain_ax subax_ in
    match ekind exp with
    | PyCall(
        {ekind = Constant(Addr ({akind = B_function "generator.__iter__"}))},
        [{ekind = Constant (Addr ({akind = U_instance(_, Some (Generator func))} as addr))}],
        []
      ) ->
      [mk_addr addr, (gabs, [])]


    | PyCall({ekind = Constant(Addr ({akind = U_function func}))}, args, [])
      when func.pyfunc_is_generator = true
      ->
      create_generator exp func args manager ctx ax subax gabs

    | PyCall({ekind = Constant(Addr {akind = U_method({akind = U_function func}, self)})}, args, [])
    | PyCall({ekind = PyLazyMethod({akind = U_function func}, {ekind = Constant (Addr self)})}, args, [])
      when func.pyfunc_is_generator = true
      ->
      create_generator exp func (mk_addr self :: args) manager ctx ax subax gabs

    | PyCall(
        {ekind = Constant(Addr ({akind = B_function "generator.__next__"}))},
        [{ekind = Constant (Addr ({akind = U_instance(_, Some (Generator func))} as addr))}],
        []
      ) ->

      let abs = subax.get_abs gabs in
      let gmap = ax.get_abs gabs in
      let ginfo = GenMap.find addr gmap in
      let old_flow = ax.get_flow gabs in

      let dead_case =
        let abs = subax.get_abs gabs in
        let dead_abs =
          DeadMap.fold (fun _ dead_abs acc ->
              SubAbstract.meet abs dead_abs |>
              subax.set_abs gabs |>
              manager.exec
                (mk_stmt (Assume (mk_binop (outside_counter addr) Eq local_counter)))
                ctx |>
              subax.get_abs |>
              SubAbstract.join acc
            ) ginfo.dead SubAbstract.bottom
        in
        debug "dead_abs =@[<h 2>  %a@]" SubAbstract.print dead_abs;
        if SubAbstract.is_bottom @@ dead_abs then
          []
        else
          let gabs = subax.set_abs gabs dead_abs |>
                     manager.exec
                       (mk_stmt ~srange:exp.erange (PyRaise (Some (mk_addr ~erange:exp.erange (Builtins.builtin_address "StopIteration")))))
                       ctx
          in
          [exp, (gabs, [])]

      in


      let active_case =
        debug "exec_next on abs =@ %a" SubAbstract.print abs;
        let yields = YieldMap.map (fun abs' ->
            debug "meet with abs' =@ %a" SubAbstract.print abs';
            let abs'' = SubAbstract.meet abs abs' in
            debug "meet res =@ %a" SubAbstract.print abs'';
            let res = manager.exec (mk_stmt
                                      (Assume (mk_binop (outside_counter addr) Eq local_counter)))
                ctx (subax.set_abs gabs abs'')
            in
            debug "assume res =@ %a" manager.print res;
            subax.get_abs res
          ) ginfo.yields in

        let init = SubAbstract.meet ginfo.init (subax.get_abs gabs) in

        if SubAbstract.is_bottom init && YieldMap.is_bottom yields then
          []
        else

          let ginfo = {
            init = SubAbstract.bottom;
            yields;
            dead = DeadMap.bottom;
          } in

          let gmap = GenMap.add addr ginfo gmap in

          let flow = {
            input_yields = yields;
            next_yields = NextMap.empty;
          } in

          let gabs = ax.set_abs gabs gmap in
          let gabs = ax.set_flow gabs flow in
          let gabs = subax.set_abs gabs init in

          let body = Framework.Visitor.map_stmt
              (fun x -> x)
              (fun x -> x)
              (fun stmt ->
                 match skind stmt with
                 | Return _ -> { stmt with skind = PyRaise (Some (mk_addr ~erange:stmt.srange (Builtins.builtin_address "StopIteration")))}
                 | _ -> stmt
              ) func.pyfunc_body
          in

          let tmp1 = mktmp () in
          let exn = mk_var tmp1 in
          
          let gabs = manager.exec
              (mk_stmt (PyTry(
                   body,
                   [mk_except
                      (Some (mk_addr @@ Builtins.builtin_address "BaseException"))
                      (Some tmp1)
                      (mk_stmt (KillGenerator (addr, exn)))
                   ],
                   mk_stmt nop,
                   mk_stmt nop
                 )))
              ctx gabs
          in

          debug "next executed@\nabs =@[ %a@]" manager.print gabs;

          let gmap = ax.get_abs gabs in
          let ginfo = GenMap.find addr gmap in
          let flow = ax.get_flow gabs in

          let exn_case =
            DeadMap.fold (fun exn abs acc ->
                if SubAbstract.is_bottom abs then
                  acc
                else
                  let gabs = subax.set_abs gabs abs in
                  debug "exception %a raised" Universal.Pp.pp_addr exn;

                  let outside_gabs =
                    manager.exec
                      (mk_stmt (Block (
                           (
                             List.map (fun v -> mk_stmt (RemoveAll v)) (local_counter_var :: func.pyfunc_locals @ func.pyfunc_parameters)
                           )
                         ))) ctx gabs
                  in

                  let local_gabs = manager.exec (mk_stmt (ProjectVars (local_counter_var :: func.pyfunc_locals @ func.pyfunc_parameters))) ctx gabs in

                  
                  let gmap' = gmap |>
                              GenMap.add addr {
                                init = SubAbstract.bottom;
                                yields = YieldMap.empty;
                                dead = DeadMap.singleton exn (subax.get_abs local_gabs);
                              }
                  in

                  let gabs = ax.set_abs outside_gabs gmap' in
                  let gabs = ax.set_flow gabs old_flow in

                  debug "pre exn gabs =@[ %a@]" manager.print gabs;

                  let gabs = manager.exec
                      (mk_stmt (PyRaise (Some (mk_addr exn))))
                      ctx gabs
                  in

                  (exp, (gabs, [mk_stmt (RemoveAll tmp1)])) :: acc
                  
              ) ginfo.dead []
              
          in

          let next_case =

            NextMap.fold (fun (range, e) abs acc ->
                if SubAbstract.is_bottom abs then
                  acc
                else
                  let gabs = subax.set_abs gabs abs in
                  
                  debug "next %a on@\n@[<h 2>  %a@]" Framework.Pp.pp_exp e SubAbstract.print abs;

                  let tmp2 = mktmp () in
                  let target = mk_var tmp2 in
                  
                  let gabs = eval_exec (fun e gabs ->
                      manager.exec (mk_assign target e) ctx gabs
                    ) e manager ctx gabs
                  in

                  debug "post assign gabs = %a" manager.print gabs;

                  let outside_gabs =
                    manager.exec
                      (mk_stmt (Block (
                           (mk_assign (outside_counter addr) local_counter) ::
                           (
                             List.map (fun v -> mk_stmt (RemoveAll v)) (local_counter_var :: func.pyfunc_locals @ func.pyfunc_parameters)
                           )
                         ))) ctx gabs
                  in

                  let local_gabs = manager.exec (mk_stmt (ProjectVars (local_counter_var :: func.pyfunc_locals @ func.pyfunc_parameters))) ctx gabs in
                  let yields = YieldMap.singleton range (subax.get_abs local_gabs) in

                  debug "yields computed@\n%a" YieldMap.print yields;

                  let gmap' = gmap |> GenMap.add addr {init = SubAbstract.bottom; yields; dead = DeadMap.bottom} in

                  let gabs = ax.set_abs outside_gabs gmap' in
                  let gabs = ax.set_flow gabs old_flow in

                  debug "new gabs =@[ %a@]" manager.print gabs;

                  (target, (gabs, [mk_stmt (RemoveAll tmp1); mk_stmt (RemoveAll tmp2)])) :: acc

              ) flow.next_yields [] |>
            re_eval_list manager ctx 
          in

          exn_case @ next_case
      in

      dead_case @ active_case

    | PyGeneratorComprehension _ ->
      panic "Generator expressions not supproted"

    | _ -> []

  let ask _ _ = Framework.Query.top

end

let setup () =
  register name (module Make : F);
  ()
