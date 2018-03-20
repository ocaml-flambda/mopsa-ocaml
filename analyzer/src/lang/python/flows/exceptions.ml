open Framework.Domain
open Framework.Ast
open Framework.Manager
open Universal.Ast
open Framework.Alarm
open Universal.Ast
open Ast
open Addr
open XAst
open Utils

let name = "python.flows.exceptions"

module Make(SubLayer : Framework.Layer.S) = struct

  let name = name
  let debug fmt = Debug.debug ~channel:name fmt

  
  module SubAbstract = SubLayer.Abstract

  module RaiseMap = Universal.Utils.AddrPartialMap(SubAbstract)      
  module ExnSet = Framework.Lattices.BoundedSet.Make(struct type t = addr let compare = compare_addr let print = Universal.Pp.pp_addr end)

  module Abstract = Framework.Lattice.EmptyLattice
  module Flow =
  struct
    type t = {
      rmap : RaiseMap.t;
      handler_exn : ExnSet.t;
    }


    let bottom = {
      rmap = RaiseMap.bottom;
      handler_exn = ExnSet.bottom;
    }

    let top = {
      rmap = RaiseMap.top;
      handler_exn = ExnSet.top;
    }

    let init = {
      rmap = RaiseMap.empty;
      handler_exn = ExnSet.empty;
    }

    let is_bottom abs = false
    (* RaiseMap.is_bottom abs.rmap &&
     * ExnSet.is_bottom abs.handler_exn *)

    let is_top abs = false

    let leq abs1 abs2 = 
      RaiseMap.leq abs1.rmap abs2.rmap &&
      ExnSet.leq abs1.handler_exn abs2.handler_exn

    let join abs1 abs2 = {
      rmap = RaiseMap.join abs1.rmap abs2.rmap;
      handler_exn = ExnSet.join abs1.handler_exn abs2.handler_exn;
    }

    let meet abs1 abs2 = {
      rmap = RaiseMap.meet abs1.rmap abs2.rmap;
      handler_exn = ExnSet.meet abs1.handler_exn abs2.handler_exn;
    }

    let widening abs1 abs2 = {
      rmap = RaiseMap.widening abs1.rmap abs2.rmap;
      handler_exn = ExnSet.widening abs1.handler_exn abs2.handler_exn;
    }

    let print fmt abs =
      Format.fprintf fmt "exn:@   @[%a@]@\nexn handlers:@   @[%a@]"
        RaiseMap.print abs.rmap
        ExnSet.print abs.handler_exn

  end

  open Flow

  let has_abstract = false
  let has_flow = true
  
  type t = Abstract.t * Flow.t
  type st = SubLayer.t

  let exec_except excpt manager ctx ax subax gabs =
    debug "exec except";
    let flow = ax.get_flow gabs in
    match excpt.pyexcpt_type with
    | None ->
      let sabs_in, handler_exn = RaiseMap.fold (fun eaddr eabs (acc_in, acc_exn) ->
          SubAbstract.join eabs acc_in, ExnSet.add eaddr acc_exn
        ) flow.rmap (SubAbstract.bottom, ExnSet.empty)
      in
      let gabs_in = subax.set_abs gabs sabs_in in
      let gabs_in = ax.set_flow gabs_in {rmap = RaiseMap.bottom; handler_exn} in
      let gabs_in_exit = manager.exec excpt.pyexcpt_body ctx gabs_in in
      ax.set_flow gabs_in_exit { (ax.get_flow gabs_in_exit) with handler_exn = ExnSet.empty}

    | Some e ->

      let except_gabs =
        RaiseMap.fold (fun eaddr eabs acc ->
            subax.set_abs gabs eabs |>
            manager.join acc
        ) flow.rmap manager.bottom

      in

      eval_exec (fun e gabs0 ->
          match ekind e with
          | Constant (Addr cls) when Utils.issubclass cls (Builtins.builtin_address "BaseException")->
            let sabs_in, handler_exn = RaiseMap.fold (fun eaddr esabs (acc_in, acc_exn) ->
                let gabs = subax.set_abs gabs esabs in
                if not (Utils.isinstance eaddr cls) then
                  (acc_in, acc_exn)
                else
                  let gabs =
                    match excpt.pyexcpt_name with
                    | None -> gabs
                    | Some v ->
                      manager.exec
                        (mk_assign (mk_var v) (mk_addr eaddr))
                        ctx gabs
                  in
                  let acc_in' = SubAbstract.join acc_in (subax.get_abs gabs) in
          
                  let acc_exn' = ExnSet.add eaddr acc_exn in
                  (acc_in', acc_exn')
              ) flow.rmap (SubAbstract.bottom, ExnSet.empty)
            in
            let gabs_in = ax.set_flow gabs {rmap = RaiseMap.bottom; handler_exn } in
            let gabs_in = subax.set_abs gabs_in sabs_in in
            let gabs_in_exit = manager.exec excpt.pyexcpt_body ctx gabs_in in

            ax.set_flow gabs_in_exit { (ax.get_flow gabs_in_exit) with handler_exn = ExnSet.empty}

          | _ ->
            let gabs0 = ax.set_flow gabs0 {rmap = RaiseMap.bottom; handler_exn = ExnSet.empty } in
            manager.exec
              (mk_stmt (PyRaise (Some
                                   (mk_addr ~erange:e.erange (Builtins.builtin_address "TypeError"))
                                )))
              ctx gabs0

        ) e manager ctx except_gabs

    let escape_except excpt manager ctx ax subax gabs =
      debug "escape except";
      let flow = ax.get_flow gabs in
      match excpt.pyexcpt_type with
      | None ->
        manager.bottom

      | Some e ->
        let except_gabs =
          RaiseMap.fold (fun eaddr eabs acc ->
              subax.set_abs gabs eabs |>
              manager.join acc
            ) flow.rmap manager.bottom
            
        in
        eval_exec (fun e _ ->
            match ekind e with
            | Constant (Addr cls) when Utils.issubclass cls (Builtins.builtin_address "BaseException") ->
              let rmap_out = RaiseMap.fold (fun eaddr esabs acc ->
                  if Utils.isinstance eaddr cls then
                    acc
                  else
                    RaiseMap.add eaddr esabs acc
                ) flow.rmap RaiseMap.bottom
              in
              let gabs_out = ax.set_flow gabs {rmap = rmap_out; handler_exn = ExnSet.empty} in
              gabs_out

            | _ ->
              manager.bottom
          ) e manager ctx except_gabs


  let exec stmt manager ctx ax_ subax_ gabs =
    let ax = Framework.Domain.mk_domain_ax ax_ and subax = Framework.Domain.mk_domain_ax subax_ in
    match skind stmt with
    | PyTry(body, excepts, orelse, finally) ->
      let old_flow = ax.get_flow gabs in
      let gabs = ax.set_flow gabs Flow.bottom in

      let try_gabs = manager.exec body ctx gabs in

      let orelse_gabs = manager.exec orelse ctx (ax.set_flow try_gabs Flow.bottom) in
      
      let gabs_caught, gabs_uncaught = excepts |> List.fold_left (fun (acc_caught, acc_uncaught) excpt ->
          let caught = exec_except excpt manager ctx ax subax acc_uncaught in
          let uncaught = escape_except excpt manager ctx ax subax acc_uncaught in
          manager.join caught acc_caught, uncaught
        ) (manager.bottom, try_gabs) in

      let gabs =
        manager.exec finally ctx (manager.join orelse_gabs gabs_caught) |> (* FIXME : execute finally also on still uncaught envs *)
        manager.join gabs_uncaught
      in
      ax.set_flow gabs (Flow.join old_flow (ax.get_flow gabs)) |>
      stop

    | PyRaise None ->
      let flow = ax.get_flow gabs in
      let ok_case = ExnSet.may_fold (fun exn acc ->
          manager.exec (mk_stmt (PyRaise (Some (mk_addr ~erange:stmt.srange exn)))) ctx gabs |>
          manager.join acc
        ) flow.handler_exn manager.bottom
      in
      let error_case =
        if ExnSet.may_exists (fun x -> not (ExnSet.must_mem x flow.handler_exn)) flow.handler_exn then
          manager.exec (mk_stmt (PyRaise (Some (mk_addr ~erange:stmt.srange (Builtins.builtin_address "RuntimeError"))))) ctx gabs
        else
          manager.bottom
      in
      manager.join ok_case error_case |>
      stop

    | PyRaise(Some exp) ->
      if SubAbstract.is_bottom (subax.get_abs gabs) then
        stop gabs
      else
        eval_exec (fun exp gabs ->
            match ekind exp with
            | Constant(Addr ({akind = U_instance _} as addr)) ->
              if Utils.isinstance addr (Builtins.builtin_address "BaseException") then
                let sabs = subax.get_abs gabs in
                let flow = ax.get_flow gabs in
                let rabs' = SubAbstract.join sabs (RaiseMap.find addr flow.rmap) in
                let rmap = RaiseMap.add addr rabs' flow.rmap in
                let handler_exn = ExnSet.empty in
                let flow' = {rmap; handler_exn} in

                let gabs = ax.set_flow gabs flow' in
                subax.set_abs gabs SubAbstract.bottom
              else
                manager.exec
                  (mk_stmt (PyRaise (Some
                                       (mk_addr ~erange:stmt.srange (Builtins.builtin_address "TypeError"))
                                    )))
                  ctx gabs

            | Constant(Addr ({akind = U_class _ | B_class _} as cls))
              ->
              let addr, gabs = Addr.alloc_instance cls exp.erange manager ctx gabs in
              manager.exec {
                stmt with
                skind = PyRaise(Some (mk_addr ~erange:exp.erange addr))
              } ctx gabs 
          | _ ->
            manager.exec
              (mk_stmt (PyRaise (Some
                                   (mk_addr ~erange:stmt.srange (Builtins.builtin_address "TypeError"))
                                )))
              ctx gabs
        ) exp manager ctx gabs |> stop
      
    | RemoveAll v ->
      let flow = ax.get_flow gabs in
      let rmap = RaiseMap.fold (fun exn abs acc ->
          let gabs = subax.set_abs gabs abs in
          let gabs = SubLayer.exec stmt manager ctx subax_ gabs in
          let abs = subax.get_abs gabs in
          RaiseMap.add exn abs acc
        ) flow.rmap flow.rmap in
      ax.set_flow gabs {flow with rmap} |>
      continue

    | _ ->
      continue gabs
             
  let eval _ _ _ _ _ _ = []
  
  let ask : type a. a Framework.Query.t -> t -> (a * t) Framework.Query.reply =
    fun query (abs, flow) ->
      match query with
      | Framework.Alarm.QGetAlarms ->
        let alarms = RaiseMap.fold (fun exn abs acc ->
            let exn_name =
              match exn.akind with
              | U_instance({akind = U_class(cls, _)}, _) ->
                cls.pycls_var.unname
              | U_instance({akind = B_class(name)}, _) ->
                name
              | _ -> assert false
            in
            let alarm = {
              akind = Alarm.UncaughtException(exn_name);
              arange = exn.arange;
              alevel = Framework.Alarm.Unknown;
            }
            in
            alarm :: acc
          ) flow.rmap []
        in
        Framework.Query.singleton (alarms, (abs, flow))
      | _ ->
        Framework.Query.top
                  
end

let setup () =
  register name (module Make)
