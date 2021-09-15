open Mopsa
open Format

module Hook =
struct

  let name = "cpy.count_transitions"

  type entry = {
      mutable py2c: int;
      mutable c2py: int;
    }

  let table : (Callstack.callstack, entry) Hashtbl.t = Hashtbl.create 1

  let cut_callstack cs = if cs = [] then [] else [List.hd @@ List.rev cs]

  let incr_py2c cs =
    let cs = cut_callstack cs in
    let oc = Hashtbl.find_opt table cs in
    match oc with
    | None -> Hashtbl.add table cs {py2c = 1; c2py = 0}
    | Some c -> c.py2c <- c.py2c + 1

  let incr_c2py cs =
    let cs = cut_callstack cs in
    let oc = Hashtbl.find_opt table cs in
    match oc with
    | None -> Hashtbl.add table cs {py2c = 0; c2py = 1}
    | Some c -> c.c2py <- c.c2py + 1

  let init ctx = ()

  let is_cur_bottom man flow =
    man.lattice.is_bottom (Flow.get T_cur man.lattice flow)

  let on_before_eval route semantic exp man flow =
    if is_cur_bottom man flow then () else
    match ekind exp with
    | Python.Ast.E_py_call ({ekind = Python.Ast.E_py_object ({addr_kind = Python.Addr.A_py_c_function(name, uid, kind, oflags, self)}, _)}, args, kwargs) ->
       incr_py2c (Flow.get_callstack flow)
    | C.Ast.E_c_builtin_call ("PyObject_CallFunction", _)
    | C.Ast.E_c_builtin_call ("PyObject_CallMethod", _)
    | C.Ast.E_c_builtin_call ("PyObject_CallObject", _) ->
       incr_c2py (Flow.get_callstack flow)
    | _ -> ()

  let on_after_eval route semantic exp man flow evl = ()

  let on_before_exec route stmt man flow = ()

  let on_after_exec route stmt man flow post = ()

  let on_finish man flow =
    let mi, count, ma = Hashtbl.fold (fun cs entry (mi, count, ma) ->
        let py2c, c2py = entry.py2c, entry.c2py in
        (* Format.eprintf "%a: %d, %d@." Callstack.pp_callstack_short cs py2c c2py; *)
        (min mi py2c+c2py, count + py2c + c2py, max ma py2c+c2py)) table (1000000000, 0, -1000000000) in
    Format.eprintf "min, average, max number of crossings: %d %f %d@." mi (float_of_int count /. (float_of_int @@ Hashtbl.length table)) ma
end

let () =
  Core.Hook.register_stateless_hook (module Hook)
