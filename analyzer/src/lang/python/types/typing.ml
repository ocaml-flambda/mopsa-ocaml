(* TODO: move S_assume and eval of not into t_bool domain? *)
open Mopsa
open Ast
open Addr
open Data_model.Attribute
open MapExt
open Objects.Function
open Universal.Ast


module Domain =
struct

  type polytype =
    | Bot | Top

    | Class of class_address * py_object list (* class * mro *)
    | Function of function_address
    | Module of module_address

    | Instance of pytypeinst

    (* | Union of addr list *)
    | Typevar of int

  and pytypeinst = {classn: polytype (* TODO: polytype or addr? *); uattrs: addr StringMap.t; oattrs: addr StringMap.t}

  type addr_kind +=
    | A_py_instance (*of  class_address*)

  let () =
    Format.(register_addr {
        print = (fun default fmt a ->
            match a with
            | A_py_instance (*c -> fprintf fmt "Inst{%a}" pp_addr_kind (A_py_class (c, []))*)
              -> fprintf fmt "inst"
            | _ -> default fmt a);
        compare = (fun default a1 a2 ->
            match a1, a2 with
            (* | A_py_instance c1, A_py_instance c2 ->
             *   compare_addr_kind (A_py_class (c1, [])) (A_py_class (c2, [])) *)
            | _ -> default a1 a2);})

  let rec compare_polytype t1 t2 =
    match t1, t2 with
    | Class (ca, objs), Class (ca', objs') ->
      compare_addr_kind (A_py_class (ca, objs)) (A_py_class (ca', objs'))
    | Function f1, Function f2 ->
      compare_addr_kind (A_py_function f1) (A_py_function f2)
    | Module m1, Module m2 ->
      compare_addr_kind (A_py_module m1) (A_py_module m2)
    | Instance i1, Instance i2 ->
      Compare.compose [
        (* (fun () -> compare_addr i1.classn i2.classn); *)
        (fun () -> compare_polytype i1.classn i2.classn);
        (fun () -> StringMap.compare compare_addr i1.uattrs i2.uattrs);
        (fun () -> StringMap.compare compare_addr i1.oattrs i2.oattrs)
      ]
    (* | Union l1, Union l2 ->
     *   ListExt.compare compare_addr l1 l2 *)
    | Typevar a1, Typevar a2 ->
      Pervasives.compare a1 a2
    | _ -> Pervasives.compare t1 t2

  let map_printer = MapExtSig.{ print_empty = "∅";
                                print_begin = "{";
                                print_arrow = ":";
                                print_sep = ";";
                                print_end = "}"; }

  let rec pp_polytype fmt t =
    match t with
    | Bot -> Format.fprintf fmt "⊥"
    | Top -> Format.fprintf fmt "⊤"
    | Class (C_user c, _) -> Format.fprintf fmt "Class {%a}" pp_var c.py_cls_var
    | Class (C_builtin c, _) | Class (C_unsupported c, _) -> Format.fprintf fmt "Class[%s]" c
    | Function (F_user f) -> Format.fprintf fmt "Function {%a}" pp_var f.py_func_var
    | Function (F_builtin f) | Function (F_unsupported f) -> Format.fprintf fmt "Function[%s]" f
    | Module (M_user (m, _) | M_builtin(m)) -> Format.fprintf fmt "Module[%s]" m

    | Instance {classn; uattrs; oattrs} ->
     if StringMap.is_empty uattrs && StringMap.is_empty oattrs then
       Format.fprintf fmt "Instance[%a]" pp_polytype classn
     else
       let pp_attrs = (StringMap.fprint map_printer Format.pp_print_string pp_addr) in
       Format.fprintf fmt "Instance[%a, %a, %a]" pp_polytype classn pp_attrs uattrs pp_attrs oattrs

    (* | Union l -> Format.fprintf fmt "Union[%a]" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ") pp_addr) l *)

    | Typevar t -> Format.fprintf fmt "α(%d)" t

  module Polytypeset = Framework.Lattices.Powerset.Make
      (struct
        type t = polytype
        let compare = compare_polytype
        let print = pp_polytype
      end)

  module TMap = Framework.Lattices.Partial_map.Make
      (struct
        type t = addr
        let compare = compare_addr
        let print = pp_addr
      end)
      (Polytypeset)

  type typevar = int

  module TypeVarMap = Framework.Lattices.Partial_map.Make
      (struct
        type t = typevar
        let compare = compare
        let print fmt d = Format.fprintf fmt "%d@\n" d
      end)
      (Polytypeset)

  type t = {abs_heap: TMap.t;
            typevar_env: TypeVarMap.t}

  type _ domain += D_python_typing : t domain

  let id = D_python_typing
  let name = "python.types.typing"
  let identify : type a. a domain -> (t, a) eq option = function
    | D_python_typing -> Some Eq
    | _ -> None

  type _ Framework.Query.query +=
    | Q_types : Framework.Ast.expr -> Typingdomain.polytype Framework.Query.query

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface = {export = [any_zone]; import = [Zone.Z_py_types]}
  let eval_interface = {export = [Zone.Z_py, Zone.Z_py_addr]; import = [Universal.Zone.Z_u_heap, Z_any]}

  let join _ = Exceptions.panic "todo join "

  let polytype_leq (pty, env) (pty', env') =
    (* FIXME *)
    compare_polytype pty pty' = 0

  let subset d d' =
    TMap.fold (fun absaddr ptys acc ->
        let ptys' = TMap.find absaddr d'.abs_heap in
        (* acc && polytype_leq (pty, d.typevar_env) (pty', d'.typevar_env) *)
        acc && Polytypeset.for_all (fun pty -> Polytypeset.exists (fun pty' -> polytype_leq (pty, d.typevar_env) (pty', d'.typevar_env)) ptys') ptys
      )
      d.abs_heap true

  let meet _ _ =  Exceptions.panic "todo meet "
  let widen _ _  = Exceptions.panic "todo widen"
  let top = {abs_heap = TMap.top; typevar_env = TypeVarMap.top}
  let bottom = (* FIXME *) {abs_heap = TMap.bottom; typevar_env = TypeVarMap.bottom}
  let is_bottom {abs_heap; typevar_env} = TMap.is_bottom abs_heap && TypeVarMap.is_bottom typevar_env

  let pp_absheap = TMap.print

  let pp_typevar_env = TypeVarMap.print

  let print fmt {abs_heap; typevar_env} =
    Format.fprintf fmt "abs_heap = %a@\ntypevar_env = %a@\n"
      pp_absheap abs_heap
      pp_typevar_env typevar_env

  let init progr man flow =
    Flow.set_domain_env T_cur {abs_heap = TMap.empty; typevar_env = TypeVarMap.empty} man flow |> Flow.without_callbacks |> OptionExt.return

  let exec zone stmt man flow =
    debug "exec %a@\n" pp_stmt stmt;
    match skind stmt with
    | S_rename ({ekind = E_addr a}, {ekind = E_addr a'}) ->
      (* TODO: le faire autrepart (addr_env), /!\ zones *)
      let cur = Flow.get_domain_cur man flow in
      let abs_heap = TMap.rename a a' cur.abs_heap in
      debug "abs_heap = %a@\n" pp_absheap abs_heap;
      Flow.set_domain_cur {cur with abs_heap} man flow |> Post.return

    | _ -> None

  let eval zs exp man flow =
    debug "eval %a@\n" pp_expr exp;
    let range = erange exp in
    match ekind exp with
    | E_addr addr ->
      let cur = Flow.get_domain_cur man flow in
      Polytypeset.fold (fun pty acc ->
          let abs_heap = TMap.add addr (Polytypeset.singleton pty) cur.abs_heap in
          let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
          match pty with
            | Class (c, b) ->
              Eval.singleton (mk_py_object ({addr with addr_kind = (A_py_class (c, b))}, exp) range) flow :: acc

            | _ -> Exceptions.panic_at range "%a@\n" pp_polytype pty) (TMap.find addr cur.abs_heap) []
      |> Eval.join_list |> OptionExt.return
    (* | E_constant (C_top T_bool)
     *   | E_constant (C_bool _ ) ->
     *    return_id_of_type man flow range (Typingdomain.builtin_inst "bool") |> OptionExt.return
     *
     * | E_constant (C_top T_int)
     *   | E_constant (C_int _) ->
     *    return_id_of_type man flow range (Typingdomain.builtin_inst "int") |> OptionExt.return
     *
     * | E_constant (C_top (T_float _))
     *   | E_constant (C_float _) ->
     *    return_id_of_type man flow range (Typingdomain.builtin_inst "float") |> OptionExt.return
     **)
    | E_constant (C_top T_string)
    | E_constant (C_string _) ->
      (* allocate addr, and map this addr to inst "str" *)
      let str_cls, str_mro =
        let obj = find_builtin "str" in
        match kind_of_object obj with
        | A_py_class (c, b) -> c, b
        | _  -> assert false in
      man.eval (mk_alloc_addr (A_py_instance (*str_cls*)) range) flow |>
      Eval.bind (fun eaddr flow ->
          let addr = match ekind eaddr with
            | E_addr a -> a
            | _ -> assert false in
          let cur = Flow.get_domain_cur man flow in
          let str_inst = (Polytypeset.singleton (Instance {classn=Class (str_cls, str_mro); uattrs=StringMap.empty; oattrs=StringMap.empty})) in
          let abs_heap = TMap.add addr str_inst cur.abs_heap in
          let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
          Eval.singleton eaddr flow
          (* Eval.singleton (mk_py_object (addr, exp) range) flow *)
        )
      |> OptionExt.return

    (*
     * | E_py_bytes _ ->
     *    return_id_of_type man flow range (Typingdomain.builtin_inst "bytes") |> OptionExt.return
     *
     * | E_constant C_py_not_implemented ->
     *    let builtin_notimpl = Typingdomain.builtin_inst "NotImplementedType" in
     *    return_id_of_type man flow range builtin_notimpl |> OptionExt.return
     *
     * | E_constant C_py_none ->
     *    let builtin_none = Typingdomain.builtin_inst "NoneType" in
     *    return_id_of_type man flow range builtin_none |> OptionExt.return *)

    (* Je pense pas avoir besoin de ça finalement *)
    (* | E_py_object ({addr_kind = A_py_class (c, b)} as addr, expr) ->
     *   let cur = Flow.get_domain_cur man flow in
     *   let abs_heap = TMap.add addr (Polytypeset.singleton (Class (c, b))) cur.abs_heap in
     *   let flow = Flow.set_domain_cur {cur with abs_heap} man flow in
     *   Eval.singleton (mk_addr addr range) flow |> OptionExt.return *)

    (* begin match akind with
     * | A_py_method (func, self) ->
     *    man.eval (mk_py_object ({addr_kind = akind; addr_uid = (-1); addr_mode=STRONG}, mk_py_empty range) range) flow
     * | _ ->
     *    let addr = {addr_kind = akind; addr_uid=(-1);addr_mode=STRONG} in
     *    Eval.singleton (mk_addr addr range) flow
     * end
     * |> OptionExt.return *)

    | E_py_ll_hasattr(e, attr) ->
      Exceptions.panic "E_py_ll_hasattr"

    | E_py_ll_getattr(e, attr) ->
      Exceptions.panic "E_py_ll_getattr"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_class (C_builtin "type", _)}, _)}, [arg], []) ->
      Exceptions.panic "type()"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "issubclass")}, _)}, [cls; cls'], []) ->
      Exceptions.panic "issubclass"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "isinstance")}, _)}, [obj; attr], []) ->
      Exceptions.panic "isinstance"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__new__")}, _)}, args, []) ->
      Exceptions.panic "object.__new__"

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin "object.__init__")}, _)}, args, []) ->
      Exceptions.panic "object.__init__"

    | _ ->
      debug "Warning: no eval for %a" pp_expr exp;
      None


  let is_type_query : type r. r Framework.Query.query -> bool =
    function
    | Q_types _ -> true
    | _ -> false

  let ask : type r. r Framework.Query.query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_types t ->
      Exceptions.panic "query on %a@\n" pp_expr t
    | _ -> None

end

let () = register_domain (module Domain)
