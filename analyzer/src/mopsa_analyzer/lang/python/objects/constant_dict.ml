(** A constant dictionary abstraction. Useful to be precise when passing named arguments to functions *)

open Mopsa
open Sig.Abstraction.Domain
open Ast
open Addr
open Universal.Ast
open Data_container_utils


type ('a, _) query += Q_py_dict_items : expr -> ('a, (expr * expr) list) query

let () = register_query {
    join = (let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_py_dict_items _ -> (a @ b)
                | _ -> next.pool_join query a b in
            f
           );
    meet = (let f : type a r. query_pool -> (a, r) query -> r -> r -> r =
              fun next query a b ->
                match query with
                | Q_py_dict_items _ -> assert false
                | _ -> next.pool_meet query a b in
            f)
  }


let compare_py_object (obj1: py_object) (obj2: py_object) : int =
  Compare.compose
    [
      (fun () -> compare_addr (fst obj1) (fst obj2));
      (fun () -> (OptionExt.compare compare_expr) (snd obj1) (snd obj2));
    ]

module Domain =
struct

  module Dicts =
    struct
      module DictSet = Framework.Lattices.Powerset.Make(struct
                           type t = (py_object * py_object) list
                           let compare = Compare.list (Compare.pair compare_py_object compare_py_object)
                           let print = unformat (fun fmt l -> Format.fprintf fmt "%a" (Format.pp_print_list (fun fmt (k, v) -> Format.fprintf fmt "%a ~> %a" Pp.pp_py_object k Pp.pp_py_object v)) l)
                         end)

      include DictSet

      let max_size = 1
      let bound (x:t) : t =
        match x with
        | Nt s when Set.cardinal s <= max_size -> x
        | _ -> TOP

      let join a1 a2 = DictSet.join a1 a2 |> bound

      let add v t =
        add v t |> bound
    end

  module DictMap = Framework.Lattices.Partial_map.Make(Addr)(Dicts)
  module DictAddrs = Framework.Lattices.Powerset.Make(Addr)

  type t = {dict: DictMap.t; addrs: DictAddrs.t}

  let empty = {dict = DictMap.empty; addrs = DictAddrs.empty}

  let widen ctx a1 a2 = {dict = DictMap.widen ctx a1.dict a2.dict; addrs = DictAddrs.join a1.addrs a2.addrs}

  let lift_binop fd fa a1 a2 = {dict = fd a1.dict a2.dict; addrs = fa a1.addrs a2.addrs}

  let join = lift_binop DictMap.join DictAddrs.join
  let meet = lift_binop DictMap.meet DictAddrs.meet

  let subset a1 a2 = DictMap.subset a1.dict a2.dict && DictAddrs.subset a1.addrs a2.addrs
  let top = {dict = DictMap.top; addrs = DictAddrs.top}
  let bottom = {dict = DictMap.bottom; addrs = DictAddrs.bottom}
  let is_bottom c = DictMap.is_bottom c.dict && DictAddrs.is_bottom c.addrs

  let find a c = DictMap.find a c.dict
  let add addr dict c = {dict = DictMap.add addr dict c.dict;
                         addrs = Dicts.fold (fun dict addrs ->
                                     List.fold_left (fun addrs ((ka, _), (va, _)) ->
                                         DictAddrs.add ka addrs |> DictAddrs.add va) addrs dict) dict c.addrs}
  let rename a a' c = {dict=DictMap.rename a a' c.dict; addrs = c.addrs}
  let remove a c = {dict=DictMap.remove a c.dict; addrs=c.addrs}

  include Framework.Core.Id.GenDomainId(struct
              type nonrec t = t
              let name = "python.objects.constant_dict"
            end)

  let checks = []

  let init _ man flow =
    set_env T_cur empty man flow

  let eval exp man flow =
    (***** FIXME: expressions are evaluated, but then the resulting python object (esp the addr) may be modified without this domain taking it into account... *****)
    let range = erange exp in
    match ekind exp with
    | E_py_dict (ks, vs) ->
       let addr_dict = mk_alloc_addr Dict.A_py_dict range in
       man.eval addr_dict flow >>$ (fun eaddr_dict flow ->
         let addr_dict = Addr.from_expr eaddr_dict in
         (* evaluate all keys and values and add them to dict *)
         Cases.bind_list ks man.eval flow >>$ fun eks flow ->
         Cases.bind_list vs man.eval flow >>$ fun evs flow ->
         let cur = get_env T_cur man flow in
         let oks = List.map object_of_expr eks in
         let ovs = List.map object_of_expr evs in
         let ncur = add addr_dict (Dicts.singleton (List.combine oks ovs)) cur in
         debug "dict ok, ncur %a" (format DictMap.print) ncur.dict;
         set_env T_cur ncur man flow |>
         Eval.singleton (mk_py_object (addr_dict, None) range)
       )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__new__", _))}, _)}, cls :: _, []) ->
      Utils.new_wrapper man range flow "dict" cls
        ~fthennew:(man.eval (mk_expr ~etyp:(T_py None) (E_py_dict ([],[])) range))


    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__getitem__" as f, _))}, _)}, args, []) ->
       Utils.check_instances ~arguments_after_check:1 f man flow range args ["dict"]
         (fun args flow ->
           let dict, key = match args with [d; k] -> d, k | _ -> assert false in
           let dict_addr = addr_of_object @@ object_of_expr dict in
           let key_obj = object_of_expr key in
           let cur = get_env T_cur man flow in
           let ds = find dict_addr cur in
           (* ok as ds should have <= 1 element. Otherwise need to fold *)
           let dict = Dicts.choose ds in

           match List.find_opt (fun (ko, _) -> compare_py_object key_obj ko = 0) dict with
           | None -> man.exec (Utils.mk_builtin_raise "KeyError" range) flow >>% Eval.empty
           | Some (_, obj) -> Flow.add_safe_check Alarms.CHK_PY_KEYERROR range flow |> Eval.singleton (mk_py_object obj range)
         )
       |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__len__" as f, _))}, _)}, args, []) ->
      Utils.check_instances f man flow range args
        ["dict"]
        (fun args flow ->
          let dict = List.hd args in
          let dict_addr = addr_of_object @@ object_of_expr dict in
          let cur = get_env T_cur man flow in
          let ds = find dict_addr cur in
          let dict = Dicts.choose ds in
          man.eval (mk_int ~typ:(T_py None) (List.length dict) range) flow
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict.__iter__" as f, _))}, _)}, args, []) ->
         Utils.check_instances f man flow range args ["dict"]
        (fun args flow ->
           let dict = List.hd args in
           let a = mk_alloc_addr (Py_list.A_py_iterator ("dict_keyiterator", None)) range in
           man.eval   a flow >>$
 (fun addr_it flow ->
                 let addr_it = Addr.from_expr addr_it in
                 man.exec   (mk_assign (mk_var (Py_list.Domain.itseq_of_addr addr_it) range) dict range) flow >>%
                   Eval.singleton (mk_py_object (addr_it, None) range)
             )
        )
      |> OptionExt.return

    | E_py_call({ekind = E_py_object ({addr_kind = A_py_function (F_builtin ("dict_keyiterator.__next__" as f, _))}, _)}, args, []) ->
       (* we return everything at once. For more precision we may want to have a counter related to the iterator *)
       (* FIXME: in its current state, this should be handled by the reduced product with the smashing dictionnary *)
      Utils.check_instances f man flow range args ["dict_keyiterator"]
        (fun args flow ->
          man.eval   (mk_var (Py_list.Domain.itseq_of_eobj @@ List.hd args) range) flow >>$
            (fun dict_eobj flow ->
              let dict_addr = addr_of_object @@ object_of_expr dict_eobj in
              let cur = get_env T_cur man flow in
              let ds = find dict_addr cur in
              (* ok as ds should have <= 1 element. Otherwise need to fold *)
              let dict = Dicts.choose ds in

              let els =
                let flow = Flow.add_safe_check Alarms.CHK_PY_STOPITERATION range flow in
                List.map (fun (_, obj) -> Eval.singleton (mk_py_object obj range) flow) dict in

              let stopiteration = man.exec (Utils.mk_builtin_raise "StopIteration" range) flow >>% Eval.empty in
              Eval.join_list ~empty:(fun () -> Eval.empty flow) (stopiteration :: els)
            )
        )
      |> OptionExt.return

    | E_py_annot {ekind = E_py_index_subscript ({ekind = E_py_object ({addr_kind = A_py_class (C_annot c, _)}, _)}, i) } when get_orig_vname c.py_cls_a_var = "Dict" ->
      let addr_dict = mk_alloc_addr Dict.A_py_dict range in
      let ty_key, ty_value = match ekind i with
        | E_py_tuple (a::b::[]) -> a, b
        | _ -> assert false in
      man.eval   addr_dict flow >>$
 (fun eaddr_dict flow ->
          let addr_dict = Addr.from_expr eaddr_dict in
          let keys_var, values_var = Dict.Domain.var_of_addr addr_dict in
          let stmts = mk_block (
              List.map (fun (var, annot) ->
                  mk_stmt (S_py_annot
                             (mk_var ~mode:(Some WEAK) var range,
                              mk_expr ~etyp:(T_py None) (E_py_annot annot) range)
                          ) range
                ) [(keys_var, ty_key); (values_var, ty_value)]) range in
          man.exec   stmts flow >>%
          Eval.singleton (mk_py_object (addr_dict, None) range)
        )
      |> OptionExt.return

    | _ -> None

  let exec stmt man flow =
    match skind stmt with
    | S_add {ekind = E_addr ({addr_kind = Dict.A_py_dict} as addr_dict, _)} ->
       let cur = get_env T_cur man flow in
       set_env T_cur (add addr_dict Dicts.empty cur) man flow |> Post.return |> OptionExt.return

    | S_rename ({ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)}, {ekind = E_addr (a', _)})
    | S_fold ({ekind = E_addr (a', _)}, [{ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)}]) ->
       let cur = get_env T_cur man flow in
       set_env T_cur (rename a a' cur) man flow |> Post.return |> OptionExt.return

    | S_expand ({ekind = E_addr ({addr_kind = Dict.A_py_dict} as addr_dict, _)}, addrs) ->
       let cur = get_env T_cur man flow in
       let d = find addr_dict cur in
       set_env T_cur
         (List.fold_left (fun cur ea ->
           let a = Addr.from_expr ea in
           add a d cur
            ) cur addrs) man flow
       |> Post.return |> OptionExt.return

    | S_remove {ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)}
    | S_invalidate {ekind = E_addr ({addr_kind = Dict.A_py_dict} as a, _)} ->
       let cur = get_env T_cur man flow in
       set_env T_cur (remove a cur) man flow |> Post.return |> OptionExt.return

    | _ -> None

  let print_expr _ _ _ _ = ()
  let print_state printer a =
    pprint ~path:[Key "Constant dictionaries"] printer (pbox DictMap.print a.dict)

  let merge _ _ _ = assert false

  let ask : type r. ('a, r) query -> ('a, t) man -> 'a flow -> r option =
    fun query man flow ->
    match query with
    | Q_py_dict_items dict ->
       let range = erange dict in
       let dict_addr = addr_of_object @@ object_of_expr dict in
       let cur = get_env T_cur man flow in
       let ds = find dict_addr cur in
       (* ok as ds should have <= 1 element. Otherwise need to fold *)
       let dict = Dicts.choose ds in
       OptionExt.return @@ List.map (fun (k, v) -> mk_py_object k range, mk_py_object v range) dict

    | _ -> None
end

let () = register_standard_domain(module Domain)
