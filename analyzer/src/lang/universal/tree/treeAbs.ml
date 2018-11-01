(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(**
   An abstraction for tree like structures
*)

open Framework.Essentials
open Framework.Visitor
open Ast
open Bot

module V = Value.VString

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)

module Domain : Framework.Domains.Stacked.S = struct

  (*==========================================================================*)
  (**               {2 Domain definition and identification}                  *)
  (*==========================================================================*)

  module VMap = Map.Make(Var)
  let fold2 f u v acc =
    let module D = Set.Make(Var) in
    let defset =
      D.empty
      |> VMap.fold (fun k _ -> D.add k) u
      |> VMap.fold (fun k _ -> D.add k) v
    in
    D.fold (fun k acc ->
        f k (VMap.find_opt k u) (VMap.find_opt k v) acc
      ) defset acc


  type t = V.t VMap.t with_bot
  let print fmt (u: t) = match u with
    | BOT -> Format.fprintf fmt "⊥"
    | Nb x -> Format.fprintf fmt "%a"
                (ToolBox.print_map
                   Var.print
                   V.print
                   VMap.bindings
                ) x

  let name = "universal.tree.treeabs"

  type _ domain += D_universal_tree : t domain
  let id = D_universal_tree

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_universal_tree -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (*==========================================================================*)
  (**                           {2 Interface}                                 *)
  (*==========================================================================*)

  let exec_interface = {
    export = [Zone.Z_u_tree];
    import = [Zone.Z_u_num]
  }

  let eval_interface = {
    export = [Zone.Z_u, Zone.Z_u_tree];
    import = [Zone.Z_u_num, Zone.Z_u_num;
              Zone.Z_u, Zone.Z_u_tree
             ]
  }
  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  let top = Nb VMap.empty
  let bottom = BOT

  let go_to_bottom range man cur flow =
    match cur with
    | BOT -> flow
    | Nb cur ->
      VMap.fold (fun _ u flow -> V.remove_all_nums range man u flow) cur flow

  let assign0 range man cur v x flow =
    match cur with
    | BOT -> (BOT, flow)
    | Nb cur ->
      begin
        match VMap.find_opt v cur with
        | Some x' -> let flow' = V.remove_all_nums range man x' flow in Nb (VMap.add v x cur), flow'
        | None -> Nb (VMap.add v x cur), flow
      end
  let is_top = bot_apply (fun _ -> VMap.is_empty) false

  let is_bottom = bot_apply (fun _ _ -> false) true

  let subset (man: ('b, 'b) man) ((u, u_num): t * 'b flow) ((v, v_num): t * 'b flow)
      : bool * 'b flow * 'b flow
    =
    let exception NotLeq in
    bot_apply2 (true, u_num, v_num) (false, u_num, v_num)
      (fun u' v' ->
         try
           let u_num, v_num = fold2 (fun k a b (u_num, v_num) ->
               match a, b with
               | None, None -> assert false
               | Some x, None -> (u_num, v_num)
               | None, Some y -> raise NotLeq
               | Some x, Some y ->
                 let b, u_num, v_num = V.subset man x u_num y v_num in
                 if b then
                   (u_num, v_num)
                 else raise NotLeq
             ) u' v' (u_num, v_num)
           in (true, u_num, v_num)
         with
         | NotLeq -> (false, u_num, v_num)
      ) u v

  let join (annot: 'a annot) (man: ('b, 'b) man) ((u, u_num): t * 'b flow) ((v, v_num): t * 'b flow)
      : t * 'b flow * 'b flow
    =
    bot_apply2 (v, u_num, v_num) (u, u_num, v_num)
      (fun u' v' ->
         let res, u_num, v_num =
           fold2 (fun k a b (res, u_num, v_num) ->
               match a, b with
               | None, None -> assert false
               | Some x, None -> (res, u_num, v_num)
               | None, Some y -> (res, u_num, v_num)
               | Some x, Some y ->
                 let pres, u_num, v_num = V.join man x u_num y v_num in
                 (VMap.add k pres res, u_num, v_num)
             ) u' v' (VMap.empty, u_num, v_num)
         in
         (Nb res, u_num, v_num)
      ) u v

  let meet (annot: 'a annot) (man: ('b, 'b) man) ((u, u_num): t * 'b flow) ((v, v_num): t * 'b flow)
      : t * 'b flow * 'b flow
    =
    bot_apply2 (BOT, u_num, v_num) (BOT, u_num, v_num)
      (fun u' v' ->
         let res, u_num, v_num =
           fold2 (fun k a b (res, u_num, v_num) ->
               match a, b with
               | None, None -> assert false
               | Some x, None -> (VMap.add k x res, u_num, v_num)
               | None, Some y -> (VMap.add k y res, u_num, v_num)
               | Some x, Some y ->
                 let pres, u_num, v_num = V.meet man x u_num y v_num in
                 (VMap.add k pres res, u_num, v_num)
             ) u' v' (VMap.empty, u_num, v_num)
         in Nb res, u_num, v_num
      ) u v

  let widen (annot: 'a annot) (man: ('b, 'b) man) ((u, u_num): t * 'b flow) ((v, v_num): t * 'b flow)
      : t * bool * 'b flow * 'b flow
      =
      let w, prop, u_num, v_num = bot_apply2 (v, true, u_num, v_num) (u, true, u_num, v_num)
        (fun u' v' ->
           let res, prop, u_num, v_num =
             fold2 (fun k a b (res, flag, u_num, v_num) ->
                 match a, b with
                 | None, None -> assert false
                 | Some x, None -> (res, flag, u_num, v_num)
                 | None, Some y -> (res, flag, u_num, v_num)
                 | Some x, Some y ->
                   let pres, flag', u_num, v_num = V.widen man x u_num y v_num in
                   (VMap.add k pres res, flag && flag', u_num, v_num)
               ) u' v' (VMap.empty, true, u_num, v_num)
           in
           (Nb res, prop, u_num, v_num)
        ) u v
      in
      (* FIXME: check stability flag *)
      w, prop, u_num, v_num

  (*==========================================================================*)
  (**                           {2 Transformers}                              *)
  (*==========================================================================*)

  let assign range (man: ('a, t) man) x t mode (flow: 'a flow) : 'a flow =
    let u = Flow.get_domain_cur man flow in
    let u, flow =
      match u with
      | BOT -> BOT, flow
      | Nb abs ->
        begin
          match mode with
          | STRONG -> (* Nb (VMap.add x t abs), flow *)
            assign0 range man u x t flow
          | WEAK ->
            begin
              match VMap.find_opt x abs with
              | None -> Nb abs, flow
              | Some t' ->
                let t'', flow = V.join_same_num man t t' flow in
                assign0 range man u x t flow
                (* Nb (VMap.add x t'' abs), flow *)
            end
        end
    in
    Flow.set_domain_cur u man flow

  let exec
      (zone: Framework.Zone.zone)
      (stmt: stmt)
      (man: ('a, t) man)
      (flow: 'a flow)
    : 'a post option
    =
    let () = debug "I am asked" in
    match skind stmt with
    | S_assign({ekind = E_var(v, mode); etyp = T_tree}, e2) ->
      let range = srange stmt in
      man.eval e2 flow
      |> (Post.bind man (fun expr flow ->
          match ekind expr with
          | TreeAst.E_tree_set t ->
            begin
              assign range man v t mode flow
              |> Post.of_flow
            end
          | _ -> Debug.fail "tree not evaluated correctly"))
      |> OptionExt.return
    | S_assume({ekind = E_call ({ekind = E_function (Builtin {name = "is_symbol"})}, [{ekind = E_var(v, _)}])}) ->
      let range = srange stmt in
      begin
        let curdom = Flow.get_domain_cur man flow in
        match curdom with
        | BOT -> Flow.bottom (Flow.get_all_annot flow) |> Post.of_flow |> OptionExt.return
        | Nb cur ->
          begin
            let curstate = Flow.get_domain_cur man flow in
            match VMap.find_opt v cur with
            | Some tree_v ->
              let u, flow = V.filter_symbol range man tree_v flow in
              (if V.is_bottom range man u flow then
                 let flow = go_to_bottom range man curstate flow in
                 Flow.set_domain_cur (BOT) man flow
               else
                 let dom, flow = assign0 range man curdom v u flow in
                 Flow.set_domain_cur dom man flow)
              |> Post.of_flow |> OptionExt.return
            | None -> None
          end
      end
    | S_assume({ekind = E_unop(O_log_not ,{ekind = E_call ({ekind = E_function (Builtin {name = "is_symbol"})}, [{ekind = E_var(v, _)}])})}) ->
      let range = srange stmt in
      begin
        let curstate = Flow.get_domain_cur man flow in
        match curstate with
        | BOT -> Flow.bottom (Flow.get_all_annot flow) |> Post.of_flow |> OptionExt.return
        | Nb cur ->
          begin
            match VMap.find_opt v cur with
            | Some tree_v ->
              let u, flow = V.filter_not_symbol range man tree_v flow in
              (if V.is_bottom range man u flow then
                 let flow = go_to_bottom range man curstate flow in
                 Flow.set_domain_cur (BOT) man flow
               else
                 let dom, flow = assign0 range man curstate v u flow in
                 Flow.set_domain_cur dom man flow)
              |> Post.of_flow |> OptionExt.return
            | None -> None
          end
      end
    | S_assume({ekind = E_call ({ekind = E_function (Builtin {name = "is_symbol"})}, [t])}) ->
      let range = srange stmt in
      begin
        match Flow.get_domain_cur man flow with
        | BOT -> Flow.bottom (Flow.get_all_annot flow) |> Post.of_flow |> OptionExt.return
        | Nb cur ->
          begin
            let curstate = Flow.get_domain_cur man flow in
            let exception NP in
              try
                man.eval t flow
                |> Post.bind man (fun t flowin -> match ekind t with
                    | TreeAst.E_tree_set t ->
                      let u, flow = V.filter_symbol range man t flowin in
                      let flowrep =
                        if V.is_bottom range man u flow then
                          let flow = go_to_bottom range man curstate flow in
                          Flow.set_domain_cur (BOT) man flow
                        else
                          flowin
                      in
                      flowrep |> Post.of_flow
                    | _ -> raise NP
                  )  |> OptionExt.return
              with
              | NP -> None
          end
      end

    | S_assume({ekind = E_unop( O_log_not, {ekind = E_call ({ekind = E_function (Builtin {name = "is_symbol"})}, [t])})}) ->
      let range = srange stmt in
      begin
        match Flow.get_domain_cur man flow with
        | BOT -> Flow.bottom (Flow.get_all_annot flow) |> Post.of_flow |> OptionExt.return
        | Nb cur ->
          begin
            let curstate = Flow.get_domain_cur man flow in
            let exception NP in
              try
                man.eval t flow
                |> Post.bind man (fun t flowin -> match ekind t with
                    | TreeAst.E_tree_set t ->
                      let u, flow = V.filter_not_symbol range man t flowin in
                      let flowrep =
                        if V.is_bottom range man u flow then
                          let flow = go_to_bottom range man curstate flow in
                          Flow.set_domain_cur (BOT) man flow
                        else
                          flowin
                      in
                      flowrep |> Post.of_flow
                    | _ -> raise NP
                  )  |> OptionExt.return
              with
              | NP -> None
          end
      end
    | _ -> None

  let rec z_fold f a b acc =
    let open Z in
    if a > b then
      acc
    else
      z_fold f (a + one) b (f a acc)

  let eval zone exp (man: ('a, t) man) (flow: 'a flow) =
    let () = debug "asked: %a" pp_expr exp in
    match ekind exp, etyp exp with
    | E_call ({ekind = E_function (Builtin {name = "subtree"})}, [t; i]), _ ->
      begin
        let exception Nt in
        let range = erange exp in
        try
          let evl = man.eval ~zone:(Zone.Z_u, Zone.Z_u_tree) t flow in
          Eval.bind (fun t flow -> match ekind t with
              | TreeAst.E_tree_set t ->
                let q = man.ask (Numeric.Values.Intervals.Value.Q_interval i) flow in
                let (l, r) = Numeric.Values.Intervals.Value.bounds q in
                z_fold (fun z acc ->
                    let i = Z.to_int z in
                    Eval.join acc (let a, flow = V.read_i range man t i flow in
                                   Eval.singleton (mk_expr ~etyp:(etyp exp) (TreeAst.E_tree_set a) range) flow)
                  ) l r (Eval.empty)
              | _ -> raise Nt
            ) evl |> OptionExt.return
        with
        | Nt -> None
      end
    | E_tree (TC_int exp), _ ->
      let range = erange exp in
      let t, flow = V.build_tree_from_expr range man exp flow in
      Eval.singleton (mk_expr ~etyp:(etyp exp) (TreeAst.E_tree_set t) range) flow
      |> OptionExt.return
    | E_tree (TC_symbol(s, l)), _ ->
      begin
        let range = erange exp in
        let exception NotAllTS in
        try
          let el = Eval.eval_list l
              (fun expr flow ->
                 man.eval expr flow |>
                 Eval.bind (fun expr flow ->
                     match ekind expr with
                     | TreeAst.E_tree_set expr -> Eval.singleton expr flow
                     | _ -> (raise NotAllTS)
                   )
              ) flow
          in
          Eval.bind (fun el flow ->
              let open Strings.Value.Value in
              let s = man.ask (Q_string s) flow in
              let s = match s with
                | B | T -> raise NotAllTS
                | V s -> s
              in
              let v, flow = V.build_tree_from_symbol range man s el flow in
              Eval.singleton (mk_expr ~etyp:(etyp exp) (TreeAst.E_tree_set v) range) flow
            ) el
          |> OptionExt.return
        with
        | NotAllTS -> None
      end
    | E_var(v, _), T_tree ->
      let range = erange exp in
      let cur = Flow.get_domain_cur man flow in
      begin
        match cur with
        | BOT ->
          Eval.singleton
            (mk_expr ~etyp:(etyp exp)
               (TreeAst.E_tree_set (V.bottom V.SA.empty)) range)
            flow
        | Nb u ->
          begin
            match VMap.find_opt v u with
            | None ->
              Eval.singleton
                (mk_expr ~etyp:(etyp exp)
                   (TreeAst.E_tree_set (V.top V.SA.empty)) range)
                flow
            | Some x ->
              let x, y, flow' = V.copy range man x flow in
              let u = VMap.add v x u in
              Eval.singleton
                (mk_expr ~etyp:(etyp exp)
                   (TreeAst.E_tree_set y) range)
                (Flow.set_domain_cur (Nb u) man flow')
          end
      end
      |> OptionExt.return
    | _ -> None

  let init prog man flow =
    None

  let ask _ _ _ =
    None
end

let () =
  Framework.Domains.Stacked.register_domain (module Domain);
