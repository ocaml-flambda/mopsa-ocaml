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
open Bot

module V = Value.Make(State)(StrSigmaAlgebra)

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

  type _ domain += D_c_cell_expand : t domain
  let id = D_c_cell_expand

  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_cell_expand -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  (*==========================================================================*)
  (**                           {2 Interface}                                 *)
  (*==========================================================================*)

  let exec_interface = {
    export = [Zone.Z_universal];
    import = [Zone.Z_universal]
  }

  let eval_interface = {
    export = [Zone.Z_universal, Zone.Z_universal];
    import = [Zone.Z_universal, Zone.Z_universal]
  }
  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  let top = Nb VMap.empty
  let bottom = BOT

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
                   let pres, u_num, v_num = V.widen man x u_num y v_num in
                   (VMap.add k pres res, u_num, v_num)
               ) u' v' (VMap.empty, u_num, v_num)
           in
           (Nb res, u_num, v_num)
        ) u v

  (*==========================================================================*)
  (**                           {2 Transformers }                             *)
  (*==========================================================================*)

  let exec zone stmt man flow =
    None

  let eval zone exp man flow =
    None

  let init prog man flow =
    None

  let ask _ _ _ =
    None
end
