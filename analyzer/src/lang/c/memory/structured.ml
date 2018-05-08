(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** 
    An alternate abstraction of memory blocks as nested structs,
    unions and arrays.
    It is a higher-level view of memory than cell-based abstractions
    and follows more closely the C specification for conformant programs.
    However, it offers limited support for pointer arithmetic and no support
    for type-punning found in non-conformant programs.
    On the pro side, it can detect more non-portable uses of unions,
    array access, pointer arithmetic and pointer casts.
 *)

(* 
   Status:

 *)

open Bot
open Top
open Framework.Ast
open Framework.Pp
open Framework.Domains.Reduce_unify.Domain
open Framework.Manager
open Framework.Visitor
open Framework.Domains
open Framework.Alarm
open Framework.Flow
open Framework.Lattice
open Framework.Eval
open Framework.Exec
open Universal.Ast
open Ast
open Base
open Access_path
   

let name = "c.memory.structured"
let debug fmt = Debug.debug ~channel:name fmt

              
(*==========================================================================*)
(**                              {2 Cells}                                  *)
(*==========================================================================*)


type var_kind +=
  | V_access_path of ap
(**
   The structured block domain associates a cell to each access path.
   The access path must have scalar type.
 *)


let ap_of_var (v:var) : ap =
  match v.vkind with
  | V_orig -> V v, [], v.vtyp (* whole variable *)
  | V_access_path ap -> ap    (* already an access path *)
  | _ -> assert false
(** Variable to access path. *)

let base_of_var (v:var) : base =
  match v.vkind with
  | V_orig -> V v
  | V_access_path (b,_,_) -> b
  | _ -> assert false
(** Base of a variable. *)
  
let annotate_var_kind (v:var) : var =
  match v.vkind with
  | V_access_path ap -> v
  | _ -> { v with vkind = V_access_path (ap_of_var v); }
(** Ensure the variable is an access path. *)

let type_of_base : base -> typ = function
  | V v -> v.vtyp
  | A _ -> assert false
         


(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain(SubDomain: Framework.Domains.Stateful.DOMAIN) = struct


  (*========================*)
  (** {3 Lattice structure} *)
  (*========================*)

  
  module B = struct
    type t = base
    let compare = compare_base
    let print = pp_base
  end
  (** A base (variable or address). *)

  module T = MapExt.Make(B)

  type non_bot = Sel.t T.t
  type t = non_bot with_bot
  (** Remember the set of access path cells, partitionned by base and 
      organized as selector trees to simplify operations.
      Missing variables are assumed to be at top.
   *)


  let bottom : t =
    BOT

  let top : t =
    Nb T.empty

  let is_top (x:t) : bool =
    x = top

  let is_bottom (x:t) : bool =
    x = bottom

  let leq : t -> t -> bool =
    bot_included
      (T.for_all2zo
         (fun _ x -> true)
         (fun _ x -> false)
         (fun _ x y -> Sel.leq x y)
      )

  let join : t -> t -> t =
    (* union of access paths from both arguments *)
    bot_neutral2
      (fun a b ->
        T.fold2zo
          (fun k x acc -> acc)
          (fun k x acc -> T.add k x acc)
          (fun k x y acc ->
            (* remove the variable if it becomes top *)
            try T.add k (Sel.join x y) acc
            with Found_TOP -> T.remove k acc
          )
          a b a
      )

  let meet (a:t) (b:t) : t =
    (* access paths common in both arguments *)
    try
      bot_lift2
        (fun a b ->
          T.fold2zo
            (fun k x acc -> T.remove k acc)
            (fun k x acc -> acc)
            (fun k x y acc ->
              T.add k (Sel.meet x y) acc
            )
            a b a
        )
        a b
    with Found_BOT -> BOT


  let widening ctx = join


  let print : Format.formatter -> t -> unit =
    T.fprint MapExt.printer_default B.print Sel.print |>
    bot_fprint
                    

    
  (*=======================*)
  (** {3 Basic operations} *)
  (*=======================*)


  (* from expand.ml *)

  let local_manager =
    mk_local_manager (module SubDomain : Framework.Domains.Stateful.DOMAIN with type t = SubDomain.t)


  let local_exec ctx stmt s =
    let flow = set_domain_cur s local_manager local_manager.flow.bottom in
    let flow' = local_manager.exec ctx stmt flow in
    get_domain_cur local_manager flow'
    
  let sub_exec subman ctx stmt flow =
    match SubDomain.exec subman ctx stmt flow with
    | Some flow -> flow
    | None -> assert false



  let var_in_domain ctx range (v:var) (u:non_bot) (s:SubDomain.t)
      : non_bot * SubDomain.t * var list =
    (* ignore non-scalar *)
    if not (is_c_scalar_type v.vtyp) then u, s, [] else
    
    (* get, or create the access path set for base *)
    let base,sel,t = ap_of_var v in
    let has_base, block =
      try true, T.find base u
      with Not_found -> false, Sel.base (type_of_base base)
    in
    (* initial range, for numeric types *)
    let init =
      if is_c_pointer_type v.vtyp then None else
        let a, b = rangeof t in
        Some (mk_z_interval a b range)
    in
    (* translate access paths of v into a set of access path in u *)
    let block', sels' = Sel.add_sel block sel in
    let vs, s =
      List.fold_left
        (fun (vs,s) sel' ->
          (* if the acess path was not already in block before add_sel,
             also add it in the subdomain
           *)
          let v' = { v with vkind = V_access_path (base, sel', t) } in
          v'::vs,
          if has_base && Sel.contains_sel block sel' then s
          else
            match init with
            | Some init ->
               (* numeric cell *)
               let stmt = Universal.Ast.(mk_assume (mk_binop (mk_var v' range) O_eq init range) range) in
               local_exec ctx stmt s
            | None ->
               (* pointer cell *)
               s
        ) ([],s) sels'
    in
    T.add base block' u, s, vs
  (** Given an access path variable [v], return the list of variables it
      corresponds to in the environment [u].
      Takes care of adding the missing variables in the environment [u] 
      and the subdomain [s].
   *)

  let var_in_domain_bot ctx range (v:var) (u:t) (s:SubDomain.t) =
    match u with
    | BOT -> BOT, s, []
    | Nb u -> let u,s,v = var_in_domain ctx range v u s in Nb u, s, v

    

  let var_of_ap (ap:ap) : var =
    let _,_,typ = ap in
    { vname = string_of_ap ap;
      vuid = 0;
      vtyp = typ;
      vkind = V_access_path ap;
    }
  (**
     Variable corresponding to an access path.
     TODO: cache this, as string building can be expensive.
   *)



    
  (*=========================*)
  (** {3 Transfer functions} *)
  (*=========================*)


    
  let init man subman ctx prog flow =
    ctx, flow


  let rec exec man subman ctx stmt flow =
    let range = stmt.srange in
    debug "%a@\nexec %a" pp_range range pp_stmt stmt;

    match skind stmt with
    | S_c_local_declaration({vkind = V_orig} as v, init) ->
       (* TODO: init *)
       let v' = annotate_var_kind v in
       debug "exec S_c_local_declaration %a" pp_var v;
       None

    | S_rename_var(v, v') ->
       assert false
      
    | S_remove_var ({vkind = V_orig | V_access_path _} as v) when is_c_int_type v.vtyp ->
       let u = get_domain_cur man flow in
       let v' = annotate_var_kind v in
       let (base,sel,_) as ap = ap_of_var v in
       debug "exec S_remove_var %a -> %a" pp_var v pp_ap ap;
       let u' =
         bot_lift1
           (fun x ->
             let b = T.find base x in
             match Sel.remove_sel b sel with
             | Nb bb -> T.add base bb x
             | BOT -> T.remove base x
           )
         u
       in
       let stmt' = {stmt with skind = Universal.Ast.S_remove_var(v')} in
       let flow = set_domain_cur u' man flow in
       SubDomain.exec subman ctx stmt' flow |>
       oflow_compose (add_flow_mergers [mk_remove_var v' stmt.srange])
       
    | S_assign(lval, rval, mode) when is_c_scalar_type lval.etyp ->
      eval_list [rval; lval] (man.eval ctx) flow |>
      eval_to_orexec (fun el flow ->
          match el with
          | [rval; {ekind = E_var ({vkind = V_access_path ap} as v)} as lval] ->
             let base,sel,typ = ap in
             let stmt' = {stmt with skind = S_assign(lval, rval, mode)} in
             debug "exec S_assign %a" Framework.Pp.pp_stmt stmt';
             SubDomain.exec subman ctx stmt' flow |>
               oflow_compose (
                   fun flow ->
                   if is_c_int_type typ then
                     let rmin, rmax = rangeof typ in
                     let cond = range_cond lval rmin rmax (erange lval) in
                     let stmt'' = (mk_assume cond (tag_range range "assume range")) in
                     debug "exec S_assign assume %a" Framework.Pp.pp_stmt stmt'';
                     match SubDomain.exec subman ctx stmt'' flow with
                     | Some flow -> flow
                     | None -> assert false
                   else
                     flow
                 ) |>
               oflow_compose (add_flow_mergers [mk_remove_var v stmt.srange])
                         
          | _ -> None
        ) (man.exec ctx) man.flow
      
      
    | _ -> None

    
  and eval man subman ctx exp flow =
    let range = exp.erange in
    debug "%a@\neval %a" pp_range range pp_expr exp;

    match ekind exp with
    | E_var ({vkind = V_orig} as v) when is_c_type v.vtyp ->
       debug "eval E_var %a" pp_var v;
       let u = get_domain_cur man flow in
       let s = get_domain_cur subman flow in
       let u', s', vs = var_in_domain_bot ctx range v u s in
       let flow'' = set_domain_cur u' man flow |>
                    set_domain_cur s' subman
       in
       List.fold_left
         (fun acc v ->
           re_eval_singleton (man.eval ctx) (Some (mk_var v exp.erange), flow'', []) |>
           add_eval_mergers [] |>
           oeval_join acc
         )
         (oeval_singleton (None, flow'', [])) vs

    | E_c_deref(p) ->
       debug "eval E_c_deref %a" pp_expr p;
       man.eval ctx (Pointer.mk_c_resolve_pointer p exp.erange) flow |>
         eval_compose
           (fun e flow ->
             match ekind e with
             | E_c_access_path pt ->
                eval_ptr man subman ctx exp flow pt
             | _ -> assert false
           )
      
    | E_c_arrow_access(p, i, f) ->
       debug "eval E_c_arrow_access %a" pp_expr p;
       let record = match remove_typedef (under_pointer_type p.etyp) with
         | T_c_record r -> r
         | _ -> assert false
       in
       man.eval ctx (Pointer.mk_c_resolve_pointer p exp.erange) flow |>
       eval_compose
         (fun e flow ->
           match ekind e with
           | E_c_access_path pt ->
              let pt = Ptr.append_field record i pt in
              eval_ptr man subman ctx exp flow pt
           | _ -> assert false
         )
       
    | E_c_array_subscript(arr, idx) ->
       (* a[b] => *(a+b) *)
       debug "array subscript to deref";
       let exp' = {exp with ekind = E_c_deref(mk_binop arr math_plus idx exp.erange ~etyp: arr.etyp)} in
       re_eval_singleton (man.eval ctx) (Some exp', flow, []) |>
       add_eval_mergers []
       
    | E_c_member_access(r, idx, f) ->
       (* a.b => (&a)->b *)
       let exp' = {exp with ekind = E_c_arrow_access(mk_c_address_of r r.erange, idx, f)} in
       re_eval_singleton (man.eval ctx) (Some exp', flow, []) |>
       add_eval_mergers []

    | _ -> None


  and eval_ptr man subman ctx exp flow (pt:Ptr.t) =
    let aps = match Ptr.enumerate pt with
      | Nt x -> x
      | TOP -> [E_ap_invalid] (* map top pointer to invalid *)
    in
    List.fold_left
      (fun acc ape ->
        debug "eval E_c_deref E_c_access_path %a" pp_apexpr ape;
        (match ape with
         | E_ap_ap ap ->
            let exp' = {exp with ekind = E_var (var_of_ap ap)} in
            oeval_singleton (Some (exp', []), flow, []) |>
              oeval_join acc
            
         | E_ap_base base ->
            (* TODO: unknown location inside a base
               -> returns top of typ + alamr ?
             *)
            assert false
           
         | E_ap_fun fn ->
            oeval_singleton (Some ({exp with ekind = E_c_function fn}), flow, []) |>
              add_eval_mergers []
           
         | E_ap_null  ->
            let flow =
              man.flow.add (Alarms.TNullDeref exp.erange) (man.flow.get TCur flow) flow |>
                man.flow.set TCur man.env.Framework.Lattice.bottom
            in
            oeval_singleton (None, flow, [])
            
         | E_ap_invalid ->
            let flow =
              man.flow.add (Alarms.TInvalidDeref exp.erange) (man.flow.get TCur flow) flow |>
                man.flow.set TCur man.env.Framework.Lattice.bottom
            in
            oeval_singleton (None, flow, [])
            
         | _ -> assert false
        )
        |> oeval_join acc
      )
      (oeval_singleton (None, flow, [])) aps


  let add_all_sels ctx range (b:base) (p:Sel.t) (u:non_bot) (s:SubDomain.t) =
    List.fold_left
      (fun (u,s) (sel,typ)->
        let v = var_of_ap (b,sel,typ) in
        let u,s,_ = var_in_domain ctx range v u s in
        u,s
      )
      (u,s) (Sel.enumerate p) 
  (** Add variables for every path in p from base. *)
        
  let unify ctx (u, s) (u', s') =
    match u,u' with
    | BOT,_ | _,BOT -> (u,s),(u',s')
    | Nb uu, Nb uu' ->
       let range = mk_fresh_range () in
       let (uu,ss),(uu',ss') =
         T.fold2zo
           (fun b p ((u,s),(u',s')) ->
             (u,s), add_all_sels ctx range b p u' s'
           )
           (fun b p ((u,s),(u',s')) ->
             add_all_sels ctx range b p u s, (u',s')
           )
           (fun b p1 p2 ((u,s),(u',s')) ->
             add_all_sels ctx range b p2 u s,
             add_all_sels ctx range b p1 u' s'
           )
           uu uu' ((uu,s),(uu',s'))
       in
       (Nb uu,ss), (Nb uu',ss')
  (** Add missing access paths so that both arguments have the same set of
      access paths and the subdomains have the same set of variables.
   *)
       
    
  let ask : type r. ('a, t) manager -> ('a, SubDomain.t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a Framework.Flow.flow -> r option =
    fun man subman ctx query flow ->
    match query with

    | QExtractVarAccessPath ({vkind = V_access_path ap} as v) ->
       let pt = Ptr.of_ap ap in
       debug "ask QExtractVarAccessPath %a -> %a" pp_var v Ptr.print pt;
       Some pt

    | QExtractVarAccessPath v ->
       debug "ask QExtractVarAccessPath %a" pp_var v;
       assert false
      
    | _ -> None
    

  let refine man subman ctx channel flow = None

end
                                                            
                                                            


let setup () =
  register_domain name (module Domain);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_access_path c1, V_access_path c2 -> compare_ap c1 c2
      | _ -> next vk1 vk2
    );
  register_pp_vkind (fun next fmt vk ->
      match vk with
      | V_access_path c -> pp_ap fmt c
      | _ -> next fmt vk
    )
