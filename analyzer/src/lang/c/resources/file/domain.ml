(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Manager of the `FileDescriptor` resources *)

(* 
   UNIX file descriptors are associated to integer identifiers. These
   ids are generated in a particular way. As described in Single UNIX
   specification (Sec. 2.14), allocation should return the lowest
   number available.

   This abstract domain maintains information about previously
   allocated file descriptors. Since the ids 0, 1 and 2 are used
   implicitly by many library functions (printf, scanf, etc.), the domain 
   tries to be more precise for those cases.

   More particularly, it defines a window of precision (interval
   of integers) where allocated file descriptors have three
   states: Free, NotFree and MaybeFree. For the two last states, we
   add the set of allocated heap addresses.

   For the remaining values, the domains keeps a partial map from
   allocated addresses to an interval over-approximating file ids.

   Example
   =======

   Consider the following abstract state:

     0 -> NotFree { @0, @1 }
     1 -> Free
     _ -> {
       @2 -> [2, 2]
       @3 -> [4, 5]
     }

   represents the state where the file descriptor 0 is allocated to 
   resources  @0 or @1, and the descriptor 1 is not allocated. For the 
   remaining descriptors, the partial map gives the possible mappings.

   In order to improve precision an under-approximation of the support 
   of the  map (i.e. set of addresses) is used. It can forbid giving 
   some ids when allocating new resources.
*)


open Mopsa
open Universal.Ast
open Stubs.Ast
open Ast
open Zone
open Mm.Common.Points_to
module Itv = Universal.Numeric.Values.Intervals.Value
open Slot
open Table


(** {2 Abstract domain} *)
(** ******************* *)

module Domain =
struct

  (** Lattice definition *)
  (** ****************** *)

  type t = {
    first : slot list;
    others: table;
  }

  (** Precision window. The first file descriptors ∈ [0, window) are 
      abstracted more precisely. *)
  let window = 3

  let init_state = {
    first = List.init window (fun _ -> Slot.Free);
    others = Table.empty;
  }

  let bottom = {
    first = List.init window (fun _ -> Slot.Bot);
    others = Table.bottom;
  }

  let top = {
    first = [];
    others = Table.top;
  }

  let is_bottom (a:t) =
    List.exists Slot.is_bottom a.first ||
    Table.is_bottom a.others

  let subset a1 a2 =
    List.for_all2 Slot.subset a1.first a2.first &&
    Table.subset a2.others a1.others

  let join annot a1 a2 = {
    first = List.map2 Slot.join a1.first a2.first;
    others = Table.join annot a1.others a2.others;
  }

  let meet annot a1 a2 = {
    first = List.map2 Slot.meet a1.first a2.first;
    others = Table.meet annot a1.others a2.others;
  }

  let widen annot a1 a2 = {
    first = List.map2 Slot.join a1.first a2.first;
    others = Table.widen annot a1.others a2.others;
  }


  let print fmt a =
    let rec pp_first fmt l =
      let l' = List.mapi (fun i s -> (i, s)) l in
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
        (fun fmt (i, s) ->
           Format.fprintf fmt "%d ↦ @[<h>%a@]"
             i Slot.print s
        )
        fmt l'
    in
    Format.fprintf fmt "files: @[@[%a@]@\n@[%a@]@]@\n"
      pp_first a.first
      Table.print a.others


  (** Domain identification *)
  (** ===================== *)

  type _ domain += D_c_resources_file : t domain

  let id = D_c_resources_file
  let name = "c.resources.file"
  let identify : type a. a domain -> (t, a) eq option =
    function
    | D_c_resources_file -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt


  (** Zoning definition *)
  (** ================= *)

  let exec_interface = {
    export = [Z_c];
    import = []
  }

  let eval_interface = {
    export = [Z_c, Z_c_low_level];
    import = [
      Z_c, Z_c_points_to;
      Z_c, Universal.Zone.Z_u_num;
      Universal.Zone.Z_u_heap, Z_any
    ]
  }


  (** Initialization of environments *)
  (** ============================== *)

  let init prog man flow =
    Some (Flow.set_domain_env T_cur init_state man flow)



  (** {2 Insertion of new resources} *)
  (** ============================= *)

  (* Insert an address in the first slots *)
  let rec insert_first addr (a:t) : t * t =
    let rec iter slots =
      match slots with
      | [] -> [], []
      | hd :: tl ->
        let inserted_here, not_inserted_here = Slot.insert addr hd in
        if Slot.is_bottom not_inserted_here then
          inserted_here :: tl, not_inserted_here :: tl
        else
          let inserted_after, not_inserted_after = iter tl in
          inserted_here :: inserted_after, not_inserted_here :: inserted_after
    in
    let inserted, not_inserted = iter a.first in
    { a with first = inserted },
    { a with first = not_inserted }


  (** Insert an address in the remaining part of the table *)
  let insert_table addr a : t =
    {
      a with
      others = Table.insert addr window a.others
    }

  (** Insert an address in the table of file descriptors *)
  let insert addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    (* Try to insert the address in the first slots *)
    let inserted_in_first, no_place_in_first = insert_first addr a in

    let a' =
      if is_bottom no_place_in_first then
        inserted_in_first
      else
        (* In case there is no place in the first slots, insert in the remaining part of the table *)
        let inserted_in_table = insert_table addr no_place_in_first in
        let annot = Flow.get_all_annot flow in
        join annot inserted_in_first inserted_in_table
    in

    Flow.set_domain_env T_cur a' man flow

  
  (** {2 Evaluation of an address into a numeric value} *)
  (** ================================================= *)
  
  let eval_first_addr_to_int addr range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let rec iter i not_found_before slots =
      match slots with
      | [] -> [], []
      | hd :: tl ->
        let found_here, not_found_here = Slot.mem addr hd in
        let cases, not_found_after = iter (i + 1) (not_found_before @ [not_found_here]) tl in
        let cases' =
          if Slot.is_bottom found_here then cases
          else
            let exp = mk_int i range in
            let a' = { a with first = not_found_before @ [found_here] @ not_found_after } in
            let flow' = Flow.set_domain_env T_cur a' man flow in
            Eval.singleton exp flow' :: cases
        in
        cases', not_found_here :: not_found_after
    in
    let cases, not_found = iter 0 [] a.first in
    cases, { a with first = not_found }
    
  let bounds itv =
    if Itv.is_bounded itv then
      Itv.bounds itv
    else
      let _, max = rangeof s32 in
      Z.of_int window, max
    
  
  let eval_table_addr_to_int addr range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let itv = Table.find addr a.others in
    if Itv.is_bottom itv then
      []
    else
      let a, b = bounds itv in
      let exp = mk_z_interval a b range in
      [Eval.singleton exp flow]

  let eval_addr_to_int addr range man flow =
    let cases1, not_found = eval_first_addr_to_int addr range man flow in
    let flow' = Flow.set_domain_env T_cur not_found man flow in
    let cases2 = eval_table_addr_to_int addr range man flow' in
    Eval.join_list (cases1 @ cases2)
  

  (** {2 Evaluation of an interval into an address} *)
  (** ============================================= *)

  let eval_int_to_addr i range man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let rec eval_first j slots flow =
      debug "trying %d" j;
      match slots with
      | [] -> debug "end of first"; eval_others flow
      | hd :: tl ->
        let addrs = Slot.get hd in
        if addrs = [] then let _ = debug "no addr here" in eval_first (j + 1) tl flow
        else
          Eval.assume (mk_binop i O_eq (mk_int j range) range) ~zone:Universal.Zone.Z_u_num
            ~fthen:(fun flow ->
                List.map (fun addr -> Eval.singleton (mk_addr addr range) flow) addrs |>
                Eval.join_list
              )
            ~felse:(fun flow ->
                eval_first (j + 1) tl flow
              )
            man flow
    and eval_others flow =
      debug "eval others";
      let itv = man.ask (Itv.Q_interval i) flow in
      let addrs, can_be_null = Table.fold (fun addr itv' (acc, can_be_null) ->
          let eq = Itv.binop () O_eq itv itv' |> Channel.without_channel in
          let ne = Itv.binop () O_ne itv itv' |> Channel.without_channel in
          match Itv.is_bottom eq, Itv.is_bottom ne with
          | true, true -> acc, can_be_null
          | false, true -> addr :: acc, can_be_null
          | true, false -> acc, can_be_null
          | false, false -> addr :: acc, true
        ) a.others ([], false)
      in
      let case1 = List.map (fun addr -> Eval.singleton (mk_addr addr range) flow) addrs in
      let case2 =
        if addrs = [] || can_be_null then [Eval.singleton (mk_c_null range) flow]
        else []
      in
      Eval.join_list (case1 @ case2)
    in
    eval_first 0 a.first flow
            
    (** {2 Removal of addresses} *)
  (** ======================== *)
  
  let remove_addr_first addr first =
    List.map (Slot.remove addr) first

  let remove_addr addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let first = remove_addr_first addr a.first in
    let others = Table.remove addr a.others in
    let a' = { first; others } in
    Flow.set_domain_env T_cur a' man flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | S_remove({ekind = E_addr ({ addr_kind = A_stub_resource "FileDescriptor"} as addr, _)}) ->
      remove_addr addr man flow |>
      Post.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ new FileDescriptor ⟧ *)
    | E_alloc_addr(A_stub_resource "FileDescriptor") ->
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) exp flow |>
      Eval.bind_return @@ fun exp' flow ->

      let addr, mode =
        match ekind exp' with
        | E_addr(addr, mode) -> addr, mode
        | _ -> assert false
      in

      let flow' = insert addr man flow in
      Eval.singleton exp' flow'

    (* 𝔼⟦ _mopsa_fd_to_int(fd) ⟧ *)
    | E_c_builtin_call("_mopsa_fd_to_int", [fd]) ->
      man.eval ~zone:(Z_c, Z_c_points_to) fd flow |>
      Eval.bind_return @@ fun pt flow ->

      let addr =
        match ekind pt with
        | E_c_points_to (P_block (A (addr, mode), _)) -> addr
        | _ -> assert false
      in

      eval_addr_to_int addr exp.erange man flow

    (* 𝔼⟦ _mopsa_int_to_fd(i) ⟧ *)
    | E_c_builtin_call("_mopsa_int_to_fd", [i]) ->
      man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) i flow |>
      Eval.bind_return @@ fun i flow ->
      eval_int_to_addr i exp.erange man flow

    | _ -> None


  let ask _ _ _ = None

end

let () =
    Framework.Domain.register_domain (module Domain)
