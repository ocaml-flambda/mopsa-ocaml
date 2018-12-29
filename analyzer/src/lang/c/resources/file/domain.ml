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


(** {2 Command-line options} *)
(** ************************ *)

(** Precision window. The first file descriptors ∈ [0, window) are 
      abstracted more precisely. *)
let opt_window = ref 3

let () =
  register_option (
    "-filedes-precision",
    Arg.Set_int opt_window,
    "number of the first file descriptors represented precisely. (default: 3)"
  )


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

  let init_state () = {
    first = List.init !opt_window (fun _ -> Slot.Free);
    others = Table.empty;
  }

  let bottom = {
    first = [Slot.Bot];
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
    Some (Flow.set_domain_env T_cur (init_state ()) man flow)



  (** File management functions *)
  (** ========================= *)


  (* Iterate on first slot and try to insert the address *)
  let rec insert_first addr i (first:slot list) =
    match first with
    | [] -> [], []
    | hd :: tl ->
      match Slot.insert addr hd with
      | Some hd', None ->
        [hd' :: tl], []

      | None, None ->
        [], []

      | None, Some hd' ->
        let after_inserted, after_no_place = insert_first addr (i + 1) tl in
        let no_place = List.map (fun tl' -> hd' :: tl') after_no_place in
        let inserted = List.map (fun tl' -> hd' :: tl') after_inserted in
        inserted, no_place

      | Some hd', Some hd'' ->
        let after_inserted, after_no_place = insert_first addr (i + 1) tl in
        let no_place = List.map (fun tl' -> hd'' :: tl') after_no_place in
        let inserted = List.map (fun tl' -> hd'' :: tl') after_inserted in
        (hd' :: tl) :: inserted, no_place


  (** Insert an address in the abstract state and compute the interval of its ids *)
  let insert addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let inserted, no_place = insert_first addr 0 a.first in
    let annot = Flow.get_all_annot flow in
    let case1 =
      match inserted with
      | hd :: tl ->
        let first = List.fold_left (List.map2 Slot.join) hd tl in
        let a' = { a with first } in
        Flow.set_domain_env T_cur a' man flow

      | [] -> Flow.bottom annot
    in
    
    let case2 =
      match no_place with
      | hd :: tl ->
        let first = List.fold_left (List.map2 Slot.join) hd tl in
        let others = Table.insert addr !opt_window a.others in
        let a' = { first; others } in
        Flow.set_domain_env T_cur a' man flow

      | _ -> Flow.bottom annot
    in

    Flow.join man case1 case2

  let find_itv_first addr first =
    let rec iter i l =
      match l with
      | [] -> Itv.bottom
      | hd :: tl ->
        let after = iter (i + 1) tl in
        if Slot.mem addr hd then Itv.join () (Itv.of_int i i) after
        else after
    in
    iter 0 first

  let find_itv_others addr (others:table) = Table.find_itv addr others

  let find_itv addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let itv1 = find_itv_first addr a.first in
    let itv2 = find_itv_others addr a.others in
    Itv.join () itv1 itv2

  let find_addr_first itv first =
    let rec iter i l =
      match l with
      | [] -> AddrSet.empty
      | hd :: tl ->
        let after = iter (i + 1) tl in
        match Itv.mem (Z.of_int i) itv, Slot.addr_opt hd with
        | false, _ -> after
        | true, None -> after
        | true, Some a ->
          AddrSet.union a after
    in
    iter 0 first

  let find_addr_others itv (others:table) =
    Table.fold (fun addr itv' acc ->
        if Itv.is_bottom (Itv.meet () itv itv') then acc
        else AddrSet.add addr acc
      ) others AddrSet.empty

  let find_addr itv man flow =
    (* FIXME: return NULL in case itv can be un-allocated*)
    let a = Flow.get_domain_env T_cur man flow in
    let a1 = find_addr_first itv a.first in
    let a2 = find_addr_others itv a.others in
    AddrSet.union a1 a2

  let remove_addr_first addr first =
    List.map (Slot.remove_addr addr) first

  let remove_addr addr man flow =
    let a = Flow.get_domain_env T_cur man flow in
    let first = remove_addr_first addr a.first in
    let others = Table.remove_addr addr a.others in
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

      let itv = find_itv addr man flow in
      let l, u = Itv.bounds itv in
      let exp' = mk_z_interval l u exp.erange in

      Eval.singleton exp' flow

    (* 𝔼⟦ _mopsa_int_to_fd(fd) ⟧ *)
    | E_c_builtin_call("_mopsa_int_to_fd", [id]) ->
      man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) id flow |>
      Eval.bind_return @@ fun id flow ->

      let itv = man.ask (Itv.Q_interval id) flow in
      let addrs = find_addr itv man flow in

      let evl = AddrSet.elements addrs |>
                List.map (fun addr ->
                    Eval.singleton (mk_addr addr exp.erange) flow
                  )
      in
      Eval.join_list evl

    | _ -> None


  let ask _ _ _ = None

end

let () =
    Framework.Domain.register_domain (module Domain)
