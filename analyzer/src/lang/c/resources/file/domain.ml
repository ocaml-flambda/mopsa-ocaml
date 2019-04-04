(****************************************************************************)
(*                                                                          *)
(* This file is part of MOPSA, a Modular Open Platform for Static Analysis. *)
(*                                                                          *)
(* Copyright (C) 2017-2019 The MOPSA Project.                               *)
(*                                                                          *)
(* This program is free software: you can redistribute it and/or modify     *)
(* it under the terms of the GNU Lesser General Public License as published *)
(* by the Free Software Foundation, either version 3 of the License, or     *)
(* (at your option) any later version.                                      *)
(*                                                                          *)
(* This program is distributed in the hope that it will be useful,          *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of           *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            *)
(* GNU Lesser General Public License for more details.                      *)
(*                                                                          *)
(* You should have received a copy of the GNU Lesser General Public License *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.    *)
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
open Memory.Common.Points_to
module Itv = Universal.Numeric.Values.Intervals.Integer.Value
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

  let bottom = {
    first = List.init window (fun _ -> Slot.Bot);
    others = Table.bottom;
  }

  let top = {
    first = List.init window (fun _ -> Slot.Top);
    others = Table.top;
  }

  let is_bottom (a:t) =
    List.exists Slot.is_bottom a.first ||
    Table.is_bottom a.others

  let subset a1 a2 =
    List.for_all2 Slot.subset a1.first a2.first &&
    Table.subset a2.others a1.others

  let join a1 a2 = {
    first = List.map2 Slot.join a1.first a2.first;
    others = Table.join a1.others a2.others;
  }

  let meet a1 a2 = {
    first = List.map2 Slot.meet a1.first a2.first;
    others = Table.meet  a1.others a2.others;
  }

  let widen annot a1 a2 = {
    first = List.map2 Slot.join a1.first a2.first;
    others = Table.widen annot a1.others a2.others;
  }

  let merge pre (post,log) (post',log') =
    assert false

  let print fmt a =
    let rec pp_first fmt l =
      let l' = List.mapi (fun i s -> (i, s)) l in
      Format.pp_print_list
        ~pp_sep:(fun fmt () -> Format.fprintf fmt " ,@;")
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

  include Framework.Core.Id.GenDomainId(struct
      type typ = t
      let name = "c.resources.file"
    end)


  (** Zoning definition *)
  (** ================= *)

  let interface = {
    iexec = {
      provides = [Z_c];
      uses = []
    };

    ieval = {
      provides = [Z_c, Z_c_low_level];
      uses = [
        Z_c, Z_c_points_to;
        Z_c, Universal.Zone.Z_u_num;
        Universal.Zone.Z_u_heap, Z_any
      ]
    }
  }


  (** Initialization of environments *)
  (** ============================== *)

  let stdno_to_string n =
    if n = 0 then "stdin" else
    if n = 1 then "stdout" else
    if n = 2 then "stderr"
    else panic "stdno_to_string: invalid argument %d" n

  let init prog man flow =
    Some flow
      (* {
       *   flow;
       *   (\* Need a callback to evaluate heap allocation *\)
       *   callbacks = [(fun flow ->
       *       (\* generic allocation function *\)
       *       let allocate_std n flow =
       *         let range = tag_range prog.prog_range "alloc_%s" (stdno_to_string n) in
       *         man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) (mk_alloc_addr (A_stub_resource "FileDescriptor") range) flow |>
       *         Eval.bind @@ fun alloc flow ->
       *         match ekind alloc with
       *         | E_addr addr -> Eval.singleton addr flow
       *         | _ -> assert false
       *       in
       *       (\* allocate stdin, stdout and stderr *\)
       *       allocate_std 0 flow |> Post.bind_flow man @@ fun stdin_addr flow ->
       *       allocate_std 1 flow |> Post.bind_flow man @@ fun stdout_addr flow ->
       *       allocate_std 2 flow |> Post.bind_flow man @@ fun stderr_addr flow ->
       * 
       *       let init_state = {
       *         first = [
       *           NotFree (AddrSet.singleton stdin_addr);
       *           NotFree (AddrSet.singleton stdout_addr);
       *           NotFree (AddrSet.singleton stderr_addr);
       *         ];
       *         others = Table.empty;
       *       }
       *       in
       *       Flow.set_domain_env T_cur init_state man flow
       *     )]
       * } *)


  (** {2 Insertion of new resources} *)
  (** ============================= *)

  (* Insert an address in the first slots *)
  let rec insert_first addr range man flow =
    let a = get_domain_env T_cur man flow in
    let rec iter i not_inserted_before slots =
      match slots with
      | [] -> [], []
      | hd :: tl ->
        let inserted_here, not_inserted_here = Slot.insert addr hd in
        let cases, not_inserted_after =
          if Slot.is_bottom not_inserted_here then
            [], tl
          else
            iter (i + 1) (not_inserted_before @ [not_inserted_here]) tl
        in
        let cases' =
          if Slot.is_bottom inserted_here then cases
          else
            let exp = mk_int i range in
            let a' = { a with first = not_inserted_before @ [inserted_here] @ not_inserted_after } in
            let flow' = set_domain_env T_cur a' man flow in
            Eval.singleton exp flow' :: cases
        in
        cases', not_inserted_here :: not_inserted_after
    in
    let cases, not_inserted = iter 0 [] a.first in
    cases, { a with first = not_inserted }

  let bounds itv =
    if Itv.is_bounded itv then
      Itv.bounds itv
    else
      let _, max = rangeof s32 in
      Z.of_int window, max


  (** Insert an address in the remaining part of the table *)
  let insert_others addr range man flow =
    let a = get_domain_env T_cur man flow in
    let others, itv = Table.insert addr window a.others in
    if Itv.is_bottom itv then
      []
    else
      let l, u = bounds itv in
      let exp = mk_z_interval l u range in
      let flow = set_domain_env T_cur { a with others } man flow in
      [Eval.singleton exp flow]


  (** Insert an address in the table of file descriptors and return its interval *)
  let insert addr range man flow =
    let case1, not_inserted = insert_first addr range man flow in
    let case2 =
      if is_bottom not_inserted then
        []
      else
        let flow' = set_domain_env T_cur not_inserted man flow in
        insert_others addr range man flow'
    in
    Eval.join_list (case1 @ case2)


  (** {2 Find the address of a numeric file descriptor} *)
  (** ================================================= *)

  let find i range man flow =
    let a = get_domain_env T_cur man flow in
    let rec find_first j slots flow =
      match slots with
      | [] -> find_others flow
      | hd :: tl ->
        let addrs = Slot.get hd in
        if addrs = [] then find_first (j + 1) tl flow
        else
          assume_eval (mk_binop i O_eq (mk_int j range) range) ~zone:Universal.Zone.Z_u_num
            ~fthen:(fun flow ->
                List.map (fun addr -> Eval.singleton (mk_addr addr range) flow) addrs |>
                Eval.join_list
              )
            ~felse:(fun flow ->
                find_first (j + 1) tl flow
              )
            man flow
    and find_others flow =
      let a = get_domain_env T_cur man flow in
      let itv = man.ask (Itv.EvalQuery.query i) flow in

      (* First case: return addresses having a descriptor interval
         intersecting with the target interval *)
      let case1 =
        Table.filter (fun addr itv' ->
            not @@ Itv.is_bottom (Itv.meet itv itv')
          ) a.others |>
        Table.pool |>
        (* FIXME: improve partitioning by filtering the flow *)
        List.map (fun addr -> Eval.singleton (mk_addr addr range) flow)
      in

      (* Second case: return NULL when all intervals may differ from the target interval *)
      let case2 =
        if Table.for_all (fun _ itv' ->
            let itv1, itv2 = Itv.compare O_ne itv itv' true in
            not @@ Itv.is_bottom itv1 &&
            not @@ Itv.is_bottom itv2
          ) a.others
        then
          [Eval.singleton (mk_c_null range) flow]
        else
          []
      in
      Eval.join_list (case1 @ case2) ~empty:(Eval.empty_singleton flow)
    in
    find_first 0 a.first flow


  (** {2 Removal of addresses} *)
  (** ======================== *)

  let remove addr man flow =
    let a = get_domain_env T_cur man flow in
    let first = List.map (Slot.remove addr) a.first in
    let others = Table.remove addr a.others in
    let a' = { first; others } in
    set_domain_env T_cur a' man flow


  (** Computation of post-conditions *)
  (** ============================== *)

  let exec zone stmt man flow  =
    match skind stmt with
    | S_remove({ekind = E_addr ({ addr_kind = A_stub_resource "FileDescriptor"} as addr)}) ->
      remove addr man flow |>
      Post.return |>
      Option.return

    | _ -> None


  (** Evaluation of expressions *)
  (** ========================= *)

  let eval zone exp man flow =
    match ekind exp with
    (* 𝔼⟦ new FileDescriptor ⟧ *)
    | E_alloc_addr(A_stub_resource "FileDescriptor") ->
      man.eval ~zone:(Universal.Zone.Z_u_heap, Z_any) exp flow |>
      Eval.bind_return @@ fun exp' flow ->
      insert (Addr.from_expr exp') exp.erange man flow

    (* 𝔼⟦ n in FileDescriptor ⟧ *)
    | E_stub_resource_mem(n, "FileDescriptor") ->
      man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) n flow |>
      Eval.bind_return @@ fun n flow ->

      find n exp.erange man flow |>
      Eval.bind @@ fun addr flow ->

      let exp' =
        match ekind addr with
        | E_addr { addr_kind = A_stub_resource "FileDescriptor" } -> mk_one ~typ:u8 exp.erange
        | _ -> mk_zero ~typ:u8 exp.erange
      in

      Eval.singleton exp' flow

    (* 𝔼⟦ _mopsa_int_to_fd(n) ⟧ *)
    | E_c_builtin_call("_mopsa_int_to_fd", [n]) ->
      man.eval ~zone:(Z_c, Universal.Zone.Z_u_num) n flow |>
      Eval.bind_return @@ fun n flow ->
      find n exp.erange man flow

    | _ -> None


  let ask _ _ _ = None

end

let () =
    Framework.Core.Sig.Intermediate.Domain.register_domain (module Domain)
