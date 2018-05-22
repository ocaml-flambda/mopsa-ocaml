(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Memory initialization to zero predicate abstraction. *)

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
open Pointer
open Universal.Utils

let name = "c.memory.cell.initzero"
let debug fmt = Debug.debug ~channel:name fmt

type minmax =
  | Min | Max
let print_minmax fmt = function
  | Max -> Format.fprintf fmt "⇥"
  | Min -> Format.fprintf fmt "⇤"
let compare_minmax = compare

type bound =
  {
    b : base;
    m : minmax;
  }

let print_bound fmt c =
  Format.fprintf fmt "%a%a" print_minmax c.m pp_base c.b
let compare_bound c c' =
  compare_composer [
    (fun () -> compare_base c.b c'.b);
    (fun () -> compare_minmax c.m c'.m);
  ]

type addr_kind +=
  | A_tmp

type var_kind +=
  | V_zero of bound
  | V_tmp of base * expr

let annotate_var (v : var) (c : bound) = match v.vkind with
  | V_orig -> {v with vkind = V_zero c}
  | _ -> Debug.fail "tried to annotate var : %a with %a" Framework.Pp.pp_var v print_bound c

let vmin (v : var) =
  annotate_var v {b = V v ; m = Min}
let vmax (v : var) =
  annotate_var v {b = V v ; m = Max}

let is_origin (v : var) = match v.vkind with
  | V_orig -> true
  | _ -> false

let var_of_base b m =
  {vname = (let () = Format.fprintf Format.str_formatter "%a" print_bound {b ; m} in Format.flush_str_formatter ());
   vuid = base_uid b;
   vtyp = T_int;
   vkind = V_zero {b ; m}
  }

let set range v e =
  mk_stmt (S_assign(
      mk_var v (tag_range range "v"),
      e,
      STRONG
    )
    ) (tag_range range "v<-")

let incrs range v =
  mk_stmt (S_assign(
      mk_var v (tag_range range "v"),
      mk_binop
        (mk_var v (tag_range range "v"))
        (O_plus T_int)
        (mk_one (tag_range range "1"))
        (tag_range range "v+1"),
      STRONG
    )
    ) (tag_range range "v++")

let decrs range v =
  mk_stmt (S_assign(
      mk_var v (tag_range range "v"),
      mk_binop
        (mk_var v (tag_range range "v"))
        (O_minus T_int)
        (mk_one (tag_range range "1"))
        (tag_range range "v-1"),
      STRONG
    )
    ) (tag_range range "v--")

let diff range a b =
  mk_binop
    a
    (O_minus T_int)
    b
    (tag_range range "dvmod")

let mk_eq a b r : expr =
  mk_binop a O_eq b ~etyp:(T_int) r

let mk_c_int a t r : expr =
  mk_expr ~etyp:t (E_constant (C_int (Z.of_int a))) r

let mk_c_range a b t r : expr =
  mk_expr ~etyp:t (E_constant (C_int_interval(a, b))) r

(*==========================================================================*)
(**                       {2 Abstract domain}                               *)
(*==========================================================================*)


module Domain(SubDomain: Framework.Domains.Stateful.DOMAIN) = struct

  (*==========================================================================*)
  (**                       {2 Lattice structure}                             *)
  (*==========================================================================*)

  type t = unit
  let is_bottom x = false
  let top = ()
  let bottom = ()
  let join _ _ = ()
  let meet _ _ = ()
  let widening _ _ _ = ()
  let print _ _ = ()
  let leq _ _ = true
  let is_top x = true

  let unify ctx a1 a2 = a1, a2

  (*==========================================================================*)
  (**                      {2 Sub-domain manager}                             *)
  (*==========================================================================*)

  let local_manager = mk_local_manager (module SubDomain : Framework.Domains.Stateful.DOMAIN with type t = SubDomain.t)

  let local_exec ctx stmt s =
    let flow = set_domain_cur s local_manager local_manager.flow.bottom in
    let flow' = local_manager.exec ctx stmt flow in
    get_domain_cur local_manager flow'


  let sub_exec subman ctx stmt flow =
    match SubDomain.exec subman ctx stmt flow with
    | Some flow -> flow
    | None -> Debug.fail "[sub_exec] got None when : %a" Framework.Pp.pp_stmt stmt

  let recover_flow x = match x with
    | Some x -> x
    | None -> Debug.fail "[recover_flow] got None"
  (*==========================================================================*)
  (**                     {2 Transfer functions}                              *)
  (*==========================================================================*)

  let mk_ge a b r =
    mk_binop a O_ge b r
  let mk_gt a b r =
    mk_binop a O_gt b r
  let mk_le a b r =
    mk_binop a O_le b r
  let mk_lt a b r =
    mk_binop a O_lt b r

  let is_top_base range subman base ctx flow =
    let vmin = var_of_base base Min in
    let vmax = var_of_base base Max in
    let cond = mk_binop
        (mk_var vmax (tag_range range "vmax-istop"))
        O_le
        (mk_var vmin (tag_range range "vmin-istop"))
        (tag_range range "cond-istop")
    in
    let f' = SubDomain.exec subman ctx (mk_assume cond (tag_range range "assume-cond")) flow in
    match f' with
    | Some f' ->
      let subcur = subman.ax.get (subman.flow.get TCur f') in
      not (SubDomain.is_bottom subcur)
    | None -> Debug.fail "[is_top_base] got bottom"

  let set man subman base offset typ e range ctx flow =
    let c0 (* e2=0? *)=
      mk_binop e O_eq (mk_zero (tag_range range "0"))
        (tag_range range "rv=0")
    in
    let e_size = e |> etyp |> sizeof_type in
    let vmin = var_of_base base Min in
    let vmax = var_of_base base Max in
    let oleft = offset in
    let oright = mk_binop offset (O_plus T_int) (mk_z e_size (tag_range range "t")) (tag_range range "o+t") in
    if is_top_base range subman base ctx flow then
      switch_rexec
        [([(c0, true)],
          (fun flow ->
             sub_exec subman ctx (set range vmin oleft) flow
             |> sub_exec subman ctx (set range vmax oright)
             |> add_flow_mergers [mk_remove_var vmin range; mk_remove_var vmax range]
          )

         );
         ([(c0, false)], (fun flow ->
              Framework.Domains.Reduce.Domain.return_flow_no_opt flow));
        ] subman ctx flow
    else
      begin
        let diff_vmax_ol = diff range (mk_var vmax (tag_range range "vmax-oleft")) oleft in
        let diff_vmin_ol = diff range (mk_var vmin (tag_range range "vmin-oleft")) oleft in

        let diff_vmax_or = diff range (mk_var vmax (tag_range range "vmax-oright")) oright in
        let diff_vmin_or = diff range (mk_var vmin (tag_range range "vmin-oright")) oright in

        let c1 (* oleft < vmin*) =
          mk_gt diff_vmin_ol (mk_int 0 (tag_range range "int"))
            (tag_range range "oleft < vmin")
        in
        let c2 (* oleft >= vmin *) =
          mk_le diff_vmin_ol (mk_int 0 (tag_range range "int"))
            (tag_range range "oleft >= vmin")
        in
        let c3 (* oleft <= vmax *) =
          mk_ge diff_vmax_ol (mk_int 0 (tag_range range "int"))
            (tag_range range "oleft <= vmax")
        in
        let c4 (* oleft > vmax *) =
          mk_lt diff_vmax_ol (mk_int 0 (tag_range range "int"))
            (tag_range range "oleft > vmax")
        in
        let d1 (* oright < vmin*) =
          mk_gt diff_vmin_or (mk_int 0 (tag_range range "int"))
            (tag_range range "oright < vmin")
        in
        let d2 (* oright >= vmin *) =
          mk_le diff_vmin_or (mk_int 0 (tag_range range "int"))
            (tag_range range "oright >= vmin")
        in
        let d3 (* oright <= vmax *) =
          mk_ge diff_vmax_or (mk_int 0 (tag_range range "int"))
            (tag_range range "oright <= vmax")
        in
        let d4 (* oright > vmax *) =
          mk_lt diff_vmax_or (mk_int 0 (tag_range range "int"))
            (tag_range range "oright > vmax")
        in
        switch_rexec
          [([(c0, true); (c1, true); (d1, true)],
            (fun flow -> Framework.Domains.Reduce.Domain.return_flow_no_opt flow));
           ([(c0, true); (c1, true); (d2, true); (d3, true)], (fun flow ->
                sub_exec subman ctx (set range vmin oleft) flow
                |> add_flow_mergers [mk_remove_var vmin range]
              )

           );
           ([(c0, true); (c1, true); (d4, true)], (fun flow ->
                sub_exec subman ctx (set range vmin oleft) flow
                |> sub_exec subman ctx (set range vmax oright)
                |> add_flow_mergers [mk_remove_var vmin range; mk_remove_var vmax range])
           );

           ([(c0, true); (c2, true) ; (c3, true) ; (d2, true); (d3, true)],
            (fun flow -> Framework.Domains.Reduce.Domain.return_flow_no_opt flow));
           ([(c0, true); (c2, true) ; (c3, true) ; (d4, true)], (fun flow ->
                sub_exec subman ctx (set range vmax oright) flow
                |> add_flow_mergers [mk_remove_var vmax range])
           );

           ([(c0, true); (c4, true) ; (d4, true)], (fun flow ->
                Framework.Domains.Reduce.Domain.return_flow_no_opt flow
              ));

           ([(c0, false); (c1, true); (d1, true)],
            (fun flow -> Framework.Domains.Reduce.Domain.return_flow_no_opt flow));
           ([(c0, false); (c1, true); (d2, true); (d3, true)], (fun flow ->
                sub_exec subman ctx (set range vmin oright) flow
                |> add_flow_mergers [mk_remove_var vmin range])
           );
           ([(c0, false); (c1, true); (d4, true)], (fun flow ->
                sub_exec subman ctx (set range vmin (mk_var vmax (tag_range range "vmax"))) flow
                |> add_flow_mergers [mk_remove_var vmin range; mk_remove_var vmax range])
           );

           ([(c0, false); (c2, true) ; (c3, true) ; (d2, true); (d3, true)], (fun flow ->
                sub_exec subman ctx (set range vmin (mk_var vmax (tag_range range "vmax"))) flow
                |> add_flow_mergers [mk_remove_var vmin range; mk_remove_var vmax range])
           );

           ([(c0, false); (c2, true) ; (c3, true) ; (d4, true)], (fun flow ->
                sub_exec subman ctx (set range vmax oleft) flow
                |> add_flow_mergers [mk_remove_var vmax range])
           );

           ([(c0, false); (c4, true) ; (d4, true)], (fun flow ->
                Framework.Domains.Reduce.Domain.return_flow_no_opt flow
              ));
          ]
          man ctx flow
      end

  let init_zone_as base offset typ e range man subman ctx flow =
    let vmin = var_of_base base Min in
    let vmax = var_of_base base Max in
    let c0 (* e=0? *) =
      mk_binop e O_eq (mk_zero (tag_range range "0"))
        (tag_range range "rv=0")
    in
    assume_to_exec c0
      (fun flow ->
         sub_exec subman ctx
           (mk_assign
              (mk_var vmin (tag_range range "t2"))
              offset
              (tag_range range "t0")
           ) flow |>
         sub_exec subman ctx
           (mk_assign
              (mk_var vmax (tag_range range "t2"))
              (mk_binop offset (O_plus T_int) (mk_z (sizeof_type typ) (tag_range range "t3"))
                 (tag_range range "t4")
              )
              (tag_range range "t0")
           )
      )
      (fun flow ->
         flow
      )
      man ctx flow ()

  let rec exec man subman ctx stmt flow =
    debug "got : %a" Framework.Pp.pp_stmt stmt;
    let range = stmt.srange in
    match skind stmt with
    | S_c_local_declaration(v, init) when is_c_int_type (v.vtyp) ->
      Init.init_local (init_manager man subman ctx) v init range flow |>
      return_flow

    | S_rename_var(v , v') when is_origin v && is_origin v' ->
      let smin =
        mk_stmt
          (S_rename_var(vmin v, vmin v'))
          (tag_range range "rename_min")
      in
      let smax =
        mk_stmt
          (S_rename_var(vmax v, vmax v'))
          (tag_range range "rename_min")
      in
      let stmt' = mk_block [smin; smax] (tag_range range "rename_block") in
      SubDomain.exec subman ctx stmt' flow |>
      oflow_compose (add_flow_mergers [mk_remove_var (vmin v) (tag_range range "vmin merger");
                                       mk_remove_var (vmax v) (tag_range range "vmax merger");])

    | S_remove_var ({vkind = V_orig } as v) ->
      let v_min = vmin v in
      let v_max = vmax v in
      let smin = mk_stmt (Universal.Ast.S_remove_var v_min ) (tag_range range "remove_min") in
      let smax = mk_stmt (Universal.Ast.S_remove_var v_max ) (tag_range range "remove_max") in
      flow |>
      SubDomain.exec subman ctx smin |>
      (function  Some x -> SubDomain.exec subman ctx smax x | None -> None) |>
      oflow_compose (add_flow_mergers [mk_remove_var v_min (tag_range range "vmin merger");
                                       mk_remove_var v_max (tag_range range "vmax merger")])


    | S_assign(lval, rval, mode) when lval |> etyp |> is_c_int_type ->
      man.eval ctx rval flow |>
      eval_to_rexec (fun rval flow ->
          let p = mk_c_address_of lval (tag_range lval.erange "addrof") in
          man.eval ctx (mk_c_resolve_pointer p (tag_range lval.erange "res")) flow |>
          eval_to_rexec (fun plval flow ->
              match ekind plval with
              | E_c_points_to(E_p_var (base, offset, t)) ->
                set man subman base offset t rval range ctx flow

              | E_c_points_to(E_p_null) ->
                man.flow.add (Alarms.TNullDeref lval.erange) (man.flow.get TCur flow) flow |>
                man.flow.set TCur man.env.Framework.Lattice.bottom |>
                Framework.Domains.Reduce.Domain.return_flow_no_opt

              | E_c_points_to(E_p_invalid) ->
                man.flow.add (Alarms.TInvalidDeref lval.erange) (man.flow.get TCur flow) flow |>
                man.flow.set TCur man.env.Framework.Lattice.bottom |>
                Framework.Domains.Reduce.Domain.return_flow_no_opt
              | _ ->
                Debug.fail "initzero : mk_c_adress yielded : %a" Framework.Pp.pp_expr plval
            ) (man.exec ctx) man.flow
            ~empty:(fun flow -> {out = man.flow.top; publish = []; mergers = []})
        ) (man.exec ctx) man.flow
      |> fun x -> Some x

    | S_assign(lval, rval, mode)  ->
      None
    | _ ->
      None

  and tmp_var_of_base_offset base offset typ =
    match base with
    | V v -> {v with vkind = V_tmp(base, offset)}
    | A a -> {vname =
                (let () = Format.fprintf Format.str_formatter "%a" pp_base base in Format.flush_str_formatter ());
              vuid = a.addr_uid;
              vtyp = typ;
              vkind = V_tmp(base, offset)
             }

  and eval_zone_as base offset t range man ctx flow =
    let vmin = var_of_base base Min in
    let vmax = var_of_base base Max in
    let rval_size = sizeof_type t in

    let oleft = offset in
    let oright = mk_binop offset (O_plus T_int) (mk_z rval_size (tag_range range "t")) (tag_range range "o+t") in

    let diff_vmin_ol = diff range (mk_var vmin (tag_range range "vmin-oleft")) oleft in
    let diff_vmax_or = diff range (mk_var vmax (tag_range range "vmax-oright")) oright in

    let c2 (* oleft >= vmin *) =
      mk_le diff_vmin_ol (mk_int 0 (tag_range range "c20int"))
        (tag_range range "oleft >= vmin")
    in
    let c3 (* oright <= vmax *) =
      mk_ge diff_vmax_or (mk_int 0 (tag_range range "c30int"))
        (tag_range range "oright <= vmax")
    in

    let cond = mk_binop c2 O_log_and c3 (tag_range range "cond") in

    let rep = assume_to_eval cond
        (fun flow ->
           oeval_singleton (Some (mk_c_int 0 t (tag_range range "t0")), flow, [])
        )
        (fun flow ->
           let a, b = rangeof t in
           oeval_singleton (Some (mk_c_range a b t (tag_range range "t0")), flow, [])
        ) man ctx flow ()
    in
    add_eval_mergers [] rep

  and eval man subman ctx exp flow =
    let range = erange exp in
    match ekind exp with
    | E_var ({vkind = V_orig} as v) when is_c_int_type v.vtyp ->
      eval_zone_as (V v) (mk_int 0 (tag_range range "t0")) (etyp exp) range man ctx flow

    | E_var ({vkind = V_orig} as v) when is_c_type v.vtyp ->
      None

    | E_c_address_of e ->
      None

    | E_c_deref(p) ->
      let p = mk_c_resolve_pointer p (tag_range range "t1") in
      man.eval ctx p flow |>
      ( eval_compose (fun p flow ->
            match ekind p with
            | E_c_points_to(E_p_var (base, offset, t)) ->
              begin
                eval_zone_as base offset t range man ctx flow
              end
            | _ ->
              None
          )
      )

    | E_c_arrow_access(p, i, f) ->
      None

    | E_c_array_subscript(arr, idx) ->
      None

    | E_c_member_access(r, idx, f) ->
      None

    | _ -> None


  (*==========================================================================*)
  (**                    {2 Cells Initialization}                             *)
  (*==========================================================================*)


(* let set man base offset typ e range ctx flow = *)
  and init_manager man subman ctx =
    Init.{
      scalar = (fun v e range flow ->
          let rec aux lv o = match ekind lv with
            | E_var v -> set man subman (V v) (mk_z o (tag_range range "t5")) (etyp e) e range ctx flow
            | E_c_array_subscript(lv,{ekind = E_constant (C_int i)}) ->
              aux lv (Z.(o + i * (lv |> etyp |> under_array_type |> sizeof_type)))
            | E_c_member_access(r, i, s) ->
              let align = align_byte (etyp lv) i in
              aux lv (Z.(o + (of_int align) * (lv |> etyp |> under_array_type |> sizeof_type)))
            | _ -> assert false
          in
          aux v Z.zero |> Framework.Domains.Reduce.Domain.flow_of_rflow
        );

      array = (fun a is_global init_list range flow ->
          let range = erange a in
          let rec aux i l flow =
            match l with
            | [] -> flow
            | init :: tl ->
              let lv = mk_c_subscript_access
                  a
                  (mk_int i (tag_range range "t0"))
                  (tag_range range "t6")
              in
              let flow' = init_expr (init_manager man subman ctx) lv is_global init range flow in
              aux (i+1) tl flow'
          in
          aux 0 init_list flow
        );

      record =  (fun s is_global init_list range flow ->
          let range = erange s in
          let record = match remove_typedef (etyp s) with T_c_record r -> r | _ -> assert false in
          match init_list with
          | Parts parts ->
            debug "init struct by parts";
            let rec aux i l acc =
              match l with
              | [] -> acc
              | init :: tl ->
                let field = List.nth record.c_record_fields i in
                let lv = mk_c_member_access
                    s
                    field
                    (tag_range range "t0")
                in
                let flow' = init_expr (init_manager man subman ctx) lv is_global init range flow in
                aux (i + 1) tl flow'
            in
            aux 0 parts flow
          | Expr e ->
            record.c_record_fields |> List.fold_left (fun flow field ->
                let init = C_init_expr (mk_c_member_access e field range) in
                let lv = mk_c_member_access s field range in
                let flow' = init_expr (init_manager man subman ctx) lv is_global (Some init) range flow in
                flow'
              ) flow
        )
    }

  let init man subman ctx prog flow =
    let flow = set_domain_cur () man flow in
    match prog.prog_kind with
    | C_program(globals, _) ->
      let flow' = Init.fold_globals ctx (init_manager man subman ctx) globals flow in
      ctx, flow'
    | _ -> ctx, flow

  and ask : type r. ('a, t) manager -> ('a, SubDomain.t) manager -> Framework.Context.context -> r Framework.Query.query -> 'a Framework.Flow.flow -> r option =
    fun man subman ctx query flow ->
      match query with
      | _ -> None

  let refine man subman ctx channel flow = None


end

let setup () =
  register_domain name (module Domain);
  register_vkind_compare (fun next vk1 vk2 ->
      match vk1, vk2 with
      | V_zero c1, V_zero c2 -> compare_bound c1 c2
      (* | V_tmp(b1,o1), V_tmp(b2,o2) -> compare_composer [(fun () -> compare_base b1 b2); (fun () -> compare o1 o2)] *)
      | _ -> next vk1 vk2
    );
  register_pp_vkind (fun next fmt vk ->
      match vk with
      | V_zero c -> print_bound fmt c
      (* | V_tmp(b,o)  -> Format.fprintf fmt "<%a,%a)>"pp_base fmt b *)
      | _ -> next fmt vk
    )
