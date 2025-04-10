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

(**
  C_simplify - C AST to C AST simplification
 *)

open C_AST
open C_utils

let tmp_name = "__SAST_tmp"

type context = {
    mutable uid: int;
    target: C.target_info;
  }

let create_context ?(min_uid=0) (target:C.target_info) = {
    uid = min_uid;
    target = target;
  }


let new_uid ctx =
  ctx.uid <- ctx.uid + 1;
  ctx.uid


let make_temp ctx range ?(com:comment list=[]) (f:func option) (t:type_qual) : variable =
  let u = new_uid ctx in
  let v = {
      var_uid = u;
      var_org_name = tmp_name;
      var_unique_name = Printf.sprintf "%s_%i" tmp_name u;
      var_kind = (
        match f with
        | Some f -> Variable_local f
        | None -> Variable_global
      );
      var_type = t;
      var_init = None;
      var_range = range;
      var_com = com;
      var_before_stmts = [];
      var_after_stmts = [];
    }
  in
  (match f with
   | Some f -> f.func_local_vars <- v::f.func_local_vars
   | None -> ());
  v


(* integer promotion *)
let int_promote target ((e,(t,q),r) as ee) =
  match t with
  | T_integer i ->
     if sizeof_int target i < sizeof_int target SIGNED_INT
     then E_cast (ee, IMPLICIT), (T_integer SIGNED_INT,q), r
     else ee
  | T_bool ->
     E_cast (ee, IMPLICIT), (T_integer SIGNED_INT,q), r
  | _ ->
     ee

(* constant 1 compatible with the incrementation of type t *)
let rec expr_one target range (t:typ) =
  match t with
  | T_integer i ->
     (* integer promotion *)
     let ii =
       if sizeof_int target i < sizeof_int target SIGNED_INT then SIGNED_INT
       else i
     in
     expr_integer_cst range ii Z.one
  | T_bool ->
     expr_integer_cst range SIGNED_INT Z.one
  | T_float f ->
     expr_float_cst range f 1.
  | T_pointer _ ->
     expr_integer_cst range (ptrdiff_type target) Z.one
  | T_bitfield (t,_) ->
     expr_one target range t
  | T_enum u ->
     expr_one target range (T_integer (match u.enum_integer_type with | Some s -> s | None -> assert false))
  | T_typedef d ->
     expr_one target range (fst d.typedef_def)
  | _ ->
     error range "cannot increment type" (C_print.string_of_type t)

(* converts back a statement list to an expression *)
let as_expr_stmt l t r : expr =
  match l with
  | [S_expression e,_] -> e
  | [] -> expr_void r
  | _ -> E_statement (make_block l), t, r

(* encolse in a S_block if the block contains variable declaration to limit their scope *)
let scope_temp range (s:statement list) : statement list =
  let b = make_block s in
  if b.blk_local_vars = [] then s else [S_block b,range]

(*****************************)
(** Simplification functions *)
(*****************************)

(* simplify an expression
   we return triples:
   - statements to execute before evaluating (tmp variable declaration, etc.)
   - simplified expression
   - statements to execute after evaluating
   - if call=true, assign the result of function calls to temporaries
     (necessary in some cases to ensure that the side-effect is only
      evaluated once)
*)
let rec simplify_expr ctx f (call:bool) ((e,t,r):expr)
  : (statement list) * (expr) * (statement list) =
  match e with

    (* e1 ? e2 : e3 -> if (e1) tmp = e2; else tmp = e3; <tmp> *)
    | E_conditional (e1,e2,e3) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       if is_void t then
         let cond = S_if (e1,
                          simplify_expr_stmt ctx f call e2 |> make_block,
                          simplify_expr_stmt ctx f call e3 |> make_block), r
         in
         before1@[cond], expr_void r, after1
       else
         let before2, e2, after2 = simplify_expr ctx f call e2 in
         let before3, e3, after3 = simplify_expr ctx f call e3 in
         let tmp = make_temp ctx r f t in
         let tmp_var = E_variable tmp, t, r in
         let create = S_local_declaration tmp, r in
         let cond =
           S_if (e1,
                 before2@[S_expression (E_assign (tmp_var, e2), t, r), r]@after2 |> make_block,
                 before3@[S_expression (E_assign (tmp_var, e3), t, r), r]@after3 |> make_block), r
         in
         before1@[create;cond], tmp_var, after1

    (* e1 ? : e2 -> if (e1) tmp = e1; else tmp = e2; <tmp>
       the side-effects of e1 are executed only once
    *)
    | E_binary_conditional (e1,e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       if is_void t then
         let cond = S_if (e1,
                          [] |> make_block,
                          simplify_expr_stmt ctx f call e2 |> make_block), r
         in
         before1@[cond], expr_void r, after1
       else
         let before2, e2, after2 = simplify_expr ctx f call e2 in
         let tmp = make_temp ctx r f t in
         let tmp_var = E_variable tmp, t, r in
         let create = S_local_declaration tmp, r in
         let cond =
           S_if (e1,
                 [S_expression (E_assign (tmp_var, e1), t, r), r] |> make_block,
                 before2@[S_expression (E_assign (tmp_var, e2), t, r), r]@after2 |> make_block), r
         in
         before1@[create;cond], tmp_var, after1

    | E_array_subscript(e1,e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before2, e2, after2 = simplify_expr ctx f call e2 in
       before1@before2, (E_array_subscript(e1,e2), t, r), after2@after1

    | E_member_access (e1,i,field) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       before1, (E_member_access (e1,i,field), t, r), after1

    | E_arrow_access (e1,i,field) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       before1, (E_arrow_access (e1,i,field), t, r), after1

    (* e1 op= e2 -> e1 = e1 op e2; <e1> *)
    | E_compound_assign (e1,t1,op,e2,t2) ->
       (* force temporaries for function calls as e1 is used twice *)
       let before1, e1, after1 = simplify_expr ctx f true e1 in
       let before2, e2, after2 = simplify_expr ctx f call e2 in
       let before = before1@before2 and after = after2@after1 in
       let e1t1 = E_cast (e1, IMPLICIT), t1, r in
       let e12 = E_binary (O_arithmetic op, e1t1, e2), t2, r in
       let e12t = E_cast (e12, IMPLICIT), t, r in
       let assign = S_expression (E_assign (e1, e12t), t, r), r in
       before@[assign], e1, after

    | E_binary (O_logical LOGICAL_OR, e1, e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before2, e2, after2 = simplify_expr ctx f call e2 in
       if before2 = [] && after2 = [] && after1 = [] then
         before1, (E_binary (O_logical LOGICAL_OR, e1, e2), t, r), []
       else
         let tmp = make_temp ctx r f bool_type in
         let tmp_var = E_variable tmp, bool_type, r in
         let create = S_local_declaration tmp, r in
         (* Since e1,e2 will be assigned to [tmp], add an implicit cast to _Bool if necessary *)
         let e1' = match expr_type e1 |> resolve_typedef |> fst with
           | T_bool -> e1
           | _ -> (E_cast(e1,IMPLICIT),bool_type,r)
         and e2' = match expr_type e2 |> resolve_typedef |> fst with
           | T_bool -> e2
           | _ -> (E_cast(e2,IMPLICIT),bool_type,r)
         in
         let assign1 = S_expression (E_assign (tmp_var, e1'), t, r), r in
         let assign2 =
           S_if (tmp_var,
                 [] |> make_block,
                 before2@[S_expression (E_assign (tmp_var, e2'), t, r), r]@after2 |> make_block), r
         in
         before1@[create;assign1]@after1@[assign2], tmp_var, []

    | E_binary (O_logical LOGICAL_AND, e1, e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before2, e2, after2 = simplify_expr ctx f call e2 in
       if before2 = [] && after2 = [] && after1 = [] then
         before1, (E_binary (O_logical LOGICAL_AND, e1, e2), t, r), []
       else
         let tmp = make_temp ctx r f bool_type in
         let tmp_var = E_variable tmp, bool_type, r in
         let create = S_local_declaration tmp, r in
         (* Since e1,e2 will be assigned to [tmp], add an implicit cast to _Bool if necessary *)
         let e1' = match expr_type e1 |> resolve_typedef |> fst with
           | T_bool -> e1
           | _ -> (E_cast(e1,IMPLICIT),bool_type,r)
         and e2' = match expr_type e2 |> resolve_typedef |> fst with
           | T_bool -> e2
           | _ -> (E_cast(e2,IMPLICIT),bool_type,r)
         in
         let assign1 = S_expression (E_assign (tmp_var, e1'), t, r), r in
         let assign2 =
           S_if (tmp_var,
                 before2@[S_expression (E_assign (tmp_var, e2'), t, r), r]@after2 |> make_block,
                 [] |> make_block), r
         in
         before1@[create;assign1]@after1@[assign2], tmp_var, []

    | E_binary (op,e1,e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before2, e2, after2 = simplify_expr ctx f call e2 in
       let before = before1@before2 and after = after2@after1 in
       before, (E_binary (op,e1,e2), t, r), after

    (* e1 = e2 -> e1 = e2; <e1> *)
    | E_assign (e1,e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before2, e2, after2 = simplify_expr ctx f call e2 in
       let before = before1@before2 and after = after2@after1 in
       let assign = S_expression (E_assign (e1,e2), t, r), r in
       before@[assign], e1, after

    (* e1,e2 -> e1; <e2> *)
    | E_comma (e1,e2) ->
      let b1 = simplify_expr_stmt ctx f call e1 in
      let before2, e2, after2 = simplify_expr ctx f call e2 in
      b1@before2, e2, after2

   | E_unary (op,e1) ->
      let before1, e1, after2 = simplify_expr ctx f call e1 in
      before1, (E_unary (op,e1), t, r), after2

   (* ++e1 -> e1 = e1 + 1; <e1> *)
   | E_increment (dir,PRE,e1) ->
      (* force temporary for function call as e1 is used twice *)
      let before1, e1, after1 = simplify_expr ctx f true e1 in
      let ep = int_promote ctx.target e1 in
      let op = O_arithmetic (if dir = INC then ADD else SUB) in
      let e1p = E_binary (op, ep, expr_one ctx.target r (fst t)), t, r in
      let e1c = E_cast (e1p, IMPLICIT), t, r in
      let inc = S_expression (E_assign (e1,e1c), t, r), r in
      before1@[inc], e1, after1

   (* e1++ -> <e1>; e1 = e1 + 1 *)
   | E_increment (dir,POST,e1) ->
      (* force temporary for function call as e1 is used twice *)
      let before1, e1, after1 = simplify_expr ctx f true e1 in
      let ep = int_promote ctx.target e1 in
      let op = O_arithmetic (if dir = INC then ADD else SUB) in
      let e1p = E_binary (op, ep, expr_one ctx.target r (fst t)), t, r in
      let e1c = E_cast (e1p, IMPLICIT), t, r in
      let inc = S_expression (E_assign (e1,e1c), t, r), r in
      before1, e1, after1@[inc]

   | E_address_of e1 ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      before1, (E_address_of e1, t, r), after1

   | E_deref e1 ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      before1, (E_deref e1, t, r), after1

   | E_cast (e1,x) ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      before1, (E_cast (e1,x), t, r), after1

   (* if call=true: f(...) -> tmp = f(...); <tmp> *)
   | E_call (e1,ea) ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      let ea = Array.copy ea in
      let beforea, aftera = ref before1, ref after1 in
      for i=0 to Array.length ea-1 do
        let before, ee, after = simplify_expr ctx f call ea.(i) in
        (* FIMXE: shouldn't this be the other way around? *)
        beforea := before@(!beforea);
        aftera := after@(!aftera);
        ea.(i) <- ee
      done;
      let ecall = E_call (e1,ea), t, r in

      if is_void t || not call then
        (* don't add a temporary *)
        !beforea, ecall, !aftera
      else (
        (* add a temporary to ensure the side-effect is executed only once *)
        let tmp = make_temp ctx r f t in
        let tmp_var = E_variable tmp, t, r in
        let create = S_local_declaration tmp, r in
        let bind = S_expression (E_assign (tmp_var, ecall), t, r), r in
        !beforea@[create;bind], tmp_var, !aftera
      )

   | E_character_literal _
   | E_integer_literal _
   | E_float_literal _
   | E_string_literal _
   | E_variable _
   | E_function _
   | E_predefined _
     -> [], (e,t,r), []

   | E_var_args e1 ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      before1, (E_var_args e1, t, r), after1

   | E_atomic (i,ea) ->
      let ea = Array.copy ea in
      let beforea, aftera = ref [], ref [] in
      for i=0 to Array.length ea-1 do
        let before, ee, after = simplify_expr ctx f call ea.(i) in
        (* FIMXE: shouldn't this be the other way around? *)
        beforea := before@(!beforea);
        aftera := after@(!aftera);
        ea.(i) <- ee
      done;
      !beforea, (E_atomic (i,ea), t, r), !aftera

   | E_compound_literal i ->
      let before1, i, after1 = simplify_init ctx f call i in
      let tmp = make_temp ctx r f t in
      let tmp_var = E_variable tmp, t, r in
      let create = S_local_declaration tmp, r in
      tmp.var_init <- Some i;
      before1@[create], tmp_var, after1

   | E_statement b ->
      let rec doit acc = function
        | [S_expression e1, r] ->
           let before1, e1, after1 = simplify_expr ctx f call e1 in
           let before1, e1 = remove_after ctx f before1 e1 after1 in
           acc@before1, e1, []
        | s::rest ->
           doit (acc@(simplify_stmt ctx f call s)) rest
        | [] ->
           acc, expr_void r, []
      in
      doit [] b.blk_stmts

   | E_convert_vector e1 ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      before1, (E_convert_vector e1, t, r), after1

   | E_vector_element (e1,a) ->
      let before1, e1, after1 = simplify_expr ctx f call e1 in
      before1, (E_vector_element (e1, a), t, r), after1

   | E_shuffle_vector ea ->
      let ea = Array.copy ea in
      let beforea, aftera = ref [], ref [] in
      for i=0 to Array.length ea-1 do
        let before, ee, after = simplify_expr ctx f call ea.(i) in
        beforea := before@(!beforea);
        aftera := after@(!aftera);
        ea.(i) <- ee
      done;
      !beforea, (E_shuffle_vector ea, t, r), !aftera

  (* case of toplevel (statement) expressions: the returned value is not used *)
  and simplify_expr_stmt ctx f (call:bool) ((e,t,r):expr) : statement list =
    match e with

    | E_conditional (e1,e2,e3) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let cond = S_if (e1,
                        simplify_expr_stmt ctx f call e2 |> make_block,
                        simplify_expr_stmt ctx f call e3 |> make_block), r
       in
       before1@[cond]@after1

    | E_binary_conditional (e1,e2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let cond = S_if (e1,
                        [] |> make_block,
                        simplify_expr_stmt ctx f call e2 |> make_block), r
       in
       before1@[cond]@after1

    | E_assign _ ->
       let before, e, after = simplify_expr ctx f call (e,t,r) in
       before@after

    | E_compound_assign _
    | E_increment _ ->
       let before, e, after = simplify_expr ctx f call (e,t,r) in
       before@after

   | E_comma (e1,e2) ->
      let b1 = simplify_expr_stmt ctx f call e1 in
      let b2 = simplify_expr_stmt ctx f call e2 in
      b1@b2

   | E_call _
   | E_unary _
   | E_binary _
   | E_array_subscript _
   | E_member_access _
   | E_arrow_access _
   | E_address_of  _
   | E_deref _
   | E_cast _
   | E_character_literal _
   | E_integer_literal _
   | E_float_literal _
   | E_string_literal _
   | E_variable _
   | E_function _
   | E_predefined _
   | E_var_args _
   | E_atomic _
   | E_compound_literal _
   | E_convert_vector _
   | E_vector_element _
   | E_shuffle_vector _
     ->
       let before,e,after = simplify_expr ctx f call (e,t,r) in
       before@[S_expression e, r]@after

   | E_statement b ->
      List.concat (List.map (simplify_stmt ctx f call) b.blk_stmts)


  and simplify_init ctx f (call:bool) (i:init) : (statement list) * init * (statement list) =
    match i with
    | I_init_expr e1 ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       before1, I_init_expr e1, after1

    | I_init_list (l1,opt) ->
       let before, l, after =
         List.fold_left (fun (before,l,after) i ->
             let before', i', after' = simplify_init ctx f call i in
             before'@before, i'::l, after'@after
           ) ([],[],[]) l1
       in
       before, I_init_list (List.rev l, opt), after

    | I_init_implicit _ -> [], i, []


  (* simplify the expressions inside a statement *)
  and simplify_stmt ctx f (call:bool) ((s,r):statement) : statement list =
    match s with
    | S_local_declaration v ->
       (match v.var_init with
        | None -> [s,r]
        | Some i ->
           let before, i, after = simplify_init ctx f call i in
           v.var_init <- Some i;
           before@[s,r]@after
       )

    | S_expression e ->
       scope_temp r (simplify_expr_stmt ctx f call e)

    | S_block b ->
       [S_block (simplify_block ctx f call b),r]

    | S_if (e1,b1,b2) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before1, e1 = remove_after ctx f before1 e1 after1 in
       scope_temp r (before1@[S_if (e1,
                                    simplify_block ctx f call b1,
                                    simplify_block ctx f call b2), r])

    | S_while (e1,b1) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let e1 = as_expr ctx f before1 e1 after1 in
       [S_while (e1, simplify_block ctx f call b1), r]

    | S_do_while (b1,e1) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let e1 = as_expr ctx f before1 e1 after1 in
       [S_do_while (simplify_block ctx f call b1, e1), r]

    | S_for (b1,e2,e3,b4) ->
       let e2 = match e2 with
         | None -> None
         | Some e2 ->
            let before2, e2, after2 = simplify_expr ctx f call e2 in
            Some (as_expr ctx f before2 e2 after2)
       and e3 = match e3 with
         | None -> None
         | Some ((_,t3,r3) as e3) ->
            Some (as_expr_stmt (simplify_expr_stmt ctx f call e3) t3 r3)
       in
       [S_for (simplify_block ctx f call b1, e2, e3, simplify_block ctx f call b4), r]

    | S_jump (S_return (Some e1, u)) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before1, e1 = remove_after ctx f before1 e1 after1 in
       scope_temp r (before1@[S_jump (S_return (Some e1, u)), r])

    | S_jump (S_switch (e1,b1)) ->
       let before1, e1, after1 = simplify_expr ctx f call e1 in
       let before1, e1 = remove_after ctx f before1 e1 after1 in
       scope_temp r (before1@[S_jump (S_switch (e1, simplify_block ctx f call b1)), r])

    | S_jump _ | S_target _ | S_asm _ ->
       [s,r]


  and simplify_block ctx f (call:bool) (b:block) : block =
    make_block (List.concat (List.map (simplify_stmt ctx f call) b.blk_stmts))

  (* use a temporary to remove the 'after' part of a triple *)
  (* before; e; after -> before; tmp = e; after; <e> *)
  and remove_after ctx f before ((_,t,r) as e) after : statement list * expr =
    if after = [] then before, e
    else
      let tmp = make_temp ctx r f t in
      let tmp_var = E_variable tmp, t, r in
      let create = S_local_declaration tmp, r in
      let assign = S_expression (E_assign (tmp_var,e), t, r), r in
      before@[create;assign]@after, tmp_var

  (* converts a triple to an expression, using statement expressions *)
  and as_expr ctx f before ((_,t,r) as e) after : expr =
    let before, e = remove_after ctx f before e after in
    if before = [] then e
    else E_statement (before@[S_expression e, r] |> make_block), t, r


(*****************)
(** Entry points *)
(*****************)

(*
  Removed:
  - E_conditional
  - E_binary_conditional
  - E_compound_assign
  - E_comma
  - E_increment
  - E_compound_literal
 *)

let simplify_func ctx (f:func) =
  match f.func_body with
  | Some body -> f.func_body <- Some (simplify_block ctx (Some f) false body |> resolve_scope);
  | None -> ()

let simplify_global_init ctx (i:init) : (statement list) * init * (statement list) =
  simplify_init ctx None false i
