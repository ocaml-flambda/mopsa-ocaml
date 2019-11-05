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

let create_context (target:C.target_info) = {
    uid = 0;
    target = target;
  }


let new_uid ctx =
  ctx.uid <- ctx.uid + 1;
  ctx.uid


let make_temp ctx range ?(com:comment list=[]) (f:func) (t:type_qual) : variable =
  let u = new_uid ctx in
  let v = {
      var_uid = u;
      var_org_name = tmp_name;
      var_unique_name = Printf.sprintf "%s_%i" tmp_name u;
      var_kind = Variable_local f;
      var_type = t;
      var_init = None;
      var_range = range;
      var_com = com;
    }
  in
  f.func_local_vars <- v::f.func_local_vars;
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
     expr_float_cst range f 0.
  | T_pointer _ ->
     expr_integer_cst range (ptrdiff_type target) Z.one
  | T_bitfield (t,_) ->
     expr_one target range t
  | T_enum u ->
     expr_one target range (T_integer u.enum_integer_type)
  | T_typedef d ->
     expr_one target range (fst d.typedef_def)
  | _ ->
     error range "cannot increment type" (C_print.string_of_type t)


(*
  Removed:
  - E_conditional
  - E_compound_assign
  - E_comma
  - E_increment
  - E_compound_literal
  - shortcut operators &&, || (TODO)

 *)

let simplify_func ctx (f:func) =

  (* simplify an expression
     we return triples:
     - statements to execute before evaluating ( temp variable declaration, etc.)
     - simplified expression
     - statements to execute after evaluating
     - if call=true, assign the reult of function calls to temporaries
       (necessary in some cases to ensure that the side-effect is only
        evaluated once)
   *)
  let rec simplify_expr (call:bool) ((e,t,r):expr)
          : (statement list) * (expr) * (statement list) =
    match e with

    (* e1 ? e2 : e3 -> if (e1) tmp = e2; else tmp = e3; <tmp> *)
    | E_conditional (e1,e2,e3) ->
       let before1, e1, after1 = simplify_expr call e1 in
       if is_void t then
         let cond = S_if (e1,
                          simplify_expr_stmt call e2,
                          simplify_expr_stmt call e3), r in
         before1@[cond], expr_void r, after1
       else
         let before2, e2, after2 = simplify_expr call e2 in
         let before3, e3, after3 = simplify_expr call e3 in
         let tmp = make_temp ctx r f t in
         let tmp_var = E_variable tmp, t, r in
         let create = S_local_declaration tmp, r in
         let cond =
           S_if (e1,
                 before2@[S_expression (E_assign (tmp_var, e2), t, r), r]@after2,
                 before3@[S_expression (E_assign (tmp_var, e3), t, r), r]@after3), r
         in
         before1@[create;cond], tmp_var, after1

    | E_array_subscript(e1,e2) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before2, e2, after2 = simplify_expr call e2 in
       before1@before2, (E_array_subscript(e1,e2), t, r), after2@after1

    | E_member_access (e1,i,f) ->
       let before1, e1, after1 = simplify_expr call e1 in
       before1, (E_member_access (e1,i,f), t, r), after1

    | E_arrow_access (e1,i,f) ->
       let before1, e1, after1 = simplify_expr call e1 in
       before1, (E_arrow_access (e1,i,f), t, r), after1

    (* e1 op= e2 -> e1 = e1 op e2; <e1> *)
    | E_compound_assign (e1,t1,op,e2,t2) ->
       (* force temporary for function call as e1 is used twice *)
       let before1, e1, after1 = simplify_expr true e1 in
       let before2, e2, after2 = simplify_expr call e2 in
       let before = before1@before2 and after = after2@after1 in
       let e1t1 = E_cast (e1, IMPLICIT), t1, r in
       let e12 = E_binary (O_arithmetic op, e1t1, e2), t2, r in
       let e12t = E_cast (e12, IMPLICIT), t, r in
       let assign = S_expression (E_assign (e1, e12t), t, r), r in
       before@[assign], e1, after

    | E_binary (O_logical LOGICAL_OR, e1, e2) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before2, e2, after2 = simplify_expr call e2 in
       if before2 = [] && after2 = [] then
         before1, (E_binary (O_logical LOGICAL_OR, e1, e2), t, r), after1
       else
         let tmp = make_temp ctx r f bool_type in
         let tmp_var = E_variable tmp, bool_type, r in
         let create = S_local_declaration tmp, r in
         let cond =
           S_if (e1,
                 [S_expression (E_assign (tmp_var, expr_bool_true r), t, r), r],
                 before2@[S_expression (E_assign (tmp_var, e2), t, r), r]@after2), r
         in
         before1@[create;cond], tmp_var, after1

    | E_binary (O_logical LOGICAL_AND, e1, e2) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before2, e2, after2 = simplify_expr call e2 in
       if before2 = [] && after2 = [] then
         before1, (E_binary (O_logical LOGICAL_AND, e1, e2), t, r), after1
       else
         let tmp = make_temp ctx r f bool_type in
         let tmp_var = E_variable tmp, bool_type, r in
         let create = S_local_declaration tmp, r in
         let cond =
           S_if (e1,
                 before2@[S_expression (E_assign (tmp_var, e2), t, r), r]@after2,
                 [S_expression (E_assign (tmp_var, expr_bool_false r), t, r), r]), r
         in
         before1@[create;cond], tmp_var, after1

    | E_binary (op,e1,e2) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before2, e2, after2 = simplify_expr call e2 in
       let before = before1@before2 and after = after2@after1 in
       before, (E_binary (op,e1,e2), t, r), after

    (* e1 = e2 -> e1 = e2; <e1> *)
    | E_assign (e1,e2) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before2, e2, after2 = simplify_expr call e2 in
       let before = before1@before2 and after = after2@after1 in
       let assign = S_expression (E_assign (e1,e2), t, r), r in
       before@[assign], e1, after

    (* e1,e2 -> e1; <e2> *)
    | E_comma (e1,e2) ->
      let b1 = simplify_expr_stmt call e1 in
      let before2, e2, after2 = simplify_expr call e2 in
      b1@before2, e2, after2

   | E_unary (op,e1) ->
      let before1, e1, after2 = simplify_expr call e1 in
      before1, (E_unary (op,e1), t, r), after2

   (* ++e1 -> e1 = e1 + 1; <e1> *)
   | E_increment (dir,PRE,e1) ->
      (* force temporary for function call as e1 is used twice *)
      let before1, e1, after1 = simplify_expr true e1 in
      let ep = int_promote ctx.target e1 in
      let op = O_arithmetic (if dir = INC then ADD else SUB) in
      let e1p = E_binary (op, ep, expr_one ctx.target r (fst t)), t, r in
      let e1c = E_cast (e1p, IMPLICIT), t, r in
      let inc = S_expression (E_assign (e1,e1c), t, r), r in
      before1@[inc], e1, after1

   (* e1++ -> <e1>; e1 = e1 + 1 *)
   | E_increment (dir,POST,e1) ->
      (* force temporary for function call as e1 is used twice *)
      let before1, e1, after1 = simplify_expr true e1 in
      let ep = int_promote ctx.target e1 in
      let op = O_arithmetic (if dir = INC then ADD else SUB) in
      let e1p = E_binary (op, ep, expr_one ctx.target r (fst t)), t, r in
      let e1c = E_cast (e1p, IMPLICIT), t, r in
      let inc = S_expression (E_assign (e1,e1c), t, r), r in
      before1, e1, after1@[inc]
      
   | E_address_of e1 ->
      let before1, e1, after1 = simplify_expr call e1 in
      before1, (E_address_of e1, t, r), after1

   | E_deref e1 ->
      let before1, e1, after1 = simplify_expr call e1 in
      before1, (E_deref e1, t, r), after1

   | E_cast (e1,x) ->
      let before1, e1, after1 = simplify_expr call e1 in
      before1, (E_cast (e1,x), t, r), after1

   (* if call=true: f(...) -> tmp = f(...); <tmp> *)
   | E_call (e1,ea) ->
      let before1, e1, after1 = simplify_expr call e1 in
      let ea = Array.copy ea in
      let beforea, aftera = ref before1, ref after1 in
      for i=0 to Array.length ea-1 do
        let before, ee, after = simplify_expr call ea.(i) in
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
      let before1, e1, after1 = simplify_expr call e1 in
      before1, (E_var_args e1, t, r), after1

   | E_atomic (i,e1,e2) ->
      let before1, e1, after1 = simplify_expr call e1 in
      let before2, e2, after2 = simplify_expr call e2 in
      let before = before1@before2 and after = after2@after1 in
      before, (E_atomic (i,e1,e2), t, r), after

   | E_compound_literal i ->
      let before1, i, after1 = simplify_init call i in
      let tmp = make_temp ctx r f t in
      let tmp_var = E_variable tmp, t, r in
      let create = S_local_declaration tmp, r in
      tmp.var_init <- Some i;
      before1@[create], tmp_var, after1

   | E_statement b ->
      let rec doit acc = function
        | [S_expression e1, r] ->
           let before1, e1, after1 = simplify_expr call e1 in
           let before1, e1 = remove_after before1 e1 after1 in
           acc@before1, e1, []
        | s::rest ->
           doit (acc@(simplify_stmt call s)) rest
        | [] ->
           acc, expr_void r, []
      in
      doit [] b


  (* case of toplevel (statement) expressions: the returned value is not used *)
  and simplify_expr_stmt (call:bool) ((e,t,r):expr) : statement list =
    match e with

    | E_conditional (e1,e2,e3) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let cond = S_if (e1,
                        simplify_expr_stmt call e2,
                        simplify_expr_stmt call e3), r in
       before1@[cond]@after1

    | E_assign _ ->
       let before, e, after = simplify_expr call (e,t,r) in
       before@after

    | E_compound_assign _
    | E_increment _ ->
       let before, e, after = simplify_expr call (e,t,r) in
       before@after

   | E_comma (e1,e2) ->
      let b1 = simplify_expr_stmt call e1 in
      let b2 = simplify_expr_stmt call e2 in
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
   | E_compound_literal _ ->
       let before,e,after = simplify_expr call (e,t,r) in
       before@[S_expression e, r]@after

   | E_statement b ->
      List.concat (List.map (simplify_stmt call) b)

  and simplify_init (call:bool) (i:init) : (statement list) * init * (statement list) =
    match i with
    | I_init_expr e1 ->
       let before1, e1, after1 = simplify_expr call e1 in
       before1, I_init_expr e1, after1

    | I_init_list (l1,opt) ->
       let before, l, after =
         List.fold_left (fun (before,l,after) i ->
             let before', i', after' = simplify_init call i in
             before'@before, i'::l, after'@after
           ) ([],[],[]) l1
       in
       before, I_init_list (List.rev l, opt), after

    | I_init_implicit _ -> [], i, []


  (* simplify the expressions inside a statement *)
  and simplify_stmt (call:bool) ((s,r):statement) : statement list =
    match s with
    | S_local_declaration v ->
       (match v.var_init with
        | None -> [s,r]
        | Some i ->
           let before, i, after = simplify_init call i in
           v.var_init <- Some i;
           before@[s,r]@after
       )

    | S_expression e ->
       scope_temp r (simplify_expr_stmt call e)

    | S_block b ->
       [S_block (simplify_block call b),r]

    | S_if (e1,b1,b2) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before1, e1 = remove_after before1 e1 after1 in
       scope_temp r (before1@[S_if (e1,
                                    simplify_block call b1,
                                    simplify_block call b2), r])

    | S_while (e1,b1) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let e1 = as_expr before1 e1 after1 in
       [S_while (e1, simplify_block call b1), r]

    | S_do_while (b1,e1) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let e1 = as_expr before1 e1 after1 in
       [S_do_while (simplify_block call b1, e1), r]

    | S_for (b1,e2,e3,b4) ->
       let e2 = match e2 with
         | None -> None
         | Some e2 ->
            let before2, e2, after2 = simplify_expr call e2 in
            Some (as_expr before2 e2 after2)
       and e3 = match e3 with
         | None -> None
         | Some ((_,t3,r3) as e3) ->
            Some (as_expr_stmt (simplify_expr_stmt call e3) t3 r3)
       in
       [S_for (simplify_block call b1, e2, e3, simplify_block call b4), r]

    | S_jump (S_return (Some e1)) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before1, e1 = remove_after before1 e1 after1 in
       scope_temp r (before1@[S_jump (S_return (Some e1)), r])

    | S_jump (S_switch (e1,b1)) ->
       let before1, e1, after1 = simplify_expr call e1 in
       let before1, e1 = remove_after before1 e1 after1 in
       scope_temp r (before1@[S_jump (S_switch (e1, simplify_block call b1)), r])

    | S_jump _ | S_target _ ->
       [s,r]

  and simplify_block (call:bool) (l:statement list) : (statement list) =
    List.concat (List.map (simplify_stmt call) l)

  (* use a temporary to remove the 'after' part of a triple *)
  (* before; e; after -> before; tmp = e; after; <e> *)
  and remove_after before ((_,t,r) as e) after : statement list * expr =
    if after = [] then before, e
    else
      let tmp = make_temp ctx r f t in
      let tmp_var = E_variable tmp, t, r in
      let create = S_local_declaration tmp, r in
      let assign = S_expression (E_assign (tmp_var,e), t, r), r in
      before@[create;assign]@after, tmp_var

  (* converts a triple to an expression, using statement expressions *)
  and as_expr before ((_,t,r) as e) after : expr =
    let before, e = remove_after before e after in
    if before = [] then e
    else E_statement (before@[S_expression e, r]), t, r

  (* converts back a statement list to an expression *)
  and as_expr_stmt l t r : expr =
    match l with
    | [S_expression e,_] -> e
    | [] -> expr_void r
    | _ -> E_statement l, t, r

  (* encolse in a S_block if the block contains variable declaration to limit their scope *)
  and scope_temp range (b:block) : block =
    let has_decl = List.exists (function (S_local_declaration _,_) -> true | _ -> false) b in
    if has_decl then [S_block b,range] else b

  in
  match f.func_body with
  | Some body -> f.func_body <- Some (simplify_block false body)
  | None -> ()
