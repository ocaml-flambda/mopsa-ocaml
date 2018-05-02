open Framework.Ast
open Framework.Flow
open Framework.Pp
open Universal.Ast
open Ast

type 'a reply =
  | Return of 'a flow
  | Deeper of int

type 'a init_fun = expr -> c_init option -> bool -> range -> 'a flow -> 'a reply

type 'a init_manager = {
  scalar : 'a init_fun;
  array  : 'a init_fun;
  strct  : 'a init_fun;
}


(** Initialization of scalar expressions *)
let rec init_scalar man v init is_global range flow =
  debug "init scalar %a" Framework.Pp.pp_expr v;
  match init with
  | None when not is_global -> flow

  | _ ->
    match man.scalar v init is_global range flow with
    | Return flow -> flow
    | Deeper _ -> Framework.Exceptions.fail "scalars can not be initialized deeper"


and init_array man a init is_global range flow =
  debug "init array %a" Framework.Pp.pp_expr a;
  match init with
  | None when not is_global -> flow

  | None when is_global ->
    begin
      match man.array a init is_global range flow with
      | Return flow -> flow
      | Deeper d ->
        let n = get_array_constant_length a.etyp in
        let rec aux i flow =
          if i = n || i = d then flow
          else
            let flow = init_expr man (mk_c_subscript_access a (mk_int i range) range) None is_global range flow in
            aux (i + 1) flow
        in
        aux 0 flow
    end

  | Some (C_init_list (l, filler)) ->
    begin
      match man.array a init is_global range flow with
      | Return flow -> flow
      | Deeper d ->
        let n = get_array_constant_length a.etyp in
        let rec aux i flow =
          if i = n || i = d then flow
          else
            let init = if i < List.length l then Some (List.nth l i) else filler in
            let flow = init_expr man (mk_c_subscript_access a (mk_int i range) range) init is_global range flow in
            aux (i + 1) flow
        in
        aux 0 flow
    end


  | Some (Ast.C_init_expr {ekind = E_constant(C_c_string (s, _))}) ->
    begin
      match man.array a init is_global range flow with
      | Return flow -> flow
      | Deeper d ->
        let n = get_array_constant_length a.etyp in
        let rec aux i flow =
          if i = n || i = d then flow
          else
            let init = if i < String.length s then Some (C_init_expr (mk_c_character (String.get s i) range)) else Some (C_init_expr (mk_c_character (char_of_int 0) range)) in
            let flow = init_expr man (mk_c_subscript_access a (mk_int i range) range) init is_global range flow in
            aux (i + 1) flow
        in
        aux 0 flow
    end

  | _ ->
    Framework.Exceptions.panic "Array initialization not supported"

and init_union  man u init is_global range flow =
  debug "init union %a" Framework.Pp.pp_expr u;
  let largest_field =
    let fields = match remove_typedef u.etyp |> remove_qual with
      | T_c_record{c_record_fields} -> c_record_fields
      | _ -> assert false
    in
    match fields with
    | [] -> assert false
    | [f] -> f
    | hd :: tl ->
      let rec doit acc = function
        | [] -> acc
        | f :: tl ->
          let acc = if Z.gt (sizeof_type f.c_field_type) (sizeof_type acc.c_field_type) then f else acc in
          doit acc tl
      in
      doit hd tl
  in

  match init with
  | None when not is_global -> flow

  | None when is_global ->
    debug "initialization of uninitialized global";
    debug "largest_field = %a" Framework.Pp.pp_expr (mk_c_member_access u largest_field range);
    init_expr man (mk_c_member_access u largest_field range) None is_global range flow

  | _ -> Framework.Exceptions.panic "Initialization of union not supported"


and init_struct man s init is_global range flow =
  debug "init struct %a" Framework.Pp.pp_expr s;
  let get_nth_field n =
    match remove_typedef s.etyp |> remove_qual with
    | T_c_record{c_record_kind = C_struct; c_record_fields} -> List.nth c_record_fields n
    | _ -> assert false
  in
  let nb_fields =
    match remove_typedef s.etyp |> remove_qual with
    | T_c_record{c_record_kind = C_struct; c_record_fields} -> List.length c_record_fields
    | _ -> assert false
  in
  match init with
  | None when not is_global -> flow

  | None when is_global ->
    begin
      match man.strct s init is_global range flow with
      | Return flow -> flow
      | Deeper d ->
        let rec aux i flow =
          if i = nb_fields || i = d then flow
          else
            let flow = init_expr man (mk_c_member_access s (get_nth_field i) range) None is_global range flow in
            aux (i + 1) flow
        in
        aux 0 flow
    end

  | Some (C_init_list(l, None)) ->
    begin
      match man.strct s init is_global range flow with
      | Return flow -> flow
      | Deeper d ->
        let rec aux i flow =
          if i = nb_fields then flow
          else
            let init = if i < List.length l then Some (List.nth l i) else None in
            let flow = init_expr man (mk_c_member_access s (get_nth_field i) range) init is_global range flow in
            aux (i + 1) flow
        in
        aux 0 flow
    end

  | Some (C_init_expr e) ->
    begin
      match man.strct s init is_global range flow with
      | Return flow -> flow
      | Deeper _ -> Framework.Exceptions.panic "memory.init: can not go deeper in init of struct with expression"
    end

  | _ -> assert false

and init_expr man e (init: c_init option) is_global range flow =
  if is_c_scalar_type e.etyp then init_scalar man e init is_global range flow else
  if is_c_array_type e.etyp then  init_array man e init is_global range flow else
  if is_c_struct_type e.etyp then init_struct man e init is_global range flow else
  if is_c_union_type e.etyp then init_union man e init is_global range flow else
    Framework.Exceptions.panic "Unsupported initialization of %a" pp_expr e


let fold_globals man globals ctx flow =
  (* Initialize string symbols as global variables *)
  let range = mk_fresh_range () in
  let table = Program.find_string_table ctx in
  let globals = Program.StringTable.fold (fun s v acc ->
      let init = C_init_expr (mk_c_string s range) in
      (v, Some init) :: acc
    ) table globals
  in
  (* Initialize global variables *)
  List.fold_left (fun flow (v, init) ->
      let range = mk_fresh_range () in
      let v = mk_var v range in
      init_expr man v init true range flow
    ) flow globals

let init_local man v init range flow =
  assert false
