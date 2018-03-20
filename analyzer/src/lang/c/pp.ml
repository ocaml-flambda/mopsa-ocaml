open Format
open Framework.Pp
open Framework.Ast
open Ast

let () =
  register_pp_expr (fun default fmt expr ->
      match ekind expr with
      | E_c_addrof e -> fprintf fmt "&(%a)" pp_expr e
      | E_c_deref e -> fprintf fmt "*(%a)" pp_expr e
      | _ -> default fmt expr
    );
  register_pp_typ (fun default fmt typ ->
      match typ with
      | TClang (u) -> Format.fprintf fmt "TClang(%s)"
                        (C_print.string_of_type_qual u)
      | _ -> default fmt typ
    );
  register_pp_stmt (fun default fmt stmt ->
      match skind stmt with
      | S_c_for (init,cond,it,stmts) ->
        fprintf fmt "@[<v 2>for (%a;%a;%a) {@,%a@]@,}"
          Framework.Pp.pp_stmt init
          (Printers.print_option Framework.Pp.pp_expr) cond
          (Framework.Pp.pp_stmt) it
          Framework.Pp.pp_stmt stmts
      | _ -> default fmt stmt
    );
  Framework.Pp.register_pp_program (fun default fmt prg ->
      match prg with
      | Ast.P_c_program prg -> assert false
      | _ -> default fmt prg
    );
