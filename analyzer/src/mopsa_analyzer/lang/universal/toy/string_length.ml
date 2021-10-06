open Mopsa
open Sig.Abstraction.Stateless
open Ast

type check +=
   | CHK_U_SUBSCRIPT_ACCESS

type alarm_kind += A_u_invalid_subscript of expr


let () =
  register_check (fun default fmt a ->
      match a with
      | CHK_U_SUBSCRIPT_ACCESS -> Format.fprintf fmt "Subscript access"
      | _ -> default fmt a);
  register_alarm {
      check = (fun next -> function
                | A_u_invalid_subscript _ -> CHK_U_SUBSCRIPT_ACCESS
                | a -> next a);
      compare = (fun default a a' ->
        match a, a' with
        | A_u_invalid_subscript e1, A_u_invalid_subscript e2 ->
           compare_expr e1 e2
        | _ -> default a a');
      print = (fun default fmt a ->
        match a with
        | A_u_invalid_subscript e -> Format.fprintf fmt "Invalid subscript access %a" pp_expr e
        | _ -> default fmt a);
      join = (fun next a1 a2 ->
        match a1, a2 with
        | A_u_invalid_subscript _, A_u_invalid_subscript e2 ->
           if compare_alarm_kind a1 a2 = 0 then Some a1 else None
        | _ -> next a1 a2);
    }

let safe_subscript_access_check exp flow =
  Flow.add_safe_check CHK_U_SUBSCRIPT_ACCESS exp.erange flow

let invalid_subscript_access_alarm exp flow lattice =
  let cs = Flow.get_callstack flow in
  Flow.raise_alarm (mk_alarm (A_u_invalid_subscript exp) cs exp.erange) ~bottom:false lattice flow

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "universal.toy.string_length"
    end)

  let mk_len_string s = mk_attr_var s "len" T_int

  let checks = []


  let init (prog:program) man flow = flow

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_len {ekind = E_binop(O_concat, {ekind = E_var (v1, _)}, {ekind = E_var (v2, _)})} ->
       Eval.singleton (mk_binop ~etyp:T_int (mk_var (mk_len_string v1) range) O_plus (mk_var (mk_len_string v2) range) range) flow
       |> OptionExt.return

    | E_len ({ekind = E_var (s, _)}) ->
       Eval.singleton (mk_var (mk_len_string s) range) flow |>
         OptionExt.return

    | E_len ({ekind = E_constant (C_string s)}) ->
       Eval.singleton (mk_int (String.length s) range) flow |>
         OptionExt.return

    | E_subscript({ekind = E_var (s, _)}, i) ->
       (
         assume (mk_log_and (mk_le (mk_zero range) i range) (mk_lt i (mk_var (mk_len_string s) range) range) range) man flow
           ~fthen:(fun flow ->
             let flow = safe_subscript_access_check exp flow in
             Cases.return (mk_int_interval 0 127 range) flow)
           ~felse:(fun flow -> Cases.empty (invalid_subscript_access_alarm exp flow man.lattice))
       ) |> OptionExt.return

    | _ -> None

  let exec stmt man flow =
    let range = srange stmt in
    match skind stmt with
    | S_assign ({ekind = E_var (s, _); etyp=T_string}, e) ->
       (eval (mk_expr ~etyp:T_int (E_len e) range) man flow |> OptionExt.none_to_exn >>$ fun le flow ->
       man.exec (mk_assign (mk_var (mk_len_string s) range) le range) flow)
       |> OptionExt.return

    | S_assign ({ekind = E_subscript ({ekind = E_var (s, _)}, i); etyp=T_int}, e) ->
       assume (mk_log_and
                 (mk_log_and (mk_le (mk_zero range) e range) (mk_le e (mk_int 127 range) range) range)
                 (mk_log_and (mk_le (mk_zero range) i range) (mk_le i (mk_var (mk_len_string s) range) range) range)
                 range) man flow
         ~fthen:(fun flow ->  Post.return flow)
         ~felse:(Cases.empty)
       |> OptionExt.return



    | _ -> None

  let ask : type r. ('a, r) query -> ('a, unit) man -> 'a flow -> r option =
    fun query man flow -> None

  let print_expr man flow printer exp = ()

end

let () =
  register_stateless_domain (module Domain)
