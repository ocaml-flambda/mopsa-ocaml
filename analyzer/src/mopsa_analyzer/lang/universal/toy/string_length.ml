open Mopsa
open Sig.Abstraction.Stateless
open Ast

module Domain =
struct

  include GenStatelessDomainId(struct
      let name = "universal.toy.string_length"
    end)

  let mk_len_string s = mk_attr_var s "len" T_int

  let checks = []

  (* TODO *)
  let safe_subscript_access_check exp flow = flow
  let invalid_subscript_access_alarm exp flow = flow

  let init (prog:program) man flow = flow

  let eval exp man flow =
    let range = erange exp in
    match ekind exp with
    | E_len {ekind = E_binop(O_concat, {ekind = E_var (v1, _)}, {ekind = E_var (v2, _)})} ->
       Eval.singleton (mk_binop ~etyp:T_int (mk_var (mk_len_string v1) range) O_plus (mk_var (mk_len_string v2) range) range) flow
       |> OptionExt.return

    (* | E_len {ekind = E_binop(O_concat, e1, e2)} when etyp e1 = T_string && etyp e2 = T_string ->
     *    (man.eval e1 flow >>$ fun e1 flow ->
     *    man.eval e2 flow >>$ fun e2 flow ->
     *    match ekind e1, ekind e2 with
     *    | E_var (v1, _), E_var (v2, _) ->
     *       Eval.singleton (mk_binop ~etyp:T_int (mk_var (mk_len_string v1) range) O_plus (mk_var (mk_len_string v2) range) range) flow
     *    | _, _ ->
     *       Eval.singleton (mk_binop ~etyp:T_int (mk_expr (E_len e1) range) O_plus (mk_expr (E_len e2) range) range) flow)
     *    |> OptionExt.return *)

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
           ~felse:(fun flow -> Cases.empty (invalid_subscript_access_alarm exp flow))
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
