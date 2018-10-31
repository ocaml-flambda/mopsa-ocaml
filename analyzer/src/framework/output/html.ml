module Make(Domain: Domain.DOMAIN) =
struct

exception NoCloser
  type eval_beg =
    { e : Ast.expr ; zs : Zone.zone * Zone.zone }
  type exec_beg =
    { s : Ast.stmt ; z : Zone.zone }
  type eval_done =
    { e_res : (Domain.t, Ast.expr) Manager.evl}
  type exec_done =
    { s_res : Domain.t Flow.flow}

  type action =
    | Eval of eval_beg
    | Exec of exec_beg
    | EvalDone of eval_done
    | ExecDone of exec_done

  type action_tree =
    | Act_eval of eval_beg * eval_done * action_tree list
    | Act_exec of exec_beg * exec_done * action_tree list

let parse_action_list (al: action list) =
  let rec aux al atl =
    match al with
    | (EvalDone(eval_end) as hd)::tl -> atl, Some hd, tl
    | (Eval(eval_beg))::tl ->
      begin
        let atl', hd, tl = aux tl [] in
        match hd with
        | Some (EvalDone(eval_end)) ->
          begin
            aux tl (Act_eval(eval_beg, eval_end, List.rev_append atl' [])::atl)
          end
        | _ -> failwith "bad_parenthesis"
      end
    | (ExecDone(exec_end) as hd)::tl -> atl, Some hd, tl
    | (Exec(exec_beg))::tl ->
      begin
        let atl', hd, tl = aux tl [] in
        match hd with
        | Some (ExecDone(exec_end)) ->
          begin
            aux tl (Act_exec(exec_beg, exec_end, List.rev_append atl' [])::atl)
          end
        | _ -> failwith "bad_parenthesis"
      end
    | [] -> List.rev_append atl [], None, []
  in
  let res, _, _ = aux al [] in
  res
  let print_action fmt a = match a with
    | Eval(e) -> Format.fprintf fmt "<E %a>" Ast.pp_expr e.e
    | EvalDone(_) -> Format.fprintf fmt "</E>"
    | Exec(s) -> Format.fprintf fmt "<S %a>" Ast.pp_stmt s.s
    | ExecDone(_) -> Format.fprintf fmt "</S>"

  let counter = ref 0
  let fresh () = incr counter; !counter

  let rec dump_html_of_tree_list fmt l =
    Format.fprintf fmt "@[<v 2>%a@]"
      (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
         dump_html_of_tree
      ) l

  and dump_html_of_tree fmt t =
    match t with
    | Act_eval (eb, ed, []) ->
      Format.fprintf fmt "<li class=\"file\">eval %a</li>" pp_eb_ed (eb, ed)
    | Act_exec (sb, sd, []) ->
      Format.fprintf fmt "<li class=\"file\">exec %a</li>" pp_sb_sd (sb, sd)
    | Act_eval (eb, ed, l) ->
      let cont = fresh () in
      Format.fprintf fmt "@[<v 2> <li>@,<label \
                          for=\"tok%d\">%a</label>@,<input \
                          type=\"checkbox\" id=\"tok%d\" />
                @[<v 2> <ol> @,@[<v 2>%a@]@,</ol>@,@]</li>@]"
        cont
        pp_eb_ed (eb, ed)
        cont
        dump_html_of_tree_list l
    | Act_exec (sb, sd, l) ->
      let cont = fresh () in
      Format.fprintf fmt "@[<v 2> <li>@,<label \
                          for=\"tok%d\">%a</label>@,<input \
                          type=\"checkbox\" id=\"tok%d\" />
                @[<v 2> <ol> @,@[<v 2>%a@]@,</ol>@,@]</li>@]"
        cont
        pp_sb_sd (sb, sd)
        cont
        dump_html_of_tree_list l

  and pp_eb_ed fmt (eb, ed) =
    Format.fprintf fmt "%a" Ast.pp_expr eb.e
  and pp_sb_sd fmt (sb, sd) =
    Format.fprintf fmt "%a" Ast.pp_stmt sb.s

  let actions = ref []

  let push_action (a: action): unit =
    if Options.common_options.html_output_path <> "" then
      actions := a :: !actions

  let dump_html_actions () =
    if Options.common_options.html_output_path <> "" then
      begin
        let oo = open_out Options.common_options.html_output_path in
        let outfmt = Format.formatter_of_out_channel oo in
        let atl = parse_action_list (List.rev_append !actions []) in
        Format.fprintf outfmt "<head><link rel=\"stylesheet\" type=\"text/css\" \
                              href=\"droppable.css\"></head>@[<v 2><ol \
                              class=\"tree\">@,%a@,</ol>"
          dump_html_of_tree_list atl;
        close_out oo
      end

  let rec print_action_tree fmt t = match t with
    | Act_eval(b, e, atl) -> Format.fprintf fmt "@[<v 2> %a@,{%a}@]" Ast.pp_expr b.e (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") print_action_tree) atl
    | Act_exec(b, e, atl) -> Format.fprintf fmt "@[<v 2> %a@,{%a}@]" Ast.pp_stmt b.s (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") print_action_tree) atl

  let print_action_tree_list fmt l =
    Format.fprintf fmt "%a" (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,") print_action_tree) l

end
