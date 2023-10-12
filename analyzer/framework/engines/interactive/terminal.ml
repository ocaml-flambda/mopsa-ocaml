open Core.All
open Mopsa_utils
open Location
open Callstack
open Interface
open Format
open Breakpoint
open Toplevel

module Make(Toplevel : TOPLEVEL) =
struct

  (** Commands *)
  type terminal_command =
    | Break of string
    (** Add a breakpoint *)

    | Continue
    (** Stop at next breakpoint *)

    | Next
    (** Stop at next statement and skip function calls *)

    | Step
    (** Step into function calls  *)

    | Finish
    (** Finish current function *)

    | NextI
    (** Stop at next statement and skip nodes in the interpretation sub-tree *)

    | StepI
    (** Step into interpretation sub-tree  *)

    | Print of string list * (string * int) option
    (** Print the abstract state or the value of variables *)

    | Env of string list
    (** Print the current abstract environment, associated to token T_cur,
        eventually projected on a list of domains *)

    | Where
    (** Show current program point *)

    | Info of info_command
    (** Print extra information *)

    | Enable of enable_command
    (** Enable an option *)

    | Disable of enable_command
    (** Disable an option *)

    | Set of set_command * string
    (** Set an option *)

    | Unset of set_command
    (** Unset an option *)

    | BackTrace
    (** Print the callstack *)

    | Save of string
    (** Save the environment in a file *)

    | LoadScript of string


  (** Information sub-commands *)
  and info_command =
    | Alarms
    | Checks
    | Breakpoints
    | Tokens
    | Variables
    | Context

  (** Enable/Disable sub-commands *)
  and enable_command =
    | Hook of string

  (** Set/Unset sub-commands *)
  and set_command =
    | Debug
    | Script


  (** Print a command *)
  let pp_terminal_command fmt = function
    | Break loc   -> Format.fprintf fmt "break %s" loc
    | Continue    -> Format.pp_print_string fmt "continue"
    | Next        -> Format.pp_print_string fmt "next"
    | Finish      -> Format.pp_print_string fmt "finish"
    | NextI       -> Format.pp_print_string fmt "nexti"
    | Step        -> Format.pp_print_string fmt "step"
    | StepI       -> Format.pp_print_string fmt "stepi"
    | Print([], None)    -> Format.pp_print_string fmt "print"
    | Print(vars, None)  ->
      Format.fprintf fmt "print %a"
        (pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
           pp_print_string
        ) vars
    | Print(vars, Some(file, line))  ->
      Format.fprintf fmt "print@%s:%d %a"
        file line
        (pp_print_list
           ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
           pp_print_string
        ) vars
    | Env []           -> Format.pp_print_string fmt "env"
    | Env domains      -> Format.fprintf fmt "env %a"
                            (pp_print_list
                               ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                               pp_print_string
                            ) domains
    | Where            -> Format.pp_print_string fmt "where"
    | Info Alarms      -> Format.pp_print_string fmt "info alarms"
    | Info Checks      -> Format.pp_print_string fmt "info checks"
    | Info Breakpoints -> Format.pp_print_string fmt "info breakpoints"
    | Info Tokens      -> Format.pp_print_string fmt "info tokens"
    | Info Variables   -> Format.pp_print_string fmt "info variables"
    | Info Context     -> Format.pp_print_string fmt "info context"
    | Enable (Hook h)  -> Format.fprintf fmt "enable hook %s" h
    | Disable (Hook h) -> Format.fprintf fmt "disable hook %s" h
    | Set (Debug, d)   -> Format.fprintf fmt "set debug %s" d
    | Set (Script, d)  -> Format.fprintf fmt "set script %s" d
    | LoadScript s     -> Format.fprintf fmt "load script %s" s
    | Unset Debug      -> Format.pp_print_string fmt "unset debug"
    | Unset Script     -> Format.pp_print_string fmt "unset script"  | BackTrace        -> Format.pp_print_string fmt "backtrace"
    | Save file        -> Format.fprintf fmt "save %s" file


  (** Print help message *)
  let print_usage () =
    printf "Available commands:@.";
    printf "  b[reak] <[file:]line>     add a breakpoint at a line@.";
    printf "  b[reak] <function>        add a breakpoint at a function@.";
    printf "  c[ontinue]                run until next breakpoint@.";
    printf "  n[ext]                    stop at next statement and skip function calls.@.";
    printf "  n[ext]i                   stop at next statement and skip nodes in the interpretation sub-tree@.";
    printf "  s[tep]                    step into function calls@.";
    printf "  s[tep]i                   step into interpretation sub-tree@.";
    printf "  f[inish]                  finish current function@.";
    printf "  p[rint]                   print the abstract state@.";
    printf "  p[rint] <vars>            print the value of selected variables@.";
    printf "  p[rint] <vars> #<f>:<l>  print the value of selected variables at the given program location@.";
    printf "  e[nv]                     print the current abstract environment@.";
    printf "  e[nv] <domain>,...        print the current abstract environment of selected domains@.";
    printf "  b[ack]t[race]             print the current call stack@.";
    printf "  t[race]                   print the analysis trace@.";
    printf "  w[here]                   show current program point@.";
    printf "  w[here]i                  show current interpreter transfer function@.";
    printf "  i[info] a[larms]          print the list of detected alarms@.";
    printf "  i[info] c[hecks]          print the list of performed checks@.";
    printf "  i[info] b[reakpoints]     print the list of registered breakpoints@.";
    printf "  i[info] t[okens]          print the list of flow tokens@.";
    printf "  i[info] v[ariables]       print the list of variables@.";
    printf "  i[info] c[on]t[e]x[t]     print the flow-insensitive context@.";
    printf "  e[nable] h[hook] <h>      enable a hook@.";
    printf "  d[isable] h[hook] <h>     disable a hook@.";
    printf "  s[et] d[ebug] <d>         set debug channels@.";
    printf "  u[nset] d[ebug]           unset debug channels@.";
    printf "  s[et] script <file>       store commands into a file@.To be used in combination with load script <file>@.";
    printf "  u[nset] script            do not store commands in file anymore@.";
    printf "  load script <file>        reads script command from <file>@.";
    printf "  save <file>               save the abstract state in a file@.";
    printf "  mopsa_bt                  shows the current backtrace of the analyzer@.";
    printf "  help                      print this message@.";
    ()


  (** Print input prompt *)
  let print_prompt () =
    printf "%a %a @?"
      Debug.(color_str teal) "mopsa"
      Debug.(color_str green) ">>"


  (** Context of LineEdit library *)
  let linedit_ctx = LineEdit.create_ctx ()


  (** Reference to the last commands read from the prompt *)
  let last_prompt_commands = ref []

  (** Buffer of upcoming commands to execute *)
  let commands_buffer = Queue.create ()

  let script : out_channel option ref = ref None

  (* Read the next command as a string *)
  let rec read_terminal_command_string () =
    (* Check the commands buffer *)
    if not (Queue.is_empty commands_buffer) then
      Queue.pop commands_buffer
    else
      (* Buffer is empty, so ask the user *)
      let () = print_prompt () in
      let input = LineEdit.read_line linedit_ctx |>
                  String.trim in
      (* Split the input into multiple commands with delimeter ';' *)
      let parts = String.split_on_char ';' input |>
                  List.map String.trim |>
                  List.filter (function "" -> false | _ -> true) in
      match parts, !last_prompt_commands with
      | [], [] ->
        (* No command entered and no command in history, so as user again *)
        read_terminal_command_string ()

      | [], _ ->
        (* No command entered, but we know the last command, so replay it *)
        let () = List.iter (fun c -> Queue.add c commands_buffer) !last_prompt_commands in
        read_terminal_command_string ()

      | _ ->
        (* Some commands entered. Save them in [last_prompt_commands],
           initialize [commands_buffer] and iterate again *)
        last_prompt_commands := parts;
        List.iter (fun c -> Queue.add c commands_buffer) parts;
        read_terminal_command_string ()

  (** Read the next command *)
  let rec read_terminal_command logger =
    let s = read_terminal_command_string () in
    logger s;
    (* Get command's parts *)
    let parts = String.split_on_char ' ' s |>
                List.map String.trim |>
                List.filter (function "" -> false | _ -> true)
    in
    match parts with
    | ["continue" | "c"]   -> Continue
    | ["next"     | "n"]   -> Next
    | ["step"     | "s"]   -> Step
    | ["finish"   | "f"]   -> Finish
    | ["nexti"    |"ni"]   -> NextI
    | ["stepi"    |"si"]   -> StepI
    | ["where"    | "w"]   -> Where
    | ["backtrace"|"bt"]   -> BackTrace
    | ["break"    | "b"; l]-> Break l

    | ("env"      | "e") :: domains ->
      let domains =
        List.fold_left
          (fun acc s ->
             let parts = String.split_on_char ',' s |>
                         List.filter (function "" -> false | _ -> true)
             in
             SetExt.StringSet.union acc (SetExt.StringSet.of_list parts)
          ) SetExt.StringSet.empty domains
      in
      Env (SetExt.StringSet.elements domains)

    | ("print"    | "p") :: vars ->
      let vars =
        List.fold_left
          (fun acc s ->
             let parts = String.split_on_char ',' s |>
                         List.filter (function "" -> false | _ -> true)
             in
             SetExt.StringSet.union acc (SetExt.StringSet.of_list parts)
          ) SetExt.StringSet.empty vars
      in
      Print (SetExt.StringSet.elements vars, None)

    | ["help" | "h"]   ->
      print_usage ();
      read_terminal_command logger

    | ["info" |"i"; "tokens"      | "t"] | ["it"] -> Info Tokens
    | ["info" |"i"; "breakpoints" | "b"] | ["ib"] -> Info Breakpoints
    | ["info" |"i"; "alarms"      | "a"] | ["ia"] -> Info Alarms
    | ["info" |"i"; "checks"      | "c"] | ["ic"] -> Info Checks
    | ["info" |"i"; "variables"   | "vars" | "var"  | "v"] | ["iv"] -> Info Variables
    | ["info" |"i"; "context"     | "ctx"] | ["ictx"] -> Info Context

    | ["enable" | "en"; "hook" | "h"; h] | ["eh"; h] -> Enable (Hook h)
    | ["disable"|"d";   "hook" | "h"; h] | ["dh"; h] -> Disable (Hook h)

    | ["set"  |"s";  "debug"| "d"; d] | ["sd"; d] -> Set (Debug, d)
    | ["unset"|"u";  "debug"| "d"] | ["ud"] -> Unset Debug

    | ["set"  |"s";  "script"| "s"; d] | ["sc"; d] -> Set (Script, d)
    | ["unset"|"u";  "script"| "s"] | ["uc"] -> Unset Script

    | ["load"; "script"; s] | ["ls"; s] -> LoadScript s

    | ["save"; file] -> Save file

    | print::vars when Str.string_match (Str.regexp {|\(p\|print\)@\(.+\):\([0-9]+\)|}) print 0 ->
      let file = Str.matched_group 2 print in
      let line = Str.matched_group 3 print |> int_of_string in
      let vars =
        List.fold_left
          (fun acc s ->
             let parts = String.split_on_char ',' s |>
                         List.filter (function "" -> false | _ -> true)
             in
             SetExt.StringSet.union acc (SetExt.StringSet.of_list parts)
          ) SetExt.StringSet.empty vars
      in
      Print(SetExt.StringSet.elements vars, Some(file, line))

    | _ ->
      printf "Unknown command %s@." s;
      print_usage ();
      read_terminal_command logger


  (** {2 Pretty printers} *)
  (** ******************* *)

  (* Get the number of digits of an integer *)
  let nb_digits n =
    int_of_float (log10 (float_of_int n)) + 1

  (* Right align an integer *)
  let pp_right_align_int width fmt i =
    let digits = nb_digits i in
    fprintf fmt "%s%d"
      (String.init (width - digits) (fun _ -> ' '))
      i

  let pp_right_align width pp fmt x =
    let s = asprintf "%a" pp x in
    let len = String.length s in
    fprintf fmt "%s%s"
      (String.init (width - len) (fun _ -> ' '))
      s

  (* Format has issues when identing in presence of unicode characters. So we
       do it manually. *)
  let fix_string_indentation indent s =
    let lines = String.split_on_char '\n' s in
    match lines with
    | [] -> ""
    | [_] -> s
    | hd::tl ->
      let lines' = hd :: List.map (fun l -> (String.make indent ' ') ^ "    " ^ l) tl in
      String.concat "\n" lines'

  let truncate_string s =
    let lines = String.split_on_char '\n' s in
    match lines with
    | [] | [_] -> s
    | hd::tl -> hd ^ " ..."

  let pp_exec ~truncate ~indent fmt stmt =
    let s = asprintf "@[<v>%a@]" pp_stmt stmt in
    fprintf fmt "%a %a %s %a"
      Debug.(color 45 pp_print_string) "𝕊"
      Debug.(color 45 pp_print_string) "⟦"
      (if truncate then truncate_string s else fix_string_indentation indent s)
      Debug.(color 45 pp_print_string) "⟧"

  let pp_eval ~truncate ~indent fmt exp =
    let s = asprintf "@[<v>%a@]" pp_expr exp in
    fprintf fmt "%a %a %s : %a %a"
      Debug.(color 209 pp_print_string) "𝔼"
      Debug.(color 209 pp_print_string) "⟦"
      (if truncate then truncate_string s else fix_string_indentation indent s)
      pp_typ exp.etyp
      Debug.(color 209 pp_print_string) "⟧"

  let pp_action ?(truncate=false) ?(indent=0) fmt action =
    match action with
    | Exec(stmt,_) -> pp_exec ~truncate ~indent fmt stmt
    | Eval(exp,_,_) -> pp_eval ~truncate ~indent fmt exp

  let pp_trace fmt trace =
    let remove_new_lines s =
      Bytes.of_string s |>
      Bytes.map (function '\n' -> ' ' | x -> x) |>
      Bytes.to_string
    in
    let trace',n = List.fold_left (fun (acc,i) a -> (i,a)::acc,(i+1)) ([],0) trace in
    let max_digits = nb_digits n in
    fprintf fmt "@[<v>%a@]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,")
         (fun fmt (i,a) ->
            let s = asprintf "@[<h>%a%a  @[%a@] at %a@]"
                (fun fmt () -> if i = 0 then () else fprintf fmt "@,") ()
                (pp_right_align (max_digits+1) pp_print_string) ("#" ^ (string_of_int (n-i-1)))
                (pp_action ~truncate:true ~indent:0) a
                pp_relative_range (action_range a) in
            let s' = remove_new_lines s in
            pp_print_string fmt s'
         )
      ) trace'

  (** Print source code of an action *)
  let pp_action_source_code fmt action =
    (* Entry point *)
    let rec doit () =
      let range = action_range action in
      if not (is_orig_range (untag_range range)) then () else
        let start = get_range_start range in
        let file = start.pos_file in
        let line = start.pos_line in
        if not (Sys.file_exists file) then ()
        else
          let ch = open_in file in
          let before,at,after = read_lines_around ch line in
          let max_line = line + List.length after in
          let max_digits = nb_digits max_line in
          List.iter (pp_surrounding_line max_digits std_formatter) before;
          pp_target_line max_digits std_formatter at;
          List.iter (pp_surrounding_line max_digits std_formatter) after;
          close_in ch
    (* Read lines before and after a target line *)
    and read_lines_around ch line =
      let rec iter before at after i =
        try
          let l = input_line ch in
          if i < line - 5 then iter before at after (i+1) else
          if i > line + 5 then (before,at,after)
          else
          if i < line then iter ((i,l)::before) at after (i+1) else
          if i = line then iter before (i,l) after (i+1)
          else iter before at ((i,l)::after) (i+1)
        with End_of_file -> (before,at,after)
      in
      let before,at,after = iter [] (0,"") [] 1 in
      List.rev before, at, List.rev after
    (* Print a surrounding line *)
    and pp_surrounding_line max_line fmt (i,l) =
      fprintf fmt "   %a  %s@."
        (pp_right_align_int max_line) i
        l
    (* Print the target line *)
    and pp_target_line max_line fmt (i,l) =
      fprintf fmt " %a %a  %a@."
        Debug.(color 118 pp_print_string) "►"
        Debug.(color 118 (pp_right_align_int max_line)) i
        Debug.(color 118 pp_print_string) l
    in
    doit ()

  module Addr =
  struct
    type t = addr
    let compare = compare_addr
    let print = unformat pp_addr
    let from_expr e =
      match ekind e with
      | E_addr (addr, _) -> addr
      | _ -> assert false
  end

  module AddrSet =
  struct
    include SetExt.Make(Addr)
    let print printer s =
      pp_list Addr.print printer (elements s) ~lopen:"{" ~lsep:"," ~lclose:"}"
  end


  let dummy_range = mk_fresh_range ()

  (** Print value of variables *)
  let pp_vars action names man flow =
    if man.lattice.is_bottom (Flow.get T_cur man.lattice flow) then
      printf "⊥@."
    else
      let names =
        match names with
        | [] ->
          List.map (fun v -> asprintf "%a" pp_var v) (action_line_vars action)
        | _ -> names in
      let vars = man.ask Q_defined_variables flow in
      let addrs = man.ask Q_allocated_addresses flow in
      let vmap =
        List.fold_left
          (fun acc v ->
             let vname = asprintf "%a" pp_var v in
             let old = OptionExt.default VarSet.empty
                 (MapExt.StringMap.find_opt vname acc) in
             MapExt.StringMap.add vname (VarSet.add v old) acc)
          MapExt.StringMap.empty vars
      in
      let amap =
        List.fold_left
          (fun acc a ->
             let aname = asprintf "%a" pp_addr a in
             let old = OptionExt.default AddrSet.empty
                 (MapExt.StringMap.find_opt aname acc) in
             MapExt.StringMap.add aname (AddrSet.add a old) acc)
          MapExt.StringMap.empty addrs
      in
      let vfound, afound, not_found = List.fold_left
          (fun (vfound,afound,not_found) name ->
             match MapExt.StringMap.find_opt name vmap with
             | Some vars -> (VarSet.elements vars)@vfound,afound,not_found
             | None     ->
               match MapExt.StringMap.find_opt name amap with
               | Some addrs -> vfound,(AddrSet.elements addrs)@afound,not_found
               | None      -> vfound,afound,name::not_found
          ) ([],[],[]) names in
      let protect_print print_thunk =
        let oldv = !Core.Ast.Var.print_uniq_with_uid in
        Core.Ast.Var.print_uniq_with_uid := true;
        print_thunk ();
        Core.Ast.Var.print_uniq_with_uid := oldv;
      in
      let not_found' =
        let printer = empty_printer () in
        let found,not_found =
          List.fold_left
            (fun (found,not_found) v ->
               try
                 let () = 
                   if VarSet.cardinal @@ OptionExt.default VarSet.empty (MapExt.StringMap.find_opt (asprintf "%a" pp_var v) vmap) > 1 then
                     protect_print (fun () -> man.print_expr flow printer (mk_var v dummy_range))
                   else 
                     man.print_expr flow printer (mk_var v dummy_range) in
                 true,not_found
               with Not_found ->
                 let vname = asprintf "%a" pp_var v in
                 found,vname::not_found
            ) (false,[]) vfound
        in
        let found,not_found =
          List.fold_left
            (fun (found,not_found) a ->
               try
                 let () = man.print_expr flow printer (mk_addr a dummy_range) in
                 true,not_found
               with Not_found ->
                 let aname = asprintf "%a" pp_addr a in
                 found,aname::not_found
            ) (found,not_found) afound
        in
        if found then printf "%a@." pflush printer;
        not_found
      in
      ( match not_found@not_found' with
        | [] -> ()
        | l  ->
          printf "Variable%a %a not found@."
            Debug.plurial_list not_found
            (pp_print_list
               ~pp_sep:(fun fmt () -> pp_print_string fmt ", ")
               (fun fmt vname -> fprintf fmt "'%s'" vname)
            ) l
      )

  let init () =
    printf "@.%a@.Type '%a' to get the list of commands.@.@."
      (Debug.bold pp_print_string) ("Welcome to Mopsa " ^ Version.version ^ "!")
      (Debug.bold pp_print_string) "help"

  let reach action man flow =
    let range = action_range action in
    if is_orig_range range then (
      (* Print the range of the next action *)
      printf "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      (* Print location in the source code *)
      pp_action_source_code std_formatter action;
      (* Print interpreter action *)
      printf "%a@." (pp_action ~truncate:true ~indent:0) action
    )


  let logger cmd_str =
    if List.mem cmd_str ["us"; "unset script"; "unset s"] then
      ()
    else
      (* todo: currently logs unset script, whoops *)
      match !script with
      | None -> ()
      | Some ch ->
        let file_fmt = formatter_of_out_channel ch in
        Format.fprintf file_fmt "%s@." cmd_str
  (* todo: remove last @. ... *)

  let rec read_command action envdb man flow =
    let cmd = try read_terminal_command logger
      with Exit -> exit 0
    in
    match cmd with
    | Break loc ->
      breakpoints := BreakpointSet.add (parse_breakpoint loc) !breakpoints;
      read_command action envdb man flow

    | Continue ->
      Interface.Continue

    | Next ->
      Next

    | NextI ->
      NextI

    | Step ->
      Step

    | StepI ->
      StepI

    | Finish ->
      Finish

    | BackTrace ->
      let cs = Flow.get_callstack flow in
      printf "%a@." pp_callstack cs;
      read_command action envdb man flow

    | Print(names, loc) ->
      let ctx = Flow.get_ctx flow in
      let env =
        match loc with
        | None -> Some(action, flow)
        | Some(file, line) ->
          match find_envdb_opt file line envdb with
          | None -> None
          | Some(action', envs) ->
            printf "%a@." (pp_action ~truncate:false ~indent:0) action';
            let env =
              CallstackMap.fold
                (fun _ -> man.lattice.join ctx)
                envs man.lattice.bottom
            in
            Some(action', Flow.singleton ctx T_cur env)
      in
      ( match env with
        | None -> ()
        | Some(action', flow') -> pp_vars action' names man flow'
      );
      read_command action envdb man flow

    | Env [] ->
      let env = Flow.get T_cur man.lattice flow in
      printf "%a@." (Print.format man.lattice.print) env;
      read_command action envdb man flow

    | Env domains ->
      let env = Flow.get T_cur man.lattice flow in
      let re =
        List.map
          (fun d ->
             (* Replace wildcard shortcut '_' *)
             let d' = Str.global_replace (Str.regexp_string "_") ".*" d in
             (* Accept any domain name containing the given string *)
             ".*" ^ d' ^ ".*"
          ) domains |>
        (* Add alternative operator between domains names *)
        String.concat "\\|" |>
        Str.regexp
      in
      let pobj = pbox man.lattice.print env in
      let pobj' = match_print_object_keys re pobj in
      printf "%a@." pp_print_object pobj';
      read_command action envdb man flow

    | Where ->
      pp_action_source_code std_formatter action;
      printf "%a@." Debug.(color fushia pp_relative_range) (action_range action);
      printf "%a@." (pp_action ~truncate:false ~indent:0) action;
      read_command action envdb man flow

    | Enable (Hook hook) ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook;
        read_command action envdb man flow
      ) else (
        Hook.activate_hook hook;
        let ctx = Hook.init_hook hook (Flow.get_ctx flow) in
        let flow = Flow.set_ctx ctx flow in
        read_command action envdb man flow
      )

    | Disable (Hook hook) ->
      if not (Hook.mem_hook hook) then (
        printf "Hook '%s' not found@." hook
      ) else (
        Hook.deactivate_hook hook man flow
      );
      read_command action envdb man flow

    | Info Tokens ->
      let tokens = Flow.fold (fun acc tk _ -> tk::acc) [] flow in
      printf "%a@." (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_token) tokens;
      read_command action envdb man flow

    | Info Alarms ->
      let report = Flow.get_report flow in
      ( if is_safe_report report
        then printf "%a No alarm@." Debug.(color_str green) "✔";
        let _ = Output.Text.construct_checks_summary ~print:true report None in
        ()
      );
      read_command action envdb man flow

    | Info Checks ->
      let report = Flow.get_report flow in
      let total, safe, error, warning, checks_map = Output.Text.construct_checks_summary report None in
      Output.Text.print_checks_summary checks_map total safe error warning None;
      read_command action envdb man flow

    | Info Breakpoints ->
      printf "%a@." Breakpoint.pp_breakpoint_set !breakpoints;
      read_command action envdb man flow

    | Info Variables ->
      let vars = man.ask Q_defined_variables flow in
      printf "@[<v>%a@]@."
        (Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt "@,")
           (fun fmt v -> Query.pp_var_with_type fmt (v,v.vtyp))
        ) vars
      ;
      read_command action envdb man flow

    | Info Context ->
      let ctx = Flow.get_ctx flow in
      printf "%a@." (pp_ctx man.lattice.print) ctx;
      read_command action envdb man flow

    | Set (Debug, channel) ->
      Debug.set_channels channel;
      read_command action envdb man flow

    | Unset Debug ->
      Debug.set_channels "";
      read_command action envdb man flow

    | Set (Script, filename) ->
      let ch = open_out filename in 
      script := Some ch;
      read_command action envdb man flow

    | Unset Script ->
      let () = match !script with
        | None -> ()
        | Some ch ->
          close_out ch;
          script := None in
      read_command action envdb man flow

    | LoadScript s ->
      let ch = open_in s in
      let lines =
        let rec process res =
          try
            process ((input_line ch) :: res)
          with End_of_file ->
            List.rev res
        in process []
      in
      List.iter (fun l -> Queue.add l commands_buffer) lines;
      close_in ch;
      read_command action envdb man flow

    | Save file ->
      let ch = open_out file in
      let file_fmt = formatter_of_out_channel ch in
      Format.kasprintf (fun str ->
          Format.fprintf file_fmt "%s%!" str
        )  "%a" (format (Flow.print man.lattice.print)) flow;
      close_out ch;
      read_command action envdb man flow

  let finish man flow =
    ()

  let error ex =
    ()
end
