open Ast
open Pp
open Flow


(*==========================================================================*)
(**                            {2 Evaluations}                              *)
(*==========================================================================*)


type ('a, 'b) eval_case = 'a option * 'b flow * Ast.stmt list
type ('a, 'b) evals = ('a, 'b) eval_case Dnf.t

let eval_singleton (case: ('a, 'b) eval_case) : ('a, 'b) evals =
  Dnf.singleton case

let eval_map
    (f: ('a, 'b) eval_case -> ('c, 'd) eval_case)
    (evl: ('a, 'b) evals) : ('c, 'd) evals
  =
  Dnf.map f evl

let eval_join (e1: ('a, 'b) evals) (e2: ('a, 'b) evals) : ('a, 'b) evals =
  Dnf.mk_or e1 e2

let eval_meet ?(fand=(@)) (e1: ('a, 'b) evals) (e2: ('a, 'b) evals) : ('a, 'b) evals =
  Dnf.mk_and ~fand e1 e2

let eval_substitute
    (f: ('a, 'b) eval_case -> 'c)
    (join: 'c -> 'c -> 'c)
    (meet: 'c -> 'c -> 'c)
    (evl: ('a, 'b) evals) : 'c
  =
  Dnf.substitute f join meet evl


let eval_substitute2
    (f: ('a, 'b) eval_case -> ('c, 'd) evals)
    (evl: ('a, 'b) evals) : ('c, 'd) evals
  =
  Dnf.substitute2 f evl

let eval_iter (f: ('a, 'b) eval_case -> unit) (evals: ('a, 'b) evals) : unit =
  Dnf.to_list evals |>
  List.flatten |>
  List.iter f

let eval_fold (f: 'c -> ('a, 'b) eval_case -> unit) (x: 'c) (evals: ('a, 'b) evals) : 'c =
  Dnf.to_list evals |>
  List.flatten |>
  List.fold_left f x

let pp_evals print fmt (evals: ('a, 'b) evals) =
  let l = Dnf.to_list evals in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋁@;")
    (fun fmt conj ->
       Format.fprintf fmt "(%a)"
         (Format.pp_print_list
            ~pp_sep:(fun fmt () -> Format.fprintf fmt "@;⋀@;")
            (fun fmt (x, _, _) ->
               match x with
               | None -> Format.pp_print_string fmt "none"
               | Some x -> print fmt x
            )
         ) conj
    )
    fmt l

let pp_oevals print fmt (evals: ('a, 'b) evals option) =
  match evals with
  | None -> Format.fprintf fmt "None"
  | Some evals -> pp_evals print fmt evals

(*==========================================================================*)
(**                       {2 Optional evaluations}                          *)
(*==========================================================================*)

let oeval_singleton (ev: ('a, 'b) eval_case) : ('a, 'b) evals option =
  Some (eval_singleton ev)

let oeval_map
    (f: ('a, 'b) eval_case -> ('c, 'd) eval_case)
    (oevl: ('a, 'b) evals option) : ('c, 'd) evals option
  =
  Option.option_lift1 (eval_map f) oevl

let oeval_join
    (oevl1: ('a, 'b) evals option)
    (oevl2: ('a, 'b) evals option) : ('a, 'b) evals option
  =
  Option.option_neutral2 eval_join oevl1 oevl2

let oeval_meet
    ?(fand=(@)) (oevl1: ('a, 'b) evals option)
    (oevl2: ('a, 'b) evals option) : ('a, 'b) evals option
  =
  Option.option_neutral2 (eval_meet ~fand) oevl1 oevl2


let oeval_merge
    (f: ('a, 'b) eval_case -> 'c)
    (join: 'c -> 'c -> 'c)
    (meet: 'c -> 'c -> 'c)
    (none: unit -> 'c)
    (oevl: ('a, 'b) evals option) : 'c
  =
  Option.option_dfl1 none (Dnf.substitute f join meet) oevl

let oeval_merge2
    (f1: ('a, 'b) evals -> 'e)
    (f2: ('c, 'd) evals -> 'e)
    (f12: ('a, 'b) evals -> ('c, 'd) evals -> 'e)
    (none: unit -> 'e)
    (oevl1: ('a, 'b) evals option) (oevl2: ('c, 'd) evals option) : 'e =
  Option.option_apply2 f1 f2 f12 none oevl1 oevl2


let oeval_substitute
    (f: ('a, 'b) eval_case -> ('c, 'd) evals option)
    (oevl: ('a, 'b) evals option) : ('c, 'd) evals option =
  oeval_merge f
    (oeval_merge2
       (fun evl1 -> Some evl1)
       (fun evl2 -> Some evl2)
       (fun evl1 evl2 -> Some (eval_join evl1 evl2))
       (fun () -> None)
    )
    (oeval_merge2
       (fun evl1 -> Some evl1)
       (fun evl2 -> Some evl2)
       (fun evl1 evl2 -> Some (eval_meet evl1 evl2))
       (fun () -> None)
    )
    (fun () -> None)
    oevl


let eval_compose
    (eval: 'a -> 'b flow -> ('c, 'd) evals option)
    ?(empty = (fun flow -> oeval_singleton (None, flow, [])))
    (evl: ('a, 'b) evals)
  : ('c, 'd) evals option  =
  Some evl |> oeval_substitute
    (fun (x, flow, clean) ->
       let oevl' =
         match x with
         | Some x -> eval x flow
         | None -> empty flow
       in
       oeval_map
         (fun (x', flow, clean') ->
            (x', flow, clean @ clean')
         ) oevl'
    )

let oeval_compose eval ?(empty = (fun flow -> oeval_singleton (None, flow, []))) evl =
  match evl with
  | None -> None
  | Some evl -> eval_compose eval ~empty evl

let eval_list
    (l: 'a list)
    (eval: 'a -> 'b flow -> ('c, 'b) evals)
    ?(empty = (fun flow -> eval_singleton (None, flow, [])))
    (flow: 'b flow)
  : ('c list, 'b) evals =
  let rec aux expl flow clean = function
    | [] ->
      eval_singleton (Some (List.rev expl), flow, clean)
    | exp :: tl ->
      eval exp flow |>
      eval_substitute2
        (fun (exp', flow, clean') ->
           match exp' with
           | Some exp' -> (aux (exp' :: expl) flow (clean @ clean') tl)
           | None -> empty flow
        )
  in
  aux [] flow [] l

let oeval_list
    (l: 'a list)
    (eval: 'a -> 'b flow -> ('c, 'b) evals option)
    ?(empty = (fun flow -> oeval_singleton (None, flow, [])))
    (flow: 'b flow)
  : ('c list, 'b) evals option =
  let rec aux expl flow clean = function
    | [] ->
      oeval_singleton (Some (List.rev expl), flow, clean)
    | exp :: tl ->
      eval exp flow |>
      oeval_substitute
        (fun (exp', flow, clean') ->
           match exp' with
           | Some exp' -> (aux (exp' :: expl) flow (clean @ clean') tl)
           | None -> empty flow
        )
  in
  aux [] flow [] l

(**
   [re_eval_singleton ev eval] re-evaluates a singleton evaluation case
   [ev], starting from the top-level domain.
   The cleaner statements of the new evaluations are automatically added to
   the result.
*)
let re_eval_singleton eval (exp, flow, clean)  =
  match exp with
  | None -> oeval_singleton (exp, flow, clean)
  | Some exp ->
    let evl =
      eval exp flow |>
      eval_map
        (fun (exp', flow', clean') ->
           (exp', flow', clean @ clean')
        )
    in
    Some evl
