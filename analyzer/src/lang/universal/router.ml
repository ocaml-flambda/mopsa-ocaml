open Framework.Essentials
open Ast
open Zone

module Domain(* : Framework.Domains.Stateless.S *) =
struct
  let name = "universal.router"
  type _ domain += D_universal_router : unit domain
  let id = D_universal_router

  let identify : type a. a domain -> (unit, a) eq option =
    function
    | D_universal_router -> Some Eq
    | _ -> None

  let debug fmt = Debug.debug ~channel:name fmt

  let exec_interface =
    { export = [Zone.Z_universal] ;
      import = [Zone.Z_universal_num; Zone.Z_universal_tree; Zone.Z_universal_string]
    }
  let eval_interface =
    { export = [Zone.Z_universal, Zone.Z_universal];
      import = [(* Zone.Z_universal, Zone.Z_universal_num;
                 * Zone.Z_universal, Zone.Z_universal_tree;
                 * Zone.Z_universal, Zone.Z_universal_string *)]
    }

  let zone_of_type t =
    match t with
    | T_string -> Some Z_universal_string
    | T_int -> Some Z_universal_num
    | T_tree -> Some Z_universal_tree
    | _ -> None
  let exec (_: Framework.Zone.zone) (stmt: Framework.Ast.stmt) (man: ('a, unit) man) (flow: 'a flow) =
    match skind stmt with
    | S_assign(e, e') ->
      begin
        match e' |> etyp |> zone_of_type with
        | Some z -> man.exec ~zone:z stmt flow |> Post.of_flow |> Option.return
        | None -> None
      end
    | S_assume e ->
      begin
        match e |> etyp |> zone_of_type with
        | Some z -> man.exec ~zone:z stmt flow |> Post.of_flow |> Option.return
        | None -> None
      end
    | S_fold(v, vl) | S_expand(v, vl) ->
      begin
        match v.vtyp  |> zone_of_type with
        | Some z -> man.exec ~zone:z stmt flow |> Post.of_flow |> Option.return
        | None -> None
      end
    | S_forget v ->
      begin
        match v.vtyp  |> zone_of_type with
        | Some z -> man.exec ~zone:z stmt flow |> Post.of_flow |> Option.return
        | None -> None
      end
    | _ -> None

  let eval z expr man flow =
    let () = debug "asked :%a" pp_expr expr in
    None

  let init _ _ _ = None
  let ask _ _ _ = None
end

let () =
    Framework.Domains.Stateless.register_domain (module Domain)
