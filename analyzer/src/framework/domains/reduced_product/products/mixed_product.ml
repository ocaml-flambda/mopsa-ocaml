  let pool_man (man:('a, D.t) man) : ('a, D.t, unit) pool_man = {
    get_state = (
      let f : type b. b domain -> 'a -> b = fun id env ->
        let rec aux : type b c. b domain -> c pool -> ('a, c) man -> b = fun id pool man ->
          match pool with
          | Nil -> raise Not_found
          | Cons(hd, tl) ->
            let module D = (val hd) in
            match D.identify id with
            | Some Eq ->
              let man' = head_man man in
              man'.get env
            | None -> aux id tl (tail_man man)
        in
        aux id Config.pool man
      in
      f
    );
    set_state = (
      let f : type b. b domain -> b -> 'a -> 'a = fun id a env ->
        let rec aux : type b c. b domain -> b -> c pool -> ('a, c) man -> 'a =
          fun id a pool man ->
            match pool with
            | Nil -> raise Not_found
            | Cons(hd, tl) ->
              let module D = (val hd) in
              match D.identify id with
              | Some Eq ->
                let man' = head_man man in
                man'.set a env
              | None -> aux id a tl (tail_man man)
        in
        aux id a Config.pool man
      in
      f
    );
    get_eval = (let f : type t. t domain -> 'a econj -> (Ast.expr option * 'a flow) option =
                  fun id econj ->
                    let rec aux : type s t. s pool -> t domain -> 'a econj -> (Ast.expr option * 'a flow) option =
                      fun pool id econj ->
                        match pool, econj with
                        | Nil, [] -> raise Not_found
                        | Cons(hd, tl), c :: ctl ->
                          let module D = (val hd) in
                          begin match D.identify id, c with
                            | Some Eq, None -> None
                            | Some Eq, Some {expr; flow} -> Some (expr, flow)
                            | _ -> aux tl id ctl
                          end
                        | _ -> assert false
                    in
                    aux Config.pool id econj
                in
                f);
    set_eval = (let f : type t. t domain -> Ast.expr -> 'a flow -> 'a econj -> 'a econj =
                  fun id exp flow econj ->
                    let rec aux : type s t. s pool -> t domain -> 'a econj -> 'a econj =
                      fun pool id econj ->
                        match pool, econj with
                        | Nil, [] -> []
                        | Cons(hd, tl), c :: ctl ->
                          let module D = (val hd) in
                          begin match D.identify id with
                            | Some Eq -> Some {expr = Some exp; flow; cleaners = []} :: ctl
                            | None -> c :: (aux tl id ctl)
                          end
                        | _ -> assert false
                    in
                    aux Config.pool id econj
                in
                f);
    remove_eval = (let f : type t. t domain -> 'a econj -> 'a econj =
                     fun id econj ->
                       let rec aux : type s t. s pool -> t domain -> 'a econj -> 'a econj =
                         fun pool id econj ->
                           match pool, econj with
                           | Nil, [] -> []
                           | Cons(hd, tl), c :: ctl ->
                             let module D = (val hd) in
                             begin match D.identify id with
                               | Some Eq -> None :: ctl
                               | None -> c :: (aux tl id ctl)
                             end
                           | _ -> assert false
                       in
                       aux Config.pool id econj
                   in
                   f
                  );
  }
