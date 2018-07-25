(****************************************************************************)
(*                   Copyright (C) 2017 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Generic query mechanism for extracting information from abstract domains.

    Defining a new query requires the definition of the type of its reply and a
    manager on this type providing reply merging functions.

    Here is an example. Let us define a new interval query:
    {[type _ query +=
      | QInterval : var -> (int * int) query
      ;;
    ]}

    Next, an interval manager is registered as follows:
    {[register_reply_manager {
        domatch = (let check : type a. a query -> (a, (int * int)) eq option =
                     fun q ->
                       match q with
                       | QInterval _ -> Some Eq
                       | _ -> None
                   in
                   check
                  );
        join = (fun (a1, a2) (b1, b2) ->
            (min a1 b1, max a2 b2)
          );

        meet = (fun (a1, a2) (b1, b2) ->
            (max a1 b1, min a2 b2)
          );
      };;]}

    For instance, the join of two intervals of a query [q] can be obtained simply by:
    {[join q (Some (1, 20)) (Some (-1, 5));;]}
    {v - : (int * int) option = Some (-1, 20) v}

*)

(** {2 Queries} *)

(** Type of a query, defined by domains and annotated with the type of the reply. *)
type _ query = ..

(** Query type equality witness. *)
type (_, _) eq = Eq : ('a, 'a) eq


(** {2 Managers} *)

(** Query manager defines merge operators on replies. *)
type 'r reply_manager = {
  domatch : 'a. 'a query -> ('a, 'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}

type xreply_manager = Manager : 'a reply_manager -> xreply_manager

let reply_managers : xreply_manager list ref = ref []

let register_reply_manager man =
  reply_managers := (Manager man) :: !reply_managers

let is_query_manager: type a b. a query -> b reply_manager -> a reply_manager option =
  fun q man ->
    match man.domatch q with
    | None -> None
    | Some Eq -> Some man


let find_manager query () =
  let rec aux : type a b. a query -> xreply_manager list -> a reply_manager =
    fun query -> function
      | [] -> raise Not_found
      | hd :: tl ->
        let Manager man = hd in
        match  is_query_manager query man with
        | None -> aux query tl
        | Some man -> man
  in
  aux query !reply_managers


(** {2 Operators} *)

let join : type a. a query -> a option -> a option -> a option =
  fun q r1 r2 ->
    match r1, r2 with
    | None, r | r, None -> r
    | Some r1, Some r2 ->
      let man = find_manager q () in
      Some (man.join r1 r2)

let meet : type a. a query -> a option -> a option -> a option =
  fun q r1 r2 ->
    match r1, r2 with
    | None, r | r, None -> r
    | Some r1, Some r2 ->
      let man = find_manager q () in
      Some (man.meet r1 r2)
