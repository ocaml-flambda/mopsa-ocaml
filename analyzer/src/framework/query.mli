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
    {[register_query {
        eq = (let check : type a. a query -> (a, (int * int)) eq option =
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


type 'r info = {
  eq : 'a. 'a query -> ('a, 'r) eq option;
  join: 'r -> 'r -> 'r;
  meet: 'r -> 'r -> 'r;
}

val eq : 'a query -> 'b query -> ('a, 'b) eq option

val register_query : 'a info -> unit

val join : 'a query -> 'a -> 'a -> 'a

val meet : 'a query -> 'a -> 'a -> 'a
