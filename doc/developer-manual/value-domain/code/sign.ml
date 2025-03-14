open Mopsa
open Sig.Abstraction.Value
open Ast

module Value =
struct

  (* Definition of the abstract elements *)
  type t =
         | T
    | LE | NE | GE
    | LT | EQ | GT
         | B

  let print (fmt: Format.formatter) (x: t): unit =
    Format.pp_print_string fmt
      (match x with
       | T -> "⊤"
       | LE -> "≤0" | NE -> "≠0" | GE -> "≥0"
       | LT -> "<0" | EQ -> "=0" | GT -> ">0"
       | B -> "⊥")

  (* Greatest element *)
  let top: t = T

  (* Smallest element *)
  let bottom: t = B

  (* Test equality with smallest element *)
  let is_bottom (b: t) : bool = match b with
    | B -> true
    | _ -> false

  (* Utility functions *)
  let get_basic_properties (x: t) =
    match x with
    | T  -> (true , true , true )
    | LE -> (true , true , false)
    | LT -> (true , false, false)
    | GE -> (false, true , true )
    | GT -> (false, false, true )
    | EQ -> (false, true , false)
    | NE -> (true , false, true )
    | B  -> (false, false, false)

  let from_basic_properties is_neg is_zero is_pos =
    match is_neg, is_zero, is_pos with
    | (true , true , true ) -> T 
    | (true , true , false) -> LE
    | (true , false, false) -> LT
    | (false, true , true ) -> GE
    | (false, false, true ) -> GT
    | (false, true , false) -> EQ
    | (true , false, true ) -> NE
    | (false, false, false) ->  B

  (* Abstract union *)
  let join (x: t) (y: t): t =
    let (negx, zerox, posx) = get_basic_properties x in
    let (negy, zeroy, posy) = get_basic_properties y in
    from_basic_properties
      (negx || negy)
      (zerox || zeroy)
      (posx || posy)

  (* Abstract intersection *)
  let meet (x: t) (y: t) = 
    let (negx, zerox, posx) = get_basic_properties x in
    let (negy, zeroy, posy) = get_basic_properties y in
    from_basic_properties
      (negx && negy)
      (zerox && zeroy)
      (posx && posy)

  (* Widening operator *)
  let widen = join

  (* Inclusion test *)
  let subset (x: t) (y: t) =
    let imply a b = (not a) || b in
    let (negx, zerox, posx) = get_basic_properties x in
    let (negy, zeroy, posy) = get_basic_properties y in
    imply negx negy && imply zerox zeroy && imply posx posy

  include GenValueId(
    struct
      type nonrec t = t
      let name = "universal.numeric.values.sign"
      let display = "sign"
    end
    )

  let constant (t: typ) (c: constant) =
    match t with
    | T_int | T_bool ->
      begin
        match c with
        | C_bool true -> NE
        | C_bool false -> EQ
        | C_int i when Z.equal i Z.zero -> EQ
        | C_int i when Z.( i > zero ) -> GT
        | C_int i when Z.( i < zero ) -> LT
        | C_int_interval (i1, i2) when Z.equal i1 Z.zero &&
                                       Z.equal i2 Z.zero -> EQ
        | C_int_interval (i1, i2) when Z.gt i1 Z.zero -> GT
        | C_int_interval (i1, i2) when Z.geq i1 Z.zero -> GE
        | C_int_interval (i1, i2) when Z.lt i2 Z.zero -> LT
        | C_int_interval (i1, i2) when Z.leq i2 Z.zero -> LE
        | _ -> T
      end
      |> OptionExt.return
    | _ -> None

  let cast _ (t: typ) _ =
    match t with
    | T_int | T_bool -> Some T
    | _              -> None

  let neg (x: t) =
    let (negx, zerox, posx) = get_basic_properties x in
    from_basic_properties posx zerox negx

  let unop (op: operator) (t: typ) (x: t) =
    match op with
    | O_log_not ->
      begin
        let (negx, zerox, posx) = get_basic_properties x in
        from_basic_properties
          zerox
          (negx || posx)
          zerox
      end
    | O_minus  -> neg x
    | O_plus -> x
    | _ -> T

  let rec binop (op: operator) (t: typ) (x: t) (y: t) =
    if is_bottom x || is_bottom y then bottom
    else 
      match op with
      | O_plus -> 
        let (negx, zerox, posx) = get_basic_properties x in
        let (negy, zeroy, posy) = get_basic_properties y in
        from_basic_properties
          (negx || negy)
          ((negx && posy) || (negy && posx) || (zerox && zeroy))
          (posx || posy)
      | O_minus -> binop O_plus t x (neg y)
      | O_mult  ->
        let (negx, zerox, posx) = get_basic_properties x in
        let (negy, zeroy, posy) = get_basic_properties y in
        from_basic_properties
          ((negx && posy) || (negy && posx))
          (zerox || zeroy)
          ((posx && posy) || (negx && negy))
      | O_div  ->
        begin
          let (negx, zerox, posx) = get_basic_properties x in
          let (negy, zeroy, posy) = get_basic_properties y in
          from_basic_properties
            (((negx && posy) || (negy && posx)))
            ( zerox && (posy || negy))
            ((posx && posy) || (negx && negy)  )
        end
      | _ -> top

  let filter (b: bool) (t: typ) (x: t) =
    let (negx, zerox, posx) = get_basic_properties x in
    if b then
      from_basic_properties negx false posx
    else
      from_basic_properties false zerox false

  let bwd_unop = default_bwd_unop
  let bwd_binop = default_bwd_binop
  let bwd_cast = default_bwd_cast
  let predicate = default_predicate

  let lt_aux (x: t) (y: t): t * t =
    let (negx, zerox, posx) = get_basic_properties x in
    let (negy, zeroy, posy) = get_basic_properties y in
    from_basic_properties
      (negx  && (negy || zeroy || posy))
      (zerox && posy)
      (posx  && posy)
    ,
    from_basic_properties
      (negy  && negx)
      (zeroy && negx)
      (posy  && (negx || zerox || posx))

  let le_aux (x: t) (y: t): t * t =
    let (negx, zerox, posx) = get_basic_properties x in
    let (negy, zeroy, posy) = get_basic_properties y in
    from_basic_properties
      (negx  && (negy || zeroy || posy))
      (zerox && (posy || zeroy))
      (posx  && posy)
    ,
    from_basic_properties
      (negy  && negx)
      (zeroy && (negx || zerox))
      (posy  && (negx || zerox || posx))

  let ne_aux (x: t) (y: t): t * t =
    let (negx, zerox, posx) = get_basic_properties x in
    let (negy, zeroy, posy) = get_basic_properties y in
    from_basic_properties
      (negx  && (negy || zeroy || posy))
      (zerox && (posy || negy))
      (posx  && (negy || zeroy || posy))
    ,
    from_basic_properties
      (negy  && (negx || zerox || posx))
      (zeroy && (posx || negx))
      (posy  && (negx || zerox || posx))

  let compare (op: operator) (b: bool) (t: typ) (x: t) (y: t): t * t=
    let op = if b then op else negate_comparison_op op in
    match op with
    | O_eq ->
      let a = meet x y in
      a, a
    | O_ne -> ne_aux x y
    | O_lt -> lt_aux x y
    | O_le -> le_aux x y 
    | O_gt -> let a, b = lt_aux y x in (b, a)
    | O_ge -> let a, b = le_aux y x in (b, a)
    | _ -> default_compare op b t x y

  let ask _ _ = None
end

let () =
  register_value_abstraction (module Value)
