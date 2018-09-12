open Framework.Essentials
open Ast

let rec expr_to_z (e: expr) : Z.t option =
  match ekind e with
  | E_constant (C_int n) -> Some n
  | E_unop (O_minus, e') ->
    begin
      match expr_to_z e' with
      | None -> None
      | Some n -> Some (Z.neg n)
    end
  | E_binop(op, e1, e2) ->
    begin
      match expr_to_z e1, expr_to_z e2 with
      | Some n1, Some n2 ->
        begin
          match op with
          | O_plus -> Some (Z.add n1 n2)
          | O_minus -> Some (Z.sub n1 n2)
          | O_mult -> Some (Z.mul n1 n2)
          | O_div -> if Z.equal n2 Z.zero then None else Some (Z.div n1 n2)
          | _ -> None
        end
      | _ -> None
    end
  | _ -> None
