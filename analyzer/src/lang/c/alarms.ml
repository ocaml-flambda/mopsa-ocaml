open Framework.Essentials

type alarm_kind +=
  | AOutOfBound
  | ANullDeref
  | AInvalidDeref
  | ADivideByZero
  | AIntegerOverflow

let () =
  register_alarm
    {
      compare = (fun default a b -> default a b);
      pp_token = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "out-bound"
          | ANullDeref -> Format.fprintf fmt "null-deref"
          | AInvalidDeref -> Format.fprintf fmt "invalid-deref"
          | ADivideByZero -> Format.fprintf fmt "div-zero"
          | AIntegerOverflow -> Format.fprintf fmt "int-overflow"
          | _ -> default fmt a
        );
      pp_title = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "Out of bound access"
          | ANullDeref -> Format.fprintf fmt "Null pointer dereference"
          | AInvalidDeref -> Format.fprintf fmt "Invalid pointer dereference"
          | ADivideByZero -> Format.fprintf fmt "Division by zero"
          | AIntegerOverflow -> Format.fprintf fmt "Integer overflow"
          | _ -> default fmt a
        );
      pp_report = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "TODO"
          | ANullDeref -> Format.fprintf fmt "TODO"
          | AInvalidDeref -> Format.fprintf fmt "TODO"
          | ADivideByZero -> Format.fprintf fmt "TODO"
          | AIntegerOverflow -> Format.fprintf fmt "TODO"
          | _ -> default fmt a
        )
    };
