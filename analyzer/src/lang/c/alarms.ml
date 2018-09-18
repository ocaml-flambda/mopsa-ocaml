open Framework.Essentials

type alarm_kind +=
  | AOutOfBound
  | ANullDeref
  | AInvalidDeref
  | ADivideByZero
  | ATIntegerOverflow

let () =
  register_alarm
    {
      compare = (fun default a b -> default a b);
      print = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "out-bound"
          | ANullDeref -> Format.fprintf fmt "null-deref"
          | AInvalidDeref -> Format.fprintf fmt "invalid-deref"
          | ADivideByZero -> Format.fprintf fmt "div-zero"
          | ATIntegerOverflow -> Format.fprintf fmt "int-overflow"
          | _ -> default fmt a
        );
      report = (fun default fmt a ->
          match a.alarm_kind with
          | AOutOfBound -> Format.fprintf fmt "Out of bound access"
          | ANullDeref -> Format.fprintf fmt "Null pointer dereference"
          | AInvalidDeref -> Format.fprintf fmt "Invalid pointer dereference"
          | ADivideByZero -> Format.fprintf fmt "Division by zero"
          | ATIntegerOverflow -> Format.fprintf fmt "Integer overflow"
          | _ -> default fmt a
        )
    };
