open Framework.Ast
open Framework.Domains.Reduce.Domain

type channel +=
  | CIntConstant of var * Z.t
  | CIntCongruence of var * Values.Congruence.v
