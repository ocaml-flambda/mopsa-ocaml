open Framework.Ast
open Framework.Domains.Reduction.Domain

type channel +=
  | CIntInterval of var * Values.Int.v
  | CIntCongruence of var * Values.Congruence.v
