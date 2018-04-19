open Framework.Ast
open Framework.Domains.Reduction.Domain

type channel +=
  | CIntInterval of var * Integers.Value.v
  | CIntCongruence of var * Congruence.Value.v
