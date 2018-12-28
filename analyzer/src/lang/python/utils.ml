open Framework.Ast
open Ast

let debug fmt = Debug.debug ~channel:"python.utils" fmt

let rec partition_list_by_length n l =
  if n = 0 then [], l
  else
    match l with
    | hd :: tl ->
      let lhd, ltl = partition_list_by_length (n-1) tl in
      hd :: lhd, ltl
    | _ -> assert false

let mk_builtin_raise exn range =
  mk_stmt (S_py_raise (Some (mk_py_object (Addr.find_builtin exn) range))) range

let mk_builtin_call f params range =
  mk_py_call (mk_py_object (Addr.find_builtin f) range) params range

let mk_hasattr obj attr range =
  mk_builtin_call "hasattr" [obj; Universal.Ast.mk_string attr range] range

let mk_object_hasattr obj attr range =
  mk_hasattr (mk_py_object obj range) attr range

let mk_addr_hasattr obj attr range =
  mk_hasattr (Universal.Ast.mk_addr obj range) attr range


let mk_try_stopiteration body except range =
  mk_try
    body
    [mk_except
       (Some (mk_py_object (Addr.find_builtin "StopIteration") range)) (* (mk_addr (fst @@ Addr.find_builtin "StopIteration") range)) *)
       None
       except
    ]
    (Universal.Ast.mk_block [] range)
    (Universal.Ast.mk_block [] range)
    range
