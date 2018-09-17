val tmp_name : string
type context = { mutable uid : int; }
val create_context : unit -> context
val new_uid : context -> int
val make_temp :
  context -> C_AST.range -> ?com:C_AST.comment list -> C_AST.func -> C_AST.type_qual -> C_AST.variable
val simplify_func : context -> C_AST.func -> unit
