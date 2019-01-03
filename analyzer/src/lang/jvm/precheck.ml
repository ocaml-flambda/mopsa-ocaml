(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Simple intraprocedural method analysis to type the stack and register,
    and infer the targets of returns from subroutines (ret).
    This analysis is similar in aim to the bytecode verification performed
    by the JVM.    
    The analysis is sensitive to the interprocedural call stack of 
    subroutines (jsr / ret), and assumes (and checks) no recursive 
    subroutines.
 *)

open Javalib_pack
open Javalib
open JBasics
open JCode
open Mopsa
open Cfg.Ast
open Ast
open Bot


let name = "jvm.precheck"
let debug fmt = Debug.debug ~channel:name fmt


let dump_errors = true (** show precheck errors *)
let dump_result = false (** show the analysis result *)
let trace = false (** trace CFG iteration *)
          

(*==========================================================================*)
                       (** {2 Abstract values} *)
(*==========================================================================*)


(** For double-word data: whether this is the low or hi word *)
type word = HI | LO

               
(** Abstract values: type & return address. 
    We do not distinguish between reference types.
 *)
type aval =
  | A_int
  | A_float
  | A_long of word
  | A_double of word
  | A_ref (** reference to an object or array *)
  | A_pc of node_id (** return address from subroutine *)
  | A_top


(** Maps register number to abstract value. *)
module IntMap = MapExt.IntMap

              
(** Abstract state. *)
type astate = {
    a_stack: aval list; (** topmost element first. *)
    a_reg: aval IntMap.t;
  }

(** Subroutine call stack, topmost element first. *)
type substack = node_id list

(** Order on subroutine call stacks, to build maps. *)
module SubStack =
struct
  type t = substack
  let compare = ListExt.compare compare_node_id
end

(** Maps indexed with subroutinr call stacks. *)
module SubMap = MapExt.Make(SubStack)
                
(** Abstract state partitionned by subroutine call stack. *)
type pastate = astate SubMap.t

             
             
(*==========================================================================*)
                    (** {2 Basic abstract operators} *)
(*==========================================================================*)


(** Utilities. *)

let astate_empty : astate =
  { a_stack = []; a_reg = IntMap.empty; }
            
let rec apply_n i (f:'a -> 'a) (x:'a) : 'a =
  if i <= 0 then x
  else x |> f |> apply_n (i-1) f


(** Type handling *)
  
let type_compatible v v' =
  match v,v' with
  | (A_int | A_float | A_long _ | A_double _ | A_ref), _ -> v = v'
  | A_pc _, A_pc _ -> true
  | _ -> false

(** How many (stack or register) slots are taken by a value of each type *)
let size_basic = function
  | `Float -> 1
  | `Double -> 2
  | `Long -> 2
  | `Object -> 1
  | `Void -> 0
  | `Bool | `Byte | `Char | `Short | `Int | `Int2Bool | `ByteBool -> 1

let size_value = function
  | TBasic b -> size_basic b
  | TObject _ -> 1

let string_of_aval = function
  | A_int -> "int"
  | A_float -> "float"
  | A_long LO -> "long_lo"
  | A_long HI -> "long_hi"
  | A_double LO -> "double_lo"
  | A_double HI -> "double_hi"
  | A_ref -> "ref"
  | A_top -> "top"          
  | A_pc _ -> "pc"


(** Printing *)

let pp_aval fmt  = function
  | A_int -> Format.pp_print_string fmt "int"
  | A_float -> Format.pp_print_string fmt "float"
  | A_long LO -> Format.pp_print_string fmt "long_lo"
  | A_long HI -> Format.pp_print_string fmt "long_hi"
  | A_double LO -> Format.pp_print_string fmt "double_lo"
  | A_double HI -> Format.pp_print_string fmt "double_hi"
  | A_ref -> Format.pp_print_string fmt "ref"
  | A_top -> Format.pp_print_string fmt "top"          
  | A_pc x -> Format.fprintf fmt "pc(%a)" pp_node_id x
    
let pp_astate fmt (a:astate) : unit =
  Format.fprintf
    fmt "@[stack=@[%a@];@ regs=@[%a@]@]"
    (ListExt.fprint ListExt.printer_list pp_aval) a.a_stack
    (IntMap.fprint MapExt.printer_default Format.pp_print_int pp_aval) a.a_reg

let pp_substack fmt (a:substack) : unit =
  ListExt.fprint ListExt.printer_list pp_node_id fmt a
  
let pp_pastate fmt (a:pastate) : unit =
  SubMap.fprint MapExt.printer_default pp_substack pp_astate fmt a
  

(** Join *)

let join_aval (v1:aval) (v2:aval) : aval = match v1, v2 with
  | A_pc s1, A_pc s2 ->
     if compare_node_id s1 s2 = 0 then v1 else A_top
  | x, y ->
     if x = y then x else A_top
                         
let join_astate (a1:astate) (a2:astate) : astate * string list =
  (* join registers *)
  let r =
    IntMap.map2zo
      (fun _ _ -> A_top) (fun _ _ -> A_top) (fun _ -> join_aval)
      a1.a_reg a2.a_reg
  in
  (* fill-in stacks up to the same size *)
  let s1, s2 = a1.a_stack, a2.a_stack in
  let s = List.length s1 - List.length s2 in
  let s1, s2 =
    if s > 0 then s1, (List.init s (fun _ -> A_top))@s2
    else if s < 0 then (List.init (-s) (fun _ -> A_top))@s1, s2
    else s1, s2
  in
  let err = if s = 0 then [] else ["stacks have different depth"] in
  (* join stack *)
  let rec j s1 s2 =
    if s1 == s2 then s1 (* optimize for common case: identical stack tails *)
    else
      match s1, s2 with
      | [], [] -> []
      | x1::s1, x2::s2 -> (join_aval x1 x2)::(j s1 s2)
      | _ -> failwith "impossible case"
  in
  { a_reg = r;
    a_stack = j s1 s2;
  },
  err

let join_pastate (a1:pastate) (a2:pastate) : pastate * string list =
  let err = ref [] in
  let r =
    SubMap.map2zo
      (fun _ x -> x) (fun _ x -> x)
      (fun _ x y -> let z,er = join_astate x y in err := er@(!err); z)
      a1 a2
  in
  r, !err


(** Ordering *)

let subset_val (v1:aval) (v2:aval) : bool = match v1, v2 with
  | A_pc s1, A_pc s2 -> compare_node_id s1 s2 = 0
  | _, A_top -> true
  | _ -> v1 = v2

let subset_astate (a1:astate) (a2:astate) : bool =
  (* a1 <= a2 requires that both states have the same set of
     registers and the same stack depth;
     if not, applying the join will ensure it
   *)
  IntMap.for_all2zo
    (fun _ _ -> false) (fun _ _ -> false) (fun _ -> subset_val)
    a1.a_reg a2.a_reg
  &&
    List.length a1.a_stack = List.length a2.a_stack
  &&
    List.for_all2 subset_val a1.a_stack a2.a_stack

let subset_pastate : pastate -> pastate -> bool =
  SubMap.for_all2zo
    (fun _ _ -> false) (fun _ _ -> true) (fun _ -> subset_astate)
       

(** Stack operations *)

let push v (a,err) =
  { a with a_stack = v::a.a_stack; }, err

let push_double x =
  x |> push (A_double LO) |> push (A_double HI)

let push_long x =
  x |> push (A_long LO) |> push (A_long HI)

let push_basic v a = match v with
  | `Float -> push A_float a
  | `Double -> push_double  a
  | `Long -> push_long a
  | `Object -> push A_ref a
  | `Void -> a
  | `Bool | `Byte | `Char | `Short | `Int | `Int2Bool | `ByteBool ->
     push A_int a

let push_value v a = match v with
  | TBasic b -> push_basic b a
  | TObject _ -> push A_ref a
  

let pop (a,err) =
  match a.a_stack with
  | v::bot -> v, ({ a with a_stack = bot; }, err)
  | [] -> A_top, (a, "empty stack on pop"::err)

let peek (a,err) =
  match a.a_stack with
  | v::_ -> v, (a, err)
  | [] -> A_top, (a, "empty stack on peek"::err)

        
let pop_check v (a,err) =
    match a.a_stack with
    | v'::bot ->
       if type_compatible v v' then { a with a_stack = bot; }, err
       else
         { a with a_stack = bot; },
         (Printf.sprintf
            "wrong type on pop: expected %s, got %s"
            (string_of_aval v) (string_of_aval v')
         )::err
  | [] -> a, "empty stack on pop"::err

let pop_check_double x =
  x|> pop_check (A_double HI) |> pop_check (A_double LO)

let pop_check_long x =
  x|> pop_check (A_long HI) |> pop_check (A_long LO)

let pop_check_basic v a = match v with
  | `Float -> pop_check A_float a
  | `Double -> pop_check_double  a
  | `Long -> pop_check_long a
  | `Object -> pop_check A_ref a
  | `Void -> a
  | `Bool | `Byte | `Char | `Short | `Int | `Int2Bool | `ByteBool ->
     pop_check A_int a

let pop_check_value v a = match v with
  | TBasic b -> pop_check_basic b a
  | TObject _ -> pop_check A_ref a
  

               
(** Register operations *)

let load index (a,err) =
  try IntMap.find index a.a_reg with Not_found -> A_top

let load_check index v (a,err) =
  let v' = load index (a,err) in
  a,
  (if type_compatible v v' then err
   else
     (Printf.sprintf
        "wrong type on load: expected %s, got %s"
        (string_of_aval v) (string_of_aval v')
     )::err)

let load_check_long index a =
  a |> load_check index (A_long LO) |> load_check (index+1) (A_long HI)
  
let load_check_double index a =
  a |> load_check index (A_double LO) |> load_check (index+1) (A_double HI)

let load_check_basic index v a = match v with
  | `Float -> load_check index A_float a
  | `Object -> load_check index A_ref a
  | `Long -> load_check_long index a
  | `Double -> load_check_double index a
  | `Int2Bool | `Int | `Bool | `Short | `Byte | `Char ->
     load_check index A_int a

let load_check_value index v a = match v with
  | TBasic b -> load_check_basic index b a
  | TObject _ -> load_check index A_ref a
  
             
let store index v (a,err) =
  { a with a_reg = IntMap.add index v a.a_reg }, err
  
let store_long index a =
  a |> store index (A_long LO) |> store (index+1) (A_long HI)
  
let store_double index a =
  a |> store index (A_double LO) |> store (index+1) (A_double HI)

let store_basic index v a = match v with
  | `Float -> store index A_float a
  | `Object -> store index A_ref a
  | `Long -> store_long index a
  | `Double -> store_double index a
  | `Int2Bool | `Int | `Bool | `Short | `Byte | `Char ->
     store index A_int a

let store_value index v a = match v with
  | TBasic b -> store_basic index b a
  | TObject _ -> store index A_ref a



(*==========================================================================*)
                      (** {2 Abstract opcode execution} *)
(*==========================================================================*)


(** An error, with location information. *)
type err =
  node_id  (** location *)
  * string (** operation *)
  * string (** error message *)


(** Executes an opcode on a single subroutine call stack partition.
    Only for opcodes that do not change the partition 
    (i.e., all except jsr and ret).
    Returns the new state and optionally add error messages.
 *)
let exec_intra (op:jopcode) (a:astate * string list) : astate * string list =
  match op with
    
  (* register operations *)

  | OpLoad (t,index) ->
     a |> load_check_basic index t |> push_basic t
    
  | OpStore (t,index) ->
     (match t, peek a with
      | `Object, (A_pc _, _) ->
         (* special case: astore can be used to store a return address *)
         let v,a = pop a in
         a |> store index v
      | _ ->
         a |> pop_check_basic t |> store_basic index t
     )
    
  | OpIInc (index,_) ->
     a |> load_check index A_int |> store index A_int
    
  (* stack operations *)
    
  | OpPop ->
     a |> pop |> snd
  | OpPop2 ->
     a |> pop |> snd |> pop  |> snd
  | OpDup ->
     let r, _ = peek a in push r a
  | OpDupX1 ->
     let r1, a = pop a in
     let r2, a = pop a in
     a |> push r1 |> push r2 |> push r1
  | OpDupX2 ->
     let r1, a = pop a in
     let r2, a = pop a in
     let r3, a = pop a in
     a |> push r1 |> push r3 |> push r2 |> push r1
  | OpDup2 ->
     let r1, a = pop a in
     let r2, a = pop a in
     a |> push r2 |> push r1 |> push r2 |> push r1
  | OpDup2X1 ->
     let r1, a = pop a in
     let r2, a = pop a in
     let r3, a = pop a in
     a |> push r2 |> push r1 |> push r3 |> push r2 |> push r1
  | OpDup2X2 ->
     let r1, a = pop a in
     let r2, a = pop a in
     let r3, a = pop a in
     let r4, a = pop a in
     a |> push r2 |> push r1 |> push r4 |> push r3 |> push r2 |> push r1
  | OpSwap ->
     let r1, a = pop a in
     let r2, a = pop a in
     a |> push r1 |> push r2
     
  | OpConst (`ANull | `Class _ | `String _) -> a |> push A_ref
  | OpConst (`Byte _ | `Int _ | `Short _) -> a |> push A_int
  | OpConst (`Float _) -> a |> push A_float
  | OpConst (`Long _)-> a |> push_long
  | OpConst (`Double _) -> a |> push_double
                         
  (* arithmetic *)
                         
  | OpNeg t ->
     a |> pop_check_basic t |> push_basic t
    
  | OpAdd t | OpSub t | OpMult t | OpDiv t | OpRem t ->
     a |> pop_check_basic t |> pop_check_basic t |> push_basic t
    
  | OpIShl | OpIShr | OpIUShr | OpIAnd | OpIOr | OpIXor ->
     a |> pop_check A_int |> pop_check A_int |> push A_int
    
  | OpLAnd | OpLOr | OpLXor ->
     a |> pop_check_long |> pop_check_long |> push_long
    
  | OpLShl | OpLShr | OpLUShr ->
     a |> pop_check A_int |> pop_check_long |> push_long
    
  (* conversions *)
    
  | OpI2L -> a |> pop_check A_int |> push_long
  | OpI2F -> a |> pop_check A_int |> push A_float
  | OpI2D -> a |> pop_check A_int |> push_double
  | OpL2I -> a |> pop_check_long |> push A_int
  | OpL2F -> a |> pop_check_long |> push A_float
  | OpL2D -> a |> pop_check_long |> push_double
  | OpF2I -> a |> pop_check A_float |> push A_int
  | OpF2L -> a |> pop_check A_float |> push_long
  | OpF2D -> a |> pop_check A_float |> push_double
  | OpD2I -> a |> pop_check_double |> push A_int
  | OpD2L -> a |> pop_check_double |> push_long
  | OpD2F -> a |> pop_check_double |> push A_float
  | OpI2B | OpI2C | OpI2S -> a |> pop_check A_int |> push A_int
                           
  (* comparisons *)
                           
  | OpCmp (`DG | `DL) ->
     a |> pop_check_double |> pop_check_double |> push A_int
    
  | OpCmp (`FG | `FL) ->
     a |> pop_check A_float |> pop_check A_float |> push A_int
    
  | OpCmp `L ->
     a |> pop_check_long |> pop_check_long |> push A_int
    
  | OpIf ((`Eq | `Ge | `Gt | `Le | `Lt | `Ne), _) ->
     a |> pop_check A_int
    
  | OpIf ((`NonNull | `Null), _) ->
     a |> pop_check A_ref
    
  | OpIfCmp ((`AEq | `ANe), _) ->
     a |> pop_check A_ref |> pop_check A_ref
    
  | OpIfCmp ((`IEq | `IGe | `IGt | `ILe | `ILt | `INe), _) ->
     a |> pop_check A_int |> pop_check A_int
    
  (* jumps *)
    
  | OpGoto _ -> a
              
  | OpTableSwitch _| OpLookupSwitch _ -> a |> pop_check A_int
                                       
  (* object operations *)
                                       
  | OpNew _ -> a |> push A_ref
  | OpNewArray _ -> a |> pop_check A_int |> push A_ref
  | OpAMultiNewArray (_,d) -> a |> apply_n d (pop_check A_int) |> push A_ref
  | OpCheckCast _ -> a |> pop_check A_ref |> push A_ref
  | OpInstanceOf _ -> a |> pop_check A_ref |> push A_int
                    
  | OpGetStatic (_,t) -> a |> push_value (fs_type t)
  | OpPutStatic (_,t) -> a |> pop_check_value (fs_type t)
  | OpGetField (_,t) -> a |> pop_check A_ref |> push_value (fs_type t)
  | OpPutField (_,t) -> a |> pop_check_value (fs_type t) |> pop_check A_ref
                      
  | OpArrayLength ->
     a |> pop_check A_ref |> push A_int
  | OpArrayLoad t ->
     a |> pop_check A_int |> pop_check A_ref |> push_basic t
  | OpArrayStore t ->
     a |> pop_check_basic t |> pop_check A_int |> pop_check A_ref 
    
  (* calls *)
    
  | OpInvoke (i,s) ->
     (* pop arguments (in reverse order) *)
     let a =
       List.fold_left
         (fun a vt -> a |> pop_check_value vt)
         a (List.rev (ms_args s))
     in
     (* pop object *)
     let a = match i with
       | `Interface _ | `Special _ | `Virtual _ -> a |> pop_check A_ref
       | `Dynamic _ | `Static _ -> a
     in
     (* push result *)
     (match ms_rtype s with
      | None -> a
      | Some vt -> a |> push_value vt
     )
     
  | OpReturn t -> a |> pop_check_basic t
                
  (* specials *)
                
  | OpThrow ->
     let a = pop_check A_ref a in
     { (fst a) with a_stack = [A_ref] }, snd a
     
  | OpMonitorEnter | OpMonitorExit -> a |> pop_check A_ref
                                    
  | OpNop | OpBreakpoint -> a
                          
  | OpInvalid -> fst a, ("invalid opcode")::(snd a)
               
  | OpJsr _ | OpRet _ ->
     failwith "jsr and ret not handled in exec_intra"

    
(** Lift exec_intra partition-wise. *)
let exec_normal (op:jopcode) (x:pastate) : pastate * string list =
  SubMap.fold
    (fun s a (m,err) ->
      let r,er = exec_intra op (a,err) in
      SubMap.add s r m, er
    )
    x (SubMap.empty, [])

  
(** [jsr] handling. 
    [next] is the location following the jsr ([F_java_ret_site]).
    Recursive calls are considered an error.
 *)
let exec_jsr (next:node_id) (x:pastate) : pastate * string list =
  SubMap.fold
    (fun s a (m,err) ->
      let r,er = push (A_pc next) (a,err) in
      (* check recursivity *)
      if List.exists (fun x -> compare_node_id next x = 0) s
      then
        (* recursive calls are cut *)
        m, "recursive subroutine"::err
      else
        (* update partition *)
        let ss = next::s in
        SubMap.add ss r m, er
    )
    x (SubMap.empty, [])


(** [ret] handling.
    Returns one abstract state for each possible return site.
 *)
let exec_ret (index:int) (x:pastate) : pastate TagLocMap.t * string list =
  let add l s a m =
    let old = try TagLocMap.find l m with Not_found -> SubMap.empty in
    if SubMap.mem s old then failwith "exec: duplicate subroutine stack in ret";
    TagLocMap.add l (SubMap.add s a old) m
  in
  SubMap.fold
    (fun s a (m,err) ->
      match load index (a,err), s with
        
      | A_pc ret1, ret2::rest ->
         (* check the return address *)
         if compare_node_id ret1 ret2 <> 0 then
           m, "ret does not return to its call site"::err
         else
           (* pop the return address & use it as index *)
           add ret1 rest a m, err
        
      | _, [] -> m, "ret on an empty call stack"::err
      | _, _ ->  m, "invalid type in ret: bytecode address expected"::err
    )
    x (TagLocMap.empty, [])


(** Executes an opcode block.
    Returns a single abstract state for non-ret instruction.
    Returns a map from return sites to abstract states for ret.
    Also returns a list of errors, with location information.
 *)
let exec_block (loc:node_id) (ret:node_id option) (l:jopcode_range list) (a:pastate)
    : pastate with_bot * pastate TagLocMap.t * err list =
  let add_err op err' err =
    (List.map (fun msg -> loc, JPrint.jopcode op, msg) err')@err
  in
  let rec doit l (a,err) =
    match l with
    | [] ->
       (* ends on a normal opcode *)
       Nb a, TagLocMap.empty, err

    | [OpJsr _ as op,_] ->
       (* ends on a jsr opcode *)
       let next = match ret with
         | Some n -> n | _ -> failwith "jsr without ret_site edge" in
       let a',err' = exec_jsr next a in
       Nb a', TagLocMap.empty, add_err op err' err

    | [OpRet index as op,_] ->
       (* ends on a ret opcode *)
       let b,err' = exec_ret index a in
       BOT, b, add_err op err' err
       
    | (op,_)::rest ->
       (* normal opcode *)
       let a',err' = exec_normal op a in
       doit rest (a', add_err op err' err)
  in
  doit l (a,[])


(** State when the exception is propagated to its handler. *)  
let exec_exn (x:pastate) : pastate =
  SubMap.map (fun a -> { a with a_stack = [A_ref] }) x


(** Initial state at method entry. *)
let method_start (static:bool) (args:value_type list) =
  let a = astate_empty, [] in
  let idx = ref 0 in
  let a = if static then a else (incr idx; store 0 A_ref a) in
  let a, err =
    List.fold_left
      (fun a t ->
        let a = store_value !idx t a in
        idx := !idx + size_value t;
        a
      )
      a args
  in
  SubMap.singleton [] a, err

  

(*==========================================================================*)
                        (** {2 Iterator} *)
(*==========================================================================*)


  (* TODO:
     - check return 
     - ensure termination (limiting the stack should be enough)
   *)

let nb_errors = ref 0
  
module Domain = struct

  type t = pastate

  type manager = j_method

  (* error printing *)
  let print_err l =
    nb_errors := !nb_errors + List.length l;
    if dump_errors then
      List.iter
        (fun (loc,op,msg) ->
          Format.printf
            "precheck error at %a for %s: %s@\n"
            pp_node_id loc op msg
        ) l

  let bot m = SubMap.empty

  let entry m id n flow =
    let init,err = method_start m.m_static m.m_args in
    print_err err;
    init

  let join m loc a b =
    let r,err = join_pastate a b in
    print_err (List.map (fun msg -> loc,"join",msg) err);
    if trace then
      Format.printf
        "join %a@.  @[%a@]@.  @[%a@]@.  @[%a@]@."
        pp_node_id loc pp_pastate a pp_pastate b pp_pastate r;
    r
                 
  let widen = join

  let join_list m loc l =
    List.fold_left (join m loc) SubMap.empty l
            
  let subset m loc = subset_pastate

  let exec m loc i e =

    (* merge abstract state inputs *)
    let x = join_list m loc (List.map snd i) in
    
    (* get ret site associated to a jsr instruction *)
    let ret = match CFG.edge_dst_port e F_java_ret_site with
      | [n] -> Some (CFG.node_id n)
      | _ -> None
    in
    
    (* execute transfer function *)
    let ops = match (CFG.edge_data e).skind with
      | S_java_opcode ops -> ops
      | _ -> failwith "not Java bytecode in Precheck.Domain.exec"
    in
    let rn, rr, err = exec_block loc ret ops x in
    print_err err;

    if trace then
      Format.printf
        "exec %a@.  @[%a@] -> @[%a@] @[%a@]@."
        pp_node_id loc pp_pastate x
        (bot_fprint pp_pastate) rn
        (TagLocMap.fprint MapExt.printer_default pp_node_id pp_pastate) rr;

    (* non-ret propagation *)
    let acc =
      match rn with
      | BOT -> []
      | Nb rn ->
         List.fold_left
           (fun acc (t,n) ->
             match t with
             | T_cur | F_java_if_true
             | F_java_jsr | F_java_return_site | F_java_switch _
               ->
                (* direct propagation *)
                (t,rn)::acc
                
             | F_java_exn ->
                (* exception *)
                (t, (exec_exn rn))::acc

             | F_java_ret _ | F_java_ret_site ->
                (* no direct propagation to from jsr to the ret site *)
                acc
          
             | _ -> failwith "not a Java flow in Precheck.Domain.exec"
           )
           [] (CFG.edge_dst e)
    in

    (* add ret propagation *)
    TagLocMap.fold
      (fun l r acc ->
        (* add the connction to the ret site, if needed *)
        let n = CFG.get_node m.m_cfg.cfg_graph l in
        CFG.edge_add_dst_unique e (F_java_ret l) n;
        (* add flow *)
        (F_java_ret l, r)::acc
      )
      rr acc
    
end


module Analyze = Cfg.Iterator.Make(Domain)(Cfg.Iterator.SetWorklist)
              

let analyze (m:j_method) =
  let r = Analyze.iterate m m.m_cfg in
  (* print result *)
  if dump_result then (
    Format.printf "precheck results:@\n";
    CFG.iter_nodes_ordered
      (fun id _ ->
        Format.printf
          "  %a: @[%a@]@\n"
          pp_node_id id pp_pastate (TagLocHash.find r id)
      ) m.m_cfg.cfg_graph;
    Format.printf "@\n"
  )

