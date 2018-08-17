(****************************************************************************)
(*                   Copyright (C) 2018 The MOPSA Project                   *)
(*                                                                          *)
(*   This program is free software: you can redistribute it and/or modify   *)
(*   it under the terms of the CeCILL license V2.1.                         *)
(*                                                                          *)
(****************************************************************************)

(** Simple inter-method analysis to type the stack and register,
    and infer the targets of returns from subroutines (ret).
    This analysis is similar in aim to the bytecode verification performed
    by the JVM.
 *)

open Javalib_pack
open Javalib
open JBasics
open JCode
open Framework.Ast
open Framework.Pp
open Cfg.Ast
open Ast
open Bot


let name = "jvm.precheck"
let debug fmt = Debug.debug ~channel:name fmt



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
  | A_pc of LocSet.t (** return address from subroutine *)
  | A_top


module IntMap = MapExt.IntMap

              
(** Abstract environment. *)
type astate = {
    a_stack: aval list; (** topmost element first *)
    a_reg: aval IntMap.t;
  }

type astate_bot = astate with_bot
            
            
             
(*==========================================================================*)
                    (** {2 Basic abstract operators} *)
(*==========================================================================*)


(** Utilities. *)

let astate_empty =
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
  | A_pc l ->
     Format.fprintf
       fmt "pc%a"
       (LocSet.fprint SetExt.printer_default pp_location) l
           
let pp_astate fmt (a:astate) : unit =
  Format.fprintf
    fmt "@[stack=%a;@ regs=%a@]"
    (ListExt.fprint ListExt.printer_list pp_aval) a.a_stack
    (IntMap.fprint MapExt.printer_default Format.pp_print_int pp_aval) a.a_reg
           

(** Join *)

let join_val (v1:aval) (v2:aval) : aval = match v1, v2 with
  | A_pc s1, A_pc s2 -> A_pc (LocSet.union s1 s2)
  | x, y -> if x = y then x else A_top
                         
let join (a1:astate) (a2:astate) : astate * string list =
  (* join registers *)
  let r =
    IntMap.map2zo
      (fun _ _ -> A_top) (fun _ _ -> A_top) (fun _ -> join_val)
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
      | x1::s1, x2::s2 -> (join_val x1 x2)::(j s1 s2)
      | _ -> failwith "impossible case"
  in
  { a_reg = r;
    a_stack = j s1 s2;
  },
  err

let join_bot x y = match x,y with
  | BOT,x | x,BOT -> x,[]
  | Nb x, Nb y -> let r,err = join x y in Nb r, err
  
    

(** Ordering *)

let subset_val (v1:aval) (v2:aval) : bool = match v1, v2 with
  | A_pc s1, A_pc s2 -> LocSet.subset s1 s2
  | _, A_top -> true
  | _ -> v1 = v2

let subset (a1:astate) (a2:astate) : bool =
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

let subset_bot = bot_included subset
       
  

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


(** An error *)
type err =
  loc (** location *)
  * string (** operation *)
  * string (** error message *)

  
let exec (loc:loc) (ret:loc option) (op:jopcode) (x:astate) : astate * err list =
  let a = x,[] in
  let r,err = match op with

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
       a |> push r1 |> push r2 |> push r3 |> push r1
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
      
    | OpLShl | OpLShr | OpLUShr | OpLAnd | OpLOr | OpLXor ->
       a |> pop_check_long |> pop_check_long |> push_long

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

    | OpJsr _ ->
       (match ret with
        | None -> failwith "jsr without ret edge"
        | Some next -> a |> push (A_pc (LocSet.singleton next))
       )
                          
    | OpRet index ->
       a |> load_check index (A_pc LocSet.empty) 

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
       { (fst a) with a_stack = [A_ref] }, []

    | OpMonitorEnter | OpMonitorExit -> a |> pop_check A_ref

    | OpNop | OpBreakpoint -> a

    | OpInvalid -> x, ["invalid opcode"]

  in
  r, List.map (fun msg -> loc, JPrint.jopcode op, msg) err


let exec_block (loc:loc) (ret:loc option) (l:jopcode_range list) (a:astate)
    : astate * err list =
  List.fold_left
    (fun (a,err) (op,_) ->
      let a',err' = exec loc ret op a in
      a',err@err'
    )
    (a,[]) l

  
  
(** Initial state at method entry. *)
let method_start (static:bool) (args:value_type list) =
  let a = astate_empty, [] in
  let idx = ref 0 in
  let a = if static then a else (incr idx; store 0 A_ref a) in
  let a =
    List.fold_left
      (fun a t ->
        let a = store_value !idx t a in
        idx := !idx + size_value t;
        a
      )
      a args
  in
  a

  

(*==========================================================================*)
                        (** {2 Iterator} *)
(*==========================================================================*)


  (* TODO:
     - propagate through ret
     - exception handlers
     - check return 
     - ensure termination (!)
     - generalize, functorize and put in Cfg.Iterator
   *)
let analyze (m:j_method) =
  (* graph *)
  let g = m.m_cfg in
  (* abstract state *)
  let a = LocHash.create 16 in

  (* worklist *)
  let wl = ref LocSet.empty in
  let wl_add l = wl := LocSet.add l !wl
  and wl_is_empty () = LocSet.is_empty !wl
  and wl_get () =
    let m = LocSet.min_elt !wl in
    wl := LocSet.remove m !wl;
    m
  in

  (* error printing *)
  let print_err l =
    List.iter
      (fun (loc,op,msg) ->
        Format.printf
          "precheck error at %a for opcode %s: %s@\n"
          pp_location loc op msg
      ) l
  in
  
  (* update *)
  let get id = LocHash.find a id
  and set id v =
    let org = LocHash.find a id in
    if not (subset_bot v org) then (
      LocHash.replace a id v;
      wl_add id
    )
  in
    
  (* init abstract state *)
  CFG.iter_nodes (fun id _ -> LocHash.add a id BOT) g;
  let init,err = method_start m.m_static m.m_args in
  print_err err;
  List.iter
    (fun (_,n) -> set (CFG.node_id n) (Nb init))
    (CFG.entries g);

  (* iterate! *)
  while not (wl_is_empty()) do
    (* while there is some node in the work list *)
    let id = wl_get () in
    let n = CFG.get_node g id in
    match get id with
    | BOT -> ()
    | Nb an ->
       (* for each outgoing edge *)
       List.iter
         (fun (f,e) ->
           let ret = match CFG.edge_dst_tag e T_java_ret with
             | [n] -> Some (CFG.node_id n)
             | _ -> None
           in        
           match (CFG.edge_data e).skind with
           | S_java_opcode ops ->
              (* apply the block transfer function *)
              let an2,err2 = exec_block id ret ops an in
              print_err err2;
              (* for each destination node *)
              List.iter
                (fun (tag',n') ->
                  (* compute the join *)
                  let id' = CFG.node_id n' in
                  let an3,err3 = join_bot (get id') (Nb an2)  in
                  (* update the destination node *)
                  let err3 = List.map (fun msg -> id, "join", msg) err3 in
                  print_err err3;
                  set id' an3
                ) (CFG.edge_dst e)
           | _ -> failwith "not Java bytecode in Precheck.analyze"
         )
         (CFG.node_out n)
  done;

  (* print *)
  Format.printf "precheck results:@\n";
  CFG.iter_nodes_ordered
    (fun id _ ->
      Format.printf
        "  %a: %a@\n"
        pp_location id (bot_fprint pp_astate) (get id)
    ) g;
  Format.printf "@\n"
  
  
