(* todo: replace ...vuid < ...vuid by the proper compare *)
(* dans addr_underattr on fait des changements en place sur d2, mais d1 est fonctionnel, c'est nul *)
(* il faut aussi changer le 'a pyTypeInst sur les attributs. ça me fait penser qu'on pourrait utiliser un des domaines fournis par mopsa pour gérer cet encadrement *)
open Pp
open Framework.Ast
open Top
open MapExt
open SetExt
open Addr
open Ast
(* open Framework.Pp *)
open Universal.Ast
(* open Framework.Lattices.Powerset *)
type pyVar = Framework.Ast.var

let name = "python.types.typingdomain"
let debug fmt = Debug.debug ~channel:name fmt



(* module StringRange = Framework.Lattices.Range_set.Make(
 *                          struct
 *                            type t = string
 *                            let compare = Pervasives.compare
 *                            let print fmt s = Format.fprintf fmt "%s" s
 *                          end) *)
(* question: possible dans 'a pytype inst de declarer un module stringrange qui aurait pour type t stirng * 'a pytype ? il y a de la récursivité mutuelle un peu poussée là *)

type typeid = int
type 'a pytype =
  | Bot | Top
  | List of 'a pytype
  (* standard library containers are defined locally *)
  | Instance of 'a pytypeinst
  (* instances are described using a classname, and under-approximation of the attributes, and an over-approximation of the attributes *)
  | Class of class_address * py_object list (* class * mro *)
  (* we use the classes defined in MOPSA *)
  | Function of function_address * 'a summary
  (* same for functions *)
  | Module of module_address
  (* and for modules *)
  | Typevar of 'a
(* polymorphic variables, to use later *)
  | Method of function_address * typeid
and 'a pytypeinst =
  {classn:'a pytype; uattrs: 'a pytype StringMap.t; oattrs: 'a pytype StringMap.t }
and 'a summary = (('a pytype list) * ('a pytype list)) list


type typevar = int
type monotype = unit pytype
type polytype = typevar pytype

let builtin_inst s =
  let obj = Addr.find_builtin s in
  match Addr.kind_of_object obj with
  | A_py_class (c, b) ->
     Instance {classn=Class (c, b) ; uattrs=StringMap.empty; oattrs=StringMap.empty}
  | _ -> assert false

let map_printer = MapExtSig.{ print_empty = "∅";
                              print_begin = "{";
                              print_arrow = ":";
                              print_sep = ";";
                              print_end = "}"; }


let rec pp_pytype (print_vars:Format.formatter -> 'a -> unit)  (fmt:Format.formatter) (ty: 'a pytype) =
  match ty with
  | Bot -> Format.fprintf fmt "⊥"
  | Top -> Format.fprintf fmt "⊤"
  | List ty -> Format.fprintf fmt "List[%a]" (pp_pytype print_vars) ty
  | Instance {classn; uattrs; oattrs} ->
     if StringMap.is_empty uattrs && StringMap.is_empty oattrs then
       Format.fprintf fmt "Instance[%a]" (pp_pytype print_vars) classn
     else
       let pp_attrs = (StringMap.fprint map_printer (fun fmt k -> Format.fprintf fmt "%s" k) (pp_pytype print_vars)) in
       Format.fprintf fmt "Instance[%a, [%a], [%a]]" (pp_pytype print_vars) classn pp_attrs uattrs pp_attrs oattrs
  | Class (C_user c, _) -> Format.fprintf fmt "Class[{%a}]" pp_var c.py_cls_var
  | Class (C_builtin c, _) | Class (C_unsupported c, _) -> Format.fprintf fmt "Class[%s]" c
  | Function (F_user f, _) -> Format.fprintf fmt "Function[{%a}]" pp_var f.py_func_var
  | Function (F_builtin f, _) | Function (F_unsupported f, _) -> Format.fprintf fmt "Function[%s]" f
  | Module (M_user (m, _) | M_builtin(m)) -> Format.fprintf fmt "Module[%s]" m
  | Typevar t -> print_vars fmt t
  | Method (F_user f, i) -> Format.fprintf fmt "Method[{%a}, id=%d]" pp_var f.py_func_var i
  | Method (F_builtin f, i) | Method (F_unsupported f, i) -> Format.fprintf fmt "Method[{%s}, id=%d]" f i

let pp_typevar fmt (i:typevar) = Format.fprintf fmt "α(%d)" i

let pp_polytype = pp_pytype pp_typevar
let pp_monotype = pp_pytype (fun fmt (s:unit) -> Format.fprintf fmt "???")



type pyVarOrTy = Var of pyVar | Ty of typeid
type Universal.Ast.addr_kind += Type of pyVarOrTy
type 'a with_undefs = {lundef: bool; gundef: bool; def: 'a option}

let merge_undefs (r: 'a with_undefs) (s: 'b with_undefs) (c: 'c) : 'c with_undefs =
  let {lundef=rl;gundef=rg;def=rd} = r and
      {lundef=sl;gundef=sg;def=sd} = s in
  let resl = rl || sl and resg = rg || sg in
  {lundef=resl; gundef=resg; def=Some c}
  (* match rd, sd with
   * | None, _ -> {lundef=resl; gundef=resg; def=sd}, d
   * | _, None -> {lundef=resl; gundef=resg; def=rd}, d
   * | Some rd, Some sd -> let res, d = f rd sd d in
   *                       {lundef=resl; gundef=resg; def=Some res}, d *)

module PyVarOrTyOrUndefVarMap =
  Framework.Lattices.Total_map.Make
    (struct
      type t = var
      let compare v1 v2 = compare_var v1 v2
      let print = pp_var
    end)
    (struct
      type t = pyVarOrTy with_undefs
      let bottom = {lundef=false; gundef=false; def=None}
      let top = {lundef=true; gundef=true; def=Some (Ty (-1))}
      let is_bottom {lundef;gundef;def} =
        (not lundef) && (not gundef) && def = None
      let is_top {lundef;gundef;def} =
        lundef && gundef && def = Some (Ty (-1))
      let subset _ _ = assert false
      let join _ _ = assert false
      let meet _ _ = assert false
      let widen _ _ _ = assert false
      let print fmt {lundef;gundef;def} =
        let print_last fmt def = match def with
          | None -> Format.fprintf fmt "None"
          | Some (Var v) -> Format.fprintf fmt "v%a" pp_var v
          | Some (Ty i) -> Format.fprintf fmt "t%d" i
        in
        match lundef, gundef with
        | true, true ->
           Format.fprintf fmt "LU, GU, {%a}" print_last def
        | false, true ->
           Format.fprintf fmt "GU, {%a}" print_last def
        | true, false ->
           Format.fprintf fmt "LU, {%a}" print_last def
        | false, false ->
           print_last fmt def
    end)


module VarSet = Set.Make(
                    struct
                      type t = var
                      let compare v1 v2 = compare_var v1 v2
                      let print = pp_var
                    end)

module Typevarset = Set.Make(
                    struct
                      type t = typevar
                      let compare = compare
                      let print = pp_typevar
                    end)


module Monotypeset = Framework.Lattices.Powerset.Make(
                         struct
                           type t = monotype
                           let compare = Pervasives.compare
                           (* todo: bad idea ? *)
                           let print = pp_monotype
                         end)


module Polytypeset = Framework.Lattices.Powerset.Make(
                         struct
                           type t = polytype
                           let compare = Pervasives.compare
                           (* todo: bad idea ? *)
                           let print = pp_polytype
                         end)


module TypeIdMap = MapExt.Make(struct type t = typeid let compare = compare end)
module TypeVarMap = MapExt.Make(struct type t = typevar let compare = compare end)

module VarMap = PyVarOrTyOrUndefVarMap

(* invariant: if d1.(i) = VarId j, then j < i (unicity of aliasing will be better to handle afterwards I think) *)
type d1 = PyVarOrTyOrUndefVarMap.t (*pyVarOrTy with_undef VarMap.t (*pyVarOrTy array*)*)
type d2 = polytype TypeIdMap.t
type d3 = Monotypeset.t TypeVarMap.t (* on pourrait faire un set quand même... *)

let map_printer = MapExtSig.{ print_empty = "⊥";
                              print_begin = "{";
                              print_arrow = ":";
                              print_sep = ";";
                              print_end = "}"; }

type domain =
  { d1: d1;
    d2: d2;
    d3: d3;
    pos_d2: int;
    pos_d3: int }

let bottom = {d1=VarMap.empty; d2=TypeIdMap.empty; d3=TypeVarMap.empty; pos_d2=0; pos_d3=0}
let is_bottom {d1;d2;d3} =
  VarMap.is_bottom d1 (* && TypeIdMap.is_empty d2 && TypeVarMap.is_empty d3 *)

let top = {d1=VarMap.top; d2=TypeIdMap.empty; d3=TypeVarMap.empty; pos_d2=0; pos_d3=0}
let is_top {d1;d2;d3} =
  (* vérifier que dans d2 tout est à top plutôt... *)
  Debug.fail "ni"

let pp_d2 fmt (d2:d2) =
  TypeIdMap.fprint map_printer (fun fmt k -> Format.fprintf fmt "%d" k)
    pp_polytype fmt d2

let pp_d3 fmt (d3:d3) =
  TypeVarMap.fprint map_printer (fun fmt k -> Format.fprintf fmt "α(%d)" k)
    Monotypeset.print fmt d3

let print fmt {d1;d2;d3} =
  Format.fprintf fmt "@[d1=%a@\nd2=%a@\nd3=%a@]@\n" VarMap.print d1 pp_d2 d2 pp_d3 d3

let rec typeindex_of_var (d1:d1) (var:pyVar) =
  match (VarMap.find var d1).def with
  | None -> assert false
  | Some (Ty i) -> i
  | Some (Var v) -> typeindex_of_var d1 v

let typeindex_aliasing_of_var d1 var =
  match (VarMap.find var d1).def with
  | None -> assert false
  | Some (Ty i) -> i, None
  | Some (Var v) -> typeindex_of_var d1 var, Some v

let rec unsafe_cast (f:'a -> 'b) (t:'a pytype) : 'b pytype =
  match t with
  | Bot -> Bot
  | Top -> Top
  | Class (c, b) -> Class (c, b)
  | Function (func, summary) -> Function (func, List.map (fun (i, o) -> List.map (unsafe_cast f) i, List.map (unsafe_cast f) o) summary)
  | Module m -> Module m
  | Method (f, i) -> Method (f, i)
  | List v -> List (unsafe_cast f v)
  | Typevar v -> Typevar (f v)
  | Instance {classn;uattrs;oattrs} ->
     let classn = unsafe_cast f classn in
     let uattrs = StringMap.map (unsafe_cast f) uattrs in
     let oattrs = StringMap.map (unsafe_cast f) oattrs in
     Instance {classn; uattrs; oattrs}

let mono_cast (t:polytype) : monotype =
  unsafe_cast (fun _ -> assert false) t

let poly_cast (t:monotype) : polytype =
  unsafe_cast (fun _ -> assert false) t

let mono_set_cast (ts:Polytypeset.t) : Monotypeset.t =
  Polytypeset.fold (fun el monos -> Monotypeset.add (mono_cast el) monos) ts Monotypeset.empty

let rec subst (t:'a pytype) (var:'a) (value:'a pytype) =
  match t with
  | Typevar v when v = var -> value
  | List v -> List (subst v var value)
  | Instance {classn; uattrs; oattrs} ->
     let classn = subst classn var value in
     let uattrs = StringMap.map (fun v -> subst v var value) uattrs in
     let oattrs = StringMap.map (fun v -> subst v var value) oattrs in
     Instance {classn; uattrs; oattrs}
  | _ -> t

let collect_vars (t:polytype) : Typevarset.t =
  let rec collect t acc =
    match t with
    | List t -> collect t acc
    | Typevar v -> Typevarset.add v acc
    | Instance {classn; uattrs; oattrs} ->
       let acc = collect classn acc in
       let acc = StringMap.fold (fun _ v acc -> collect v acc) uattrs acc in
       StringMap.fold (fun _ v acc -> collect v acc) oattrs acc
    | _ -> acc
  in collect t Typevarset.empty

let concretize_poly (t:polytype) (d3:d3) : Monotypeset.t =
  (* TODO: test that function *)
  (* to concretize a list of polymorphic types into a list of monomorphic ones, we go through the list of variables used in the polymorphic types *)
  (* for each type variable, we create every possible assignation given the environment of type variables d3, and substitute the variable with its value for each value. we remove duplicates and *)
  (* then we recurse until we have no more variables *)
  let vars = collect_vars t in
  let concretize_tvar (tvar:typevar) (ts:Polytypeset.t) : Polytypeset.t =
    let tvar_values = TypeVarMap.find tvar d3 in
    let r = Monotypeset.fold
              (fun value acc_tvar_othervalues ->
                Polytypeset.fold
                  (fun t acc -> Polytypeset.add (subst t tvar (poly_cast value)) acc)
                  ts acc_tvar_othervalues
              ) tvar_values Polytypeset.empty in
    r
  in
  let res_poly = Typevarset.fold concretize_tvar vars (Polytypeset.add t (Polytypeset.empty)) in
  mono_set_cast res_poly

let join_classes (c:class_address * py_object list) (c':class_address * py_object list) : (class_address  * py_object list) option =
  debug "Join_classes: FIXME@\n";
  if c = c' then Some c
  else None

let supp_attr (m: 'a StringMap.t) : StringSet.t =
  StringMap.fold (fun k _ acc -> StringSet.add k acc) m StringSet.empty

let search_d3 (ms:Monotypeset.t) (d3:d3) : typevar option =
  TypeVarMap.fold (fun k v acc -> match acc with
                                  | None -> if Monotypeset.equal v ms then Some k else None
                                  | Some v -> Some v) d3 None

let merge_summaries (sum:'a summary) (sum':'a summary) : 'a summary =
  (* TODO: proper merge *)
  sum @ sum'

let rec join_poly (t, d3: polytype * d3) (t', d3': polytype * d3) (d3_acc:d3) (pos_d3:int) : polytype * d3 * int =
  match t, t' with
  | Top, _ | _, Top -> Top, d3_acc, pos_d3
  | Bot, Bot -> Bot, d3_acc, pos_d3
  | Bot, e | e, Bot ->
     let vars = collect_vars e in
     let d3 = if t = Bot then d3' else d3 in
     Typevarset.fold (fun v (ty, d3_acc, pos_d3) ->
       (* for each v in vars, we need to substitute v by pos_d3 in ty, and add the binding d3_acc.(pos_d3) to the value of d3.v *)
         let ty = subst ty v (Typevar pos_d3) in
         let d3_acc = TypeVarMap.add pos_d3 (TypeVarMap.find v d3) d3_acc in
         ty, d3_acc, pos_d3+1)
       vars (e, d3_acc, pos_d3)
  | List t, List t' ->
     let t, d3_acc, pos_d3 = join_poly (t, d3) (t', d3') d3_acc pos_d3 in
     List t, d3_acc, pos_d3
  | Function (f, sum), Function (f', sum') when f = f' ->
     Function (f, merge_summaries sum sum'), d3_acc, pos_d3
  | Module m, Module m' when m = m' ->
     Module m, d3_acc, pos_d3
  | Class (c, b), Class (c', b') when join_classes (c, b) (c', b') <> None ->
     let ty = match join_classes (c, b) (c', b') with
       | None -> assert false
       | Some (cl, bl) -> Class (cl, bl) in
     ty, d3_acc, pos_d3
  | Instance {classn=c; uattrs=u ; oattrs=o},
    Instance {classn=c'; uattrs=u'; oattrs=o'} when match c, c' with
                                                    | Class (c, b), Class (c', b') -> join_classes (c, b) (c', b') <> None
                                                    | _ -> false ->
     (* FIXME: if classn <> c and classn <> c', we should add the attributes in c not in classn and in c' not in classn to the optional attributes *)
     (* we join c and c', we join the common attributes of u and u', and keep the rest to join it with those of o and o' *)
     let classn, d3_acc, pos_d3 = join_poly (c, d3) (c', d3') d3_acc pos_d3 in
     let supp_u = supp_attr u and supp_u' = supp_attr u'
         and supp_o = supp_attr o and supp_o' = supp_attr o' in
     let supp_uattrs = StringSet.inter supp_u supp_u' in
     let supp_oattrs = StringSet.union (StringSet.union supp_o supp_o') (StringSet.diff (StringSet.union supp_u supp_u') supp_uattrs) in
     let join_uattrs = (fun attrn (acc, d3_acc, pos_d3) ->
         let new_t, d3_acc, pos_d3 = join_poly (StringMap.find attrn u, d3) (StringMap.find attrn u', d3') d3_acc pos_d3 in
         let new_acc = StringMap.add attrn new_t acc in
         new_acc, d3_acc, pos_d3) in
     let uattrs, d3_acc, pos_d3 = StringSet.fold join_uattrs supp_uattrs (StringMap.empty, d3_acc, pos_d3) in
     let uo = StringMap.merge (fun k l r -> match l, r with
                                            | Some l, None -> Some l
                                            | None, Some r -> Some r
                                            | _ -> assert false) u o
     and uo' = StringMap.merge (fun k l r -> match l, r with
                                            | Some l, None -> Some l
                                            | None, Some r -> Some r
                                            | _ -> assert false) u' o' in
     let join_oattrs = fun attrn (acc, d3_acc, pos_d3) ->
       match StringMap.mem attrn uo, StringMap.mem attrn uo' with
       | true, true ->
          let new_t, d3_acc, pos_d3 = join_poly (StringMap.find attrn uo, d3) (StringMap.find attrn uo', d3') d3_acc pos_d3 in
          let new_acc = StringMap.add attrn new_t acc in
          new_acc, d3_acc, pos_d3
       | true, false ->
          let new_acc = StringMap.add attrn (StringMap.find attrn uo) acc in
          new_acc, d3_acc, pos_d3
       | false, true ->
          let new_acc = StringMap.add attrn (StringMap.find attrn uo') acc in
          new_acc, d3_acc, pos_d3
       | false, false -> assert false in
     let oattrs, d3_acc, pos_d3 = StringSet.fold join_oattrs supp_oattrs (StringMap.empty, d3_acc, pos_d3) in
     Instance {classn; uattrs; oattrs}, d3_acc, pos_d3
  (* | Typevar u, _ when Monotypeset.subset (concretize_poly t' d3') (TypeVarMap.find u d3) -> *)
  (*** oups, what if the variable u is already used in d3_acc ? ***)
  | _, _ ->
     let concr_t = concretize_poly t d3 and
         concr_t' = concretize_poly t' d3' in
     let concr = Monotypeset.union () concr_t concr_t' in
     match search_d3 concr d3_acc with
     | None ->
        let d3_acc = TypeVarMap.add pos_d3 concr d3_acc in
        Typevar pos_d3, d3_acc, (pos_d3+1)
     | Some i ->
        Typevar i, d3_acc, pos_d3

let search_d2 (t:polytype) (d2:d2) : typeid option =
  TypeIdMap.fold (fun k v acc -> match acc with
                                  | None -> if v = t then Some k else None
                                  | Some v -> Some v) d2 None


let get_type ?local_use:(local_use=false) (d:domain) (t:polytype) : typeid * domain =
  if not local_use && not (Typevarset.is_empty (collect_vars t)) then
    Debug.fail "get_type: not monomorphic, bad idea"
  else
    let opos = search_d2 t d.d2 in
    match opos with
    | None ->
       let d2 = TypeIdMap.add d.pos_d2 t d.d2 in
       d.pos_d2, {d with d2=d2; pos_d2=d.pos_d2+1}
    | Some pos -> pos, d


let join (d:domain) (d':domain) : domain =
  (* FIXME: see if union of vars still (as we are now handling the undefs *)
  debug "Joining %a and@\n%a@\n" print d print d';
  let h = Hashtbl.create (2 * (fst (TypeIdMap.max_binding d.d2))) in
  let all_vars = VarMap.fold (fun k _ acc -> VarSet.add k acc) d.d1 VarSet.empty in
  let all_vars = VarMap.fold (fun k _ acc -> VarSet.add k acc) d'.d1 all_vars in
  let res = VarSet.fold
              (fun var dcur ->
                (* debug "Var is %a@\n" pp_var var; *)
      let el1 = VarMap.find var d.d1 and el1' = VarMap.find var d'.d1 in
      match el1.def, el1'.def with
      | None, None ->
         let el = {lundef=el1.lundef||el1'.lundef;
                   gundef=el1.gundef||el1'.gundef;
                   def=None} in
         let d1 = VarMap.add var el dcur.d1 in
         {dcur with d1}
      | None, Some e | Some e, None ->
         (* hum, il faut quand même transférer le type et les variables de types associées *)
         (* FIXME: aliasing *)
         let {d1;d2;d3} = if el1.def = None then d' else d in
         let tid, ov = typeindex_aliasing_of_var d1 var in
         let new_ty, d3, pos_d3 = join_poly (TypeIdMap.find tid d2, d3) (Bot, d3) dcur.d3 dcur.pos_d3 in
         debug "Type is %a@\n" pp_polytype new_ty;
         let tid, dcur = get_type ~local_use:true {dcur with d3;pos_d3} new_ty in
         let d1 = VarMap.add var (merge_undefs el1 el1' (Ty tid)) dcur.d1 in
         debug "Tid is %d; Result is %a@\n" tid print {dcur with d1};
         {dcur with d1}
      | Some _, Some _ ->
         let tid1, ov1 = typeindex_aliasing_of_var d.d1 var and
             tid2, ov2 = typeindex_aliasing_of_var d'.d1 var in
         begin match Hashtbl.mem h (tid1, tid2) with
         | true ->
            (* this equivalence class is already known *)
            let r = Hashtbl.find h (tid1, tid2) in
            (* there are now two cases:
                  1) if ov1 <> None and ov1 = ov2 and the concrete types described by d2.(tid1) and d2.(tid2) are the same , there is the same aliasing so we can preserve it (we need to find the good root for the aliasing?! <- nope, due to the fact the all variable alias to the one having the smallest vuid <- todo: fix vuid issue)
                  2) otherwise, we bind the result of this variable to the good type id
             *)
            begin match ov1, ov2 with
            | Some v1, Some v2 when v1 = v2 && concretize_poly (TypeIdMap.find tid1 d.d2) d.d3 = concretize_poly (TypeIdMap.find tid2 d'.d2) d'.d3 ->
               let r = merge_undefs el1 el1' (Var v1) in
               {dcur with d1 = VarMap.add var r dcur.d1}
            | _, _ ->
               let r = merge_undefs el1 el1' (Ty r) in
               {dcur with d1 = VarMap.add var r dcur.d1} end
         | false ->
            let pos_d2 = dcur.pos_d2 in
            Hashtbl.add h (tid1, tid2) pos_d2;
            let t1 =
              if TypeIdMap.mem tid1 d.d2 then TypeIdMap.find tid1 d.d2
              else Bot in
            let t2 =
              if TypeIdMap.mem tid2 d'.d2 then TypeIdMap.find tid2 d'.d2
              else Bot in
            let new_ty, d3, pos_d3 = join_poly (t1, d.d3) (t2, d'.d3) dcur.d3 dcur.pos_d3 in
            let d1 = VarMap.add var (merge_undefs el1 el1' (Ty pos_d2)) dcur.d1 in
            let d2 = TypeIdMap.add pos_d2 new_ty dcur.d2 in
            let pos_d2 = pos_d2+1 in
            {d1;d2;d3;pos_d2;pos_d3}
         end)  all_vars top
  in
  res

let class_le (c, b:class_address * py_object list) (d, b':class_address * py_object list) : bool =
  Debug.warn "class_le not correctly implemented@\n";
  List.exists (fun x -> match (fst x).addr_kind with
                        | A_py_class (x, _) -> x = d
                        | _ -> false) b (* = d && b = b' *)
  (* let cname = match c with
   *   | C_builtin s | C_unsupported s -> s
   *   | C_user c -> c.py_cls_var.vname in
   * let dname = match d with
   *   | C_builtin s | C_unsupported s -> s
   *   | C_user s -> s.py_cls_var.vname in
   * let oc = if Addr.is_builtin_name cname then Addr.find_builtin cname
   *          else
   *            (\* TODO: FIXME: mro *\)
   *            {addr_kind=A_py_class (c, b); addr_range=Range_fresh (-1); addr_uid=(-1)}, {ekind=E_py_undefined true; etyp=T_any; erange=Range_fresh (-1)}
   * in
   * let od = if Addr.is_builtin_name dname then Addr.find_builtin dname
   *          else
   *                         {addr_kind=A_py_class (d, b'); addr_range=Range_fresh (-1); addr_uid=(-1)}, {ekind=E_py_undefined true; etyp=T_any; erange=Range_fresh (-1)}
   * in
   * issubclass oc od *)

let is_mono (u:polytype)  : bool =
  Typevarset.is_empty (collect_vars u)

let rec polytype_leq (t, d3: polytype * d3) (t', d3' : polytype * d3) : bool =
  match t, t' with
  | Bot, _ | _, Top -> true
  | List u, List v -> polytype_leq (u, d3) (v, d3')
  | Class (c, b), Class (c', b') -> class_le (c, b) (c', b')
  | Function (f, sum), Function (f', sum') -> f = f' && Debug.fail "ni"
  | Module m, Module m' -> m = m'
  | Instance {classn=c; uattrs=u; oattrs=o},
    Instance {classn=c'; uattrs=u'; oattrs=o'} ->
     polytype_leq (c, d3) (c', d3') &&
       StringMap.fold (fun attr tya acc ->
           acc && StringMap.mem attr u' && polytype_leq (tya, d3) (StringMap.find attr u', d3')
         ) u true &&
         StringMap.fold (fun attr tya acc ->
             acc && ((StringMap.mem attr o' && polytype_leq (tya, d3) (StringMap.find attr o', d3'))
                     || (StringMap.mem attr u' && polytype_leq (tya, d3) (StringMap.find attr u', d3')))) o true
  | Typevar u, _ ->
     let us = TypeVarMap.find u d3 in
     Monotypeset.fold (fun ty acc ->
         acc && polytype_leq (poly_cast ty, d3) (t', d3')) us true
  | _, Typevar v ->
     if is_mono t then
       Monotypeset.mem (mono_cast t) (TypeVarMap.find v d3')
     else
       let concrt = concretize_poly t d3 in
       Monotypeset.fold (fun ty acc ->
           acc && polytype_leq (poly_cast ty, d3) (t', d3')) concrt true
  | _, _ -> false

let leq d d' =
  VarMap.fold (fun id1 tid1 acc ->
      if tid1.lundef || tid1.gundef || tid1.def = None then acc
      else
        let tid1, ov1 = typeindex_aliasing_of_var d.d1 id1 in
        let tid2, ov2 = typeindex_aliasing_of_var d'.d1 id1 in
        let ty1 = TypeIdMap.find tid1 d.d2 and
            ty2 = TypeIdMap.find tid2 d'.d2 in
        acc && (polytype_leq (ty1, d.d3) (ty2, d'.d3)) && (ov1 = None || ov1 = ov2)) d.d1 true

let meet d1 d2 = Debug.fail "ni"

let rec widening_poly (t, d3: polytype * d3) (t', d3': polytype * d3) (d3_acc:d3) (pos_d3:int) : polytype * d3 * int =
  (* FIXME: except for the last case, this is like the join. So maybe we should just give a flag to join_poly enforcing the widening *)
  match t, t' with
  | Top, _ | _, Top -> Top, d3_acc, pos_d3
  | Bot, Bot -> Bot, d3_acc, pos_d3
  | Bot, e | e, Bot ->
     let vars = collect_vars e in
     let d3 = if t = Bot then d3' else d3 in
     Typevarset.fold (fun v (ty, d3_acc, pos_d3) ->
       (* for each v in vars, we need to substitute v by pos_d3 in ty, and add the binding d3_acc.(pos_d3) to the value of d3.v *)
         let ty = subst ty v (Typevar pos_d3) in
         let d3_acc = TypeVarMap.add pos_d3 (TypeVarMap.find v d3) d3_acc in
         ty, d3_acc, pos_d3+1)
       vars (e, d3_acc, pos_d3)
  | List t, List t' ->
     let t, d3_acc, pos_d3 = widening_poly (t, d3) (t', d3') d3_acc pos_d3 in
     List t, d3_acc, pos_d3
  | Function (f, sum), Function (f', sum') when f = f' ->
     Function (f, Debug.fail "ni"), d3_acc, pos_d3
  | Module m, Module m' when m = m' ->
     Module m, d3_acc, pos_d3
  | Class (c, b), Class (c', b') when join_classes (c, b) (c', b') <> None ->
     let ty = match join_classes (c, b) (c', b') with
       | None -> assert false
       | Some (cl, bl) -> Class (cl, bl) in
     ty, d3_acc, pos_d3
  | Instance {classn=c; uattrs=u ; oattrs=o},
    Instance {classn=c'; uattrs=u'; oattrs=o'} when match c, c' with
                                                    | Class (c, b), Class (c', b') -> join_classes (c, b) (c', b') <> None
                                                    | _ -> false ->
     (* we join c and c', we join the common attributes of u and u', and keep the rest to join it with those of o and o' *)
     let classn, d3_acc, pos_d3 = widening_poly (c, d3) (c', d3') d3_acc pos_d3 in
     let supp_u = supp_attr u and supp_u' = supp_attr u'
         and supp_o = supp_attr o and supp_o' = supp_attr o' in
     let supp_uattrs = StringSet.inter supp_u supp_u' in
     let supp_oattrs = StringSet.union (StringSet.union supp_o supp_o') (StringSet.diff (StringSet.union supp_u supp_u') supp_uattrs) in
     let join_uattrs = (fun attrn (acc, d3_acc, pos_d3) ->
         let new_t, d3_acc, pos_d3 = widening_poly (StringMap.find attrn u, d3) (StringMap.find attrn u', d3') d3_acc pos_d3 in
         let new_acc = StringMap.add attrn new_t acc in
         new_acc, d3_acc, pos_d3) in
     let uattrs, d3_acc, pos_d3 = StringSet.fold join_uattrs supp_uattrs (StringMap.empty, d3_acc, pos_d3) in
     let uo = StringMap.merge (fun k l r -> match l, r with
                                            | Some l, None -> Some l
                                            | None, Some r -> Some r
                                            | _ -> assert false) u o
     and uo' = StringMap.merge (fun k l r -> match l, r with
                                            | Some l, None -> Some l
                                            | None, Some r -> Some r
                                            | _ -> assert false) u' o' in
     let join_oattrs = fun attrn (acc, d3_acc, pos_d3) ->
       match StringMap.mem attrn uo, StringMap.mem attrn uo' with
       | true, true ->
          let new_t, d3_acc, pos_d3 = widening_poly (StringMap.find attrn uo, d3) (StringMap.find attrn uo', d3') d3_acc pos_d3 in
          let new_acc = StringMap.add attrn new_t acc in
          new_acc, d3_acc, pos_d3
       | true, false ->
          let new_acc = StringMap.add attrn (StringMap.find attrn uo) acc in
          new_acc, d3_acc, pos_d3
       | false, true ->
          let new_acc = StringMap.add attrn (StringMap.find attrn uo') acc in
          new_acc, d3_acc, pos_d3
       | false, false -> assert false in
     let oattrs, d3_acc, pos_d3 = StringSet.fold join_oattrs supp_oattrs (StringMap.empty, d3_acc, pos_d3) in
     Instance {classn; uattrs; oattrs}, d3_acc, pos_d3
  | _, _ ->
     let concr_t = concretize_poly t d3 in
     let concr_t' = concretize_poly t' d3' in
     debug "concr_t = %a, concr_t' = %a@\n" Monotypeset.print concr_t Monotypeset.print concr_t';
     match Monotypeset.equal concr_t concr_t' with
     | true ->
        let concr = concr_t in
        begin match search_d3 concr d3_acc with
        | None ->
           let d3_acc = TypeVarMap.add pos_d3 concr d3_acc in
           Typevar pos_d3, d3_acc, (pos_d3+1)
        | Some i ->
           Typevar i, d3_acc, pos_d3
        end
     | false -> Top, d3_acc, pos_d3


let widening ctx d d' =
  debug "%a ∇ %a@\n" print d print d';
  (* if the shape of the domain is stable (i.e, variables point to the same type ids) *)
  (* we perform a widening on polytype * d3 for each partition *)
  (* otherwise, unstable partitions are set to top *)
  let is_stable =
    VarMap.fold (fun k tid acc ->
        acc && VarMap.mem k d'.d1 &&
          let tid'_wundef = VarMap.find k d'.d1 in
          tid'_wundef.lundef = tid.lundef &&
            tid'_wundef.gundef = tid.gundef &&
              tid'_wundef.def = tid.def
              (* ((tid'_wundef.def = None && tid.def = None) ||
               *    let tid = match tid.def with
               *      | Some (Ty t) -> t
               *      | Some (Var v) -> typeindex_of_var d.d1 v
               *      | _ -> assert false in
               *    let tid' = match tid'_wundef.def with
               *      | Some (Ty t) -> t
               *      | Some (Var v) -> typeindex_of_var d.d1 v
               *      | _ -> assert false in
               *    tid = tid') *)) d.d1 true
  in
  if is_stable then
    (
      let r1 = d.d1 in
      let r2, r3, pos_r3 =
        TypeIdMap.fold2 (fun tid ty ty' (r2, r3, pos_r3) ->
            let ty, r3, pos_r3 = widening_poly (ty, d.d3) (ty', d'.d3) r3 pos_r3 in
            TypeIdMap.add tid ty r2, r3, pos_r3
          ) d.d2 d'.d2 (TypeIdMap.empty, TypeVarMap.empty, 0) in
      let res = {d1=r1; d2=r2; d3=r3; pos_d2=d.pos_d2; pos_d3=pos_r3} in
      debug "Result of widening is %a@\n" print res;
      res
    )
  else
    Debug.fail "unstable widening not implemented. Recommended widening delay: at least 1"

let set_var (d:domain) (v:pyVar) (t:polytype with_undefs) : domain =
  if is_bottom d then bottom else
  (* before mapping v to t in d, we need to check that v is not the root for some aliasing tree *)
  let d = match (VarMap.find v d.d1).def with
  | None -> d
  | Some _ ->
     let tid, ov = typeindex_aliasing_of_var d.d1 v in
     (* we may update d1 *)
     match ov with
     | Some root -> (* the root is something else, we don't need to perform anything *)
        d
     | None ->
        (* we're searching for the elements having (Var v) as root *)
        let under_v = VarMap.fold (fun k value acc ->
                          if value.def = Some (Var v) then VarSet.add k acc else acc) d.d1 VarSet.empty in
        if VarSet.is_empty under_v then d (* no other element *)
        else
          let new_root = VarSet.min_elt under_v in
          let _, _, others = VarSet.split new_root under_v in
          let d1 = VarMap.add new_root {(VarMap.find new_root d.d1) with def=Some (Ty tid)} d.d1 in
          let d1 = VarSet.fold (fun o d1 ->
                       let new_ty = {(VarMap.find o d.d1) with def=Some (Var new_root)} in
                       VarMap.add o new_ty d.d1) others d1 in
          {d with d1=d1}
  in
  (* now, we check if t.def exists already...  *)
  let d, tovou (* Ty or Var or Undef *) =
    (* we may update d2 and pos_d2 *)
    match t.def with
    | None -> d, {lundef=t.lundef; gundef=t.gundef; def=None}
    | Some ty ->
       match search_d2 ty d.d2 with
       | None ->
          let d2 = TypeIdMap.add d.pos_d2 ty d.d2 in
          {d with d2=d2; pos_d2=d.pos_d2+1}, {lundef=t.lundef; gundef=t.gundef; def=Some (Ty d.pos_d2)}
       | Some i ->
          d, {lundef=t.lundef; gundef=t.gundef; def=Some (Ty i)}
  in
  (* the last thing is to update d1 *)
  let d1 = VarMap.add v tovou d.d1 in
  {d with d1=d1}

let set_var_tid (d:domain) (v:pyVar) (tid:typeid) : domain =
  let ty = TypeIdMap.find tid d.d2 in
  set_var d v {lundef=false; gundef=false; def=Some ty}

let set_var_eq (d:domain) (x:pyVar) (y:pyVar) (* x := y *) =
  if is_bottom d then bottom else
  let tid, ov = typeindex_aliasing_of_var d.d1 y in
  match ov with
  (* in both cases, the root for the aliasing will be the var with the minimal id *)
  | None -> (* there is no aliasing, so we just have to handle two variables in that case *)
     if y.vuid < x.vuid then
       (* the root doesn't change *)
         {d with d1=VarMap.add x {lundef=false; gundef=false; def=Some (Var y)} d.d1}
     else
         let d1 = VarMap.add x (VarMap.find y d.d1) d.d1 in
         let d1 = VarMap.add y {lundef=false; gundef=false; def=Some (Var x)} d1 in
         {d with d1}
  | Some old_root ->
     if old_root.vuid < x.vuid then
       (* the root doesn't change *)
       {d with d1=VarMap.add x {lundef=false; gundef=false; def=Some (Var x)} d.d1}
     else
       let d1 = VarMap.add x (VarMap.find old_root d.d1) d.d1 in
       let d1 = VarMap.map (fun el -> if el.def = Some (Var old_root) then {lundef=false; gundef=false; def= Some (Var x)} else el) d1 in
       {d with d1=d1}

(* TODO: change t:polytype into t:pyVarOrTy, and handle what was add_var_underattr_eq and add_var_underattr in the proof of concept *)
let set_var_attr (d:domain) (v:pyVar) (attr:string) (t:polytype) : domain =
  if is_bottom d then bottom else
  (* this is v.attr := t *)
  let tid, ov = typeindex_aliasing_of_var d.d1 v in
  (* if there are other types using d.d2.(tid), we cannot modify the instance in d.d2.(tid) and must be careful *)
  (* if v is a root for the aliasing, then we need to  *)
  let others_samety = VarMap.fold (fun k value acc ->
                          (* if acc then (bla, acc)
                           * else if (value.def = Some (Ty tid)) && compare_var k v  0 then
                           *   (k, true)
                           * else
                           *   (bla, acc) *)
                          acc || (value.def = Some (Ty tid) && compare_var k v <> 0)) d.d1 false in
  debug "set_var_attr, others = %b@\n" others_samety;
  let old_instance = TypeIdMap.find tid d.d2 in
  match others_samety with
  | true ->
     begin match old_instance with
     | Instance old_instance ->
        (* we created the new instance *)
        let new_instance = Instance {old_instance with uattrs = StringMap.add attr t old_instance.uattrs} in
        let d2 = TypeIdMap.add d.pos_d2 new_instance d.d2 in

        let new_oinstance = Instance {old_instance with oattrs = StringMap.add attr t old_instance.oattrs} in
        let d2 = TypeIdMap.add tid new_oinstance d2 in
        (* there are two cases to handle: either v is aliased with others or it isn't *)
        (* if it isn't, we bind v to pos_d2 then we have nothing else to do *)
           (* if it is, the root is ov, and we bind ov to pos_d2 *)
        let v_or_root = match ov with
          | None -> v
          | Some r -> r in
        let d1 = VarMap.add v_or_root {lundef=false; gundef=false; def=Some (Ty d.pos_d2)} d.d1 in
        let pos_d2 = d.pos_d2 + 1 in
        {d with d1; d2; pos_d2}
     | _ -> assert false
     end
  | false ->
     match old_instance with
     | Instance old_instance ->
        let new_instance = Instance {old_instance with uattrs = StringMap.add attr t old_instance.uattrs} in
        let d2 = TypeIdMap.add tid new_instance d.d2 in
        {d with d2=d2}
     | _ -> assert false


let set_var_attr_ty (d:domain) (v:pyVar) (attr:string) (tid:typeid) : domain =
  let ty = TypeIdMap.find tid d.d2 in
  set_var_attr d v attr ty



let rm_var (d:domain) (v:pyVar) : domain =
  (* we need to:
     - remove the binding in d1. If this variable was used as a root for the aliasing, we need to change of root
     - remove the type binding in d2 if it's not used anywhere else
     - remove the variable binding in d3 if needed
     Notes: - if there is aliasing, no need to clean in d2
            - if the type binding in d2 is kept, no need to clean in d3
   *)
  debug "Domain is %a@\n" print d;
  if is_bottom d then d
  else if (VarMap.find v d.d1).def = None then
    let d1 = VarMap.remove v d.d1 in
    {d with d1}
  else
    let tid, ov = typeindex_aliasing_of_var d.d1 v in
  let d1 = VarMap.remove v d.d1 in
  let d = {d with d1} in
  let under_v = VarMap.fold (fun k value acc -> if value.def = Some (Var v) then VarSet.add k acc else acc) d.d1 VarSet.empty in
  (* FIXME: un type peut être utilisé comme addresse (cf a = alloc(); C.__init__(a), on va enlever a/self à la fin de init mais il ne faut pas enelver le type *)
  match VarSet.is_empty under_v with
  | true ->
  (* no aliasing, so we may need to clean d2 and d3 *)
     let others_samety = VarMap.fold (fun k value acc -> acc || (value.def = Some (Ty tid))) d.d1 false in
     begin match others_samety with
     | true -> d
     | false ->
        (* let ty = TypeIdMap.find tid d.d2 in
         * let typevars_ty = collect_vars ty in
         *
         * let d2 = TypeIdMap.remove tid d.d2 in
         * let pos_d2 = if tid = (d.pos_d2-1) then d.pos_d2-1 else d.pos_d2 in *)
        (*{d with d2; pos_d2}*)
        d
        (* FIXME: clean d3 as well *)
     end
  | false ->
     (* there is aliasing, so no need to clean d2 or d3 *)
     let new_root = VarSet.min_elt under_v in
     let _, _, others = VarSet.split new_root under_v in
     let d1 = VarMap.add new_root {(VarMap.find new_root d.d1) with def=Some (Ty tid)} d.d1 in
     let d1 = VarSet.fold (fun o d1 ->
                  let new_ty = {(VarMap.find o d.d1) with def=Some (Var new_root)} in
                    VarMap.add o new_ty d.d1) others d1 in
     {d with d1}



let get_types (d:domain) (ts:Polytypeset.t) : typevar * domain =
  (* FIXME: perform the join of polytypes only, and everything should work by itself? *)
  (* folding on join_poly would work, but would create |ts|-1 type variables and only the last one would be used afterwards  *)
  if Polytypeset.exists (fun t -> not (Typevarset.is_empty (collect_vars t))) ts then
    Debug.fail "get_types: not monomorphic, bad idea"
  else
    let mts = mono_set_cast ts in
    let d3 = d.d3 and pos_d3 = d.pos_d3 in
    let opos = search_d3 mts d3 in
    let pos, d3, pos_d3 = match opos with
      | None ->
         let d3 = TypeVarMap.add pos_d3 mts d3 in
         pos_d3, d3, pos_d3+1
      | Some pos ->
         pos, d3, pos_d3
    in
    pos, {d with d3; pos_d3}


let get_mtypes (d:domain) (mts:Monotypeset.t) : typevar * domain =
  (* FIXME: perform the join of polytypes only, and everything should work by itself? *)
  (* folding on join_poly would work, but would create |ts|-1 type variables and only the last one would be used afterwards  *)
    let d3 = d.d3 and pos_d3 = d.pos_d3 in
    let opos = search_d3 mts d3 in
    let pos, d3, pos_d3 = match opos with
      | None ->
         let d3 = TypeVarMap.add pos_d3 mts d3 in
         pos_d3, d3, pos_d3+1
      | Some pos ->
         pos, d3, pos_d3
    in
    pos, {d with d3; pos_d3}


let class_of (d:domain) (t:typeid) : class_address * py_object list =
  match TypeIdMap.find t d.d2 with
  | Instance {classn=Class (c, b)} -> (c, b)
  | Class _ -> C_builtin "type", [Addr.find_builtin "object"]
  | _ -> Debug.fail "class_of: ni"

let get_polytype (d:domain) (v:pyVar) : polytype =
  let i = (VarMap.find v d.d1).def in
  let tid = match i with
    | Some i -> typeindex_of_var d.d1 v
    | _ -> raise Not_found (*Debug.fail "get_polytype"*)
  in
  TypeIdMap.find tid d.d2

let get_addr_kind (d:domain) (v:pyVar) : Universal.Ast.addr_kind =
  (* quand v pointe vers une classe/fonction, ressortir l'addresse *)
  let i = (VarMap.find v d.d1).def in
  let tid, i = match i with
    | None -> Debug.fail "get_tvid"
    | Some i -> typeindex_of_var d.d1 v, i in
  let ty = TypeIdMap.find tid d.d2 in
  match ty with
  | Class (c, b) -> A_py_class (c, b)
  | Function (f, _) -> A_py_function f
  | Module m -> A_py_module m
  | _ -> Type i


let rec filter_polyinst (p, d3: polytype * d3) (inst:monotype) : (polytype * d3) * (polytype * d3) =
  (* TODO: remove filter_monoinst? or at least write polymorphic function *)
  let cp x y = (x, y), (x, y) in
  match p with
  | Bot | Top | Module _ -> cp p d3
  | List t ->
     let list, listb = match kind_of_object (Addr.find_builtin "list") with
       | A_py_class (c, b) -> c, b
       | _ -> assert false in
     begin match inst with
     | Class (d, b) ->
        if class_le (list, listb) (d, b) then (p, d3), (Bot, d3) else (Bot, d3), (p, d3)
     | _ -> assert false
     end
  | Function (f, _) ->
     let func, funcb = match kind_of_object (Addr.find_builtin "function") with
       | A_py_class (c, b) -> c, b
       | _ -> assert false in
     begin match inst with
     | Class (d, b) ->
        if class_le (func, funcb) (d, b) then (p, d3), (Bot, d3) else (Bot, d3), (p, d3)
     | _ -> assert false
     end
  | Method _ -> Debug.fail "ni"
  | Class (c, b) ->
     debug "p=%a@\ninst=%a@\n" pp_polytype p pp_monotype inst; Debug.fail "Cni"
  | Instance {classn=Class (c, b)} ->
     begin match inst with
     | Class (d, b') ->
        if class_le (c, b) (d, b') then (p, d3), (Bot, d3) else (Bot, d3), (p, d3)
     | _ -> assert false
     end
  | Instance {classn} -> Debug.fail "ni"
  | Typevar var ->
     let m = concretize_poly p d3 in
     let t, f = Monotypeset.fold (fun el (acct, accf) ->
                    let (mt, _), (mf, _) = filter_polyinst (poly_cast el, d3) inst in
                    let mt = mono_cast mt and mf = mono_cast mf in
                    let acct = if mt = Bot then acct else Monotypeset.add mt acct in
                    let accf = if mf = Bot then accf else Monotypeset.add mf accf in
                    (acct, accf)) m (Monotypeset.empty, Monotypeset.empty) in
     (* one more optimization: if d3t.var contains just one element, we don't change d3, but change p *)
     let pt, d3t = match Monotypeset.cardinal t with
       | 0 -> Bot, d3
       | 1 -> poly_cast (Monotypeset.choose t), d3
       | _ -> p, TypeVarMap.add var t d3 in
     let pf, d3f = match Monotypeset.cardinal f with
       | 0 -> Bot, d3
       | 1 -> poly_cast (Monotypeset.choose f), d3
       | _ -> p, TypeVarMap.add var f d3 in
     (pt, d3t), (pf, d3f)


let filter_inst (d:domain) (t:typeid) (inst:monotype) : domain * domain =
  let ty = TypeIdMap.find t d.d2 in
  let (pt, d3t), (pf, d3f) = filter_polyinst (ty, d.d3) inst in
  let dt = match pt with
    | Bot -> bottom
    | _ -> let d2t = TypeIdMap.add t pt d.d2 in
           {d with d2=d2t; d3=d3t} in
  let df = match pf with
    | Bot -> bottom
    | _ -> let d2f = TypeIdMap.add t pf d.d2 in
           {d with d2=d2f; d3=d3f} in
  dt, df

let filter_ty_inst (d:domain) (ty:polytype) (inst:monotype) : domain * domain =
  let t, d = get_type d ty in
  filter_inst d t inst



let rec filter_polyattr (p, d3: polytype * d3) (attr:string) : (polytype * d3) * (polytype * d3) =
  let cp x y = (x, y), (x, y) in
  match p with
  | Bot | Top -> cp p d3
  | Module (M_user (m, defs)) ->
     if List.exists (fun x -> x.vname = attr) defs then
       (p, d3), (Bot, d3)
     else
       (Bot, d3), (p, d3)
  | Module (M_builtin _) -> Debug.fail "ni, need to check if attr in module?"
  | List t -> Debug.fail "ni"
  | Function (f, _) -> Debug.fail "ni"
  | Method _ -> Debug.fail "ni"
  | Class (C_user c, _) ->
     let attrs = c.py_cls_static_attributes in
     if List.exists (fun x -> x.vname = attr) attrs then
       (p, d3), (Bot, d3)
     else
       (Bot, d3), (p, d3)
  | Class _ -> Debug.fail "filter_polyattr on unsupported or builtin class"
  | Instance ({classn=Class (C_user c, _); uattrs; oattrs} as inst) ->
     let attrs = c.py_cls_static_attributes in
     if List.exists (fun x -> x.vname = attr) attrs then
       (p, d3), (Bot, d3)
     else if StringMap.exists (fun k v -> k = attr) uattrs then
       (p, d3), (Bot, d3)
     else if StringMap.exists (fun k v -> k = attr) oattrs then
       (* cp p d3 *)
       let attr_value = StringMap.find attr oattrs in
       (Instance {inst with uattrs=StringMap.add attr attr_value uattrs; oattrs=StringMap.remove attr oattrs}, d3),
       (p, d3)
     else
       (Bot, d3), (p, d3)
  | Instance ({classn=Class (C_builtin c, _); uattrs; oattrs} as inst) ->
     let cls = Addr.find_builtin c in
     if Addr.is_builtin_attribute cls attr then
       (p, d3), (Bot, d3)
     else if StringMap.exists (fun k v -> k = attr) uattrs then
       (p, d3), (Bot, d3)
     else if StringMap.exists (fun k v -> k = attr) oattrs then
       let attr_value = StringMap.find attr oattrs in
       (Instance {inst with uattrs=StringMap.add attr attr_value uattrs; oattrs=StringMap.remove attr oattrs}, d3),
       (p, d3)
     else
       (Bot, d3), (p, d3)
  | Instance _ -> Debug.fail "filter_polyattr on unsupported instance"
  | Typevar var ->
     let ms = concretize_poly p d3 in
     let t, f = Monotypeset.fold (fun el (acct, accf) ->
                    let (mt, _), (mf, _) = filter_polyattr (poly_cast el, d3) attr in
                    let mt = mono_cast mt and
                        mf = mono_cast mf in
                    let acct = if mt = Bot then acct else Monotypeset.add mt acct in
                    let accf = if mf = Bot then accf else Monotypeset.add mf accf in
                    (acct, accf)) ms (Monotypeset.empty, Monotypeset.empty) in
     (* one more optimization: if d3t.var contains just one element, we don't change d3, but change p *)
     let pt, d3t = match Monotypeset.cardinal t with
       | 0 -> Bot, d3
       | 1 -> poly_cast (Monotypeset.choose t), d3
       | _ -> p, TypeVarMap.add var t d3 in
     let pf, d3f = match Monotypeset.cardinal f with
       | 0 -> Bot, d3
       | 1 -> poly_cast (Monotypeset.choose f), d3
       | _ -> p, TypeVarMap.add var f d3 in
     (pt, d3t), (pf, d3f)

let filter_attr (d:domain) (t:typeid) (attr:string) : domain * domain =
  let ty = TypeIdMap.find t d.d2 in
  let (pt, d3t), (pf, d3f) = filter_polyattr (ty, d.d3) attr in
  debug "Filter_attr: pt=%a, pf=%a@\n" pp_polytype pt pp_polytype pf;
  let dt = match pt with
    | Bot -> bottom
    | _ -> let d2t = TypeIdMap.add t pt d.d2 in
           {d with d2=d2t; d3=d3t} in
  let df = match pf with
    | Bot -> bottom
    | _ -> let d2f = TypeIdMap.add t pf d.d2 in
           {d with d2=d2f; d3=d3f} in
  dt, df

let filter_ty_attr (d:domain) (ty:polytype) (attr:string) : domain * domain =
  let t, d = get_type d ty in
  filter_attr d t attr
