(** Implements the strictness check for translating LF types. *)

open Set
open List
open Hashtbl
   
type kind =
  PiKind of (id * typ * kind)
| ImpKind of (typ * kind)
| Type 

and typ =
  PiType of (id * typ * typ)
| AppType of (id * term list)
| ImpType of (typ * typ)
| IdType of (id)

and term =
  AbsTerm of (id * typ * term)
| AppTerm of (id * term list)
| IdTerm of (id)

and id =
| Const of (string)
| Var of (string * typ) 
| LogicVar of (string * typ)

let get_id_name id =
  match id with
      Const(n) -> n
    | Var(n,_) -> n
    | LogicVar(n,_) -> n

let string_of_id id =
  match id with
      Const(n)
    | Var(n,_) 
    | LogicVar(n,_) -> n
                     


let compare_id i i' = Pervasives.compare (string_of_id i) (string_of_id i')
                    
module OrderedId = struct
  type t = id
  let compare = compare_id
end


module IdSet = Set.Make(OrderedId)
type idset = IdSet.t
(*convert a set to a list *)
let tolist = fun s -> IdSet.fold (fun x l -> x::l) s [];; 
           
type idMap = (id, idset) Hashtbl.t

(*convert a map to a list of pairs*) 
let topairlist = fun h -> Hashtbl.fold (fun k v acc -> (k, (tolist v)) :: acc) h [];;

               
type aposanntype = Pos of (id * aneganntype) list * id * term list
and aneganntype = Neg of (id * aposanntype) list * id * term list * idset

type dependency = idMap  
type delta = idset (* bounded variables in a type *)
type gamma = idset (* context *)

let none_tycon = Const "None";;


let rec find_strict_vars_pos tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_pos_rec tp g in
  	let s_final = finalize s dep in
  		(Pos (ann_pairs, c, tms), s_final)

and find_strict_vars_neg tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_neg_rec tp g in
  	let s_final = finalize s dep in
  		(Neg (ann_pairs, c, tms, (IdSet.diff s_final g)), s_final)
  		
and find_strict_vars_pos_rec tp g =
  match tp with
    PiType (x, tpA, tpB) -> let (ann_tpA, s_A) = find_strict_vars_neg tpA g
                          in let (s, dep, ann_pairs, tc, tms) =  find_strict_vars_pos_rec tpB  (IdSet.add x g)
                             in (s, (add_dep dep x (IdSet.union s_A g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms)
  | _ -> (IdSet.empty, Hashtbl.create 16, [], none_tycon, [])

and find_strict_vars_neg_rec tp g =
  match tp with
  | PiType (x, tpA, tpB) -> let (ann_tpA, s_A) = find_strict_vars_pos tpA g
                            in let (s, dep, ann_pairs, tc, tms) =  find_strict_vars_neg_rec tpB  (IdSet.add x g)
                               in (s,  (add_dep dep x (IdSet.union s_A g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms)
  | _ -> (IdSet.empty, Hashtbl.create 16, [], none_tycon, [])

and union_fsvo_terms tms g =
  match tms with
  | [] -> IdSet.empty
  | tm :: tms' -> IdSet.union (find_strict_vars_object tm g IdSet.empty) (union_fsvo_terms tms' g)

                
(* add x to (dep[v] : list) for each v in l *)
and add_dep (dep : dependency) (x : id) (l : idset) =
  IdSet.fold (fun v (tbl : dependency) ->
      match  Hashtbl.find_opt tbl v with
      | None -> Hashtbl.add tbl v (IdSet.singleton x); tbl
      | Some set -> Hashtbl.replace tbl v (IdSet.add x set);
                    tbl)  l dep

(*returns the set of strict variables in a term*)
and find_strict_vars_object tm g d =
  match tm with
  | AppTerm (v, tms) -> if (all_ids_are_strict tms d IdSet.empty)
                        then IdSet.singleton v (*Init0*)
                        else if not (IdSet.mem v g)
                        then union_sv_subterms tms g d (*App0*)
                        else IdSet.empty
  | AbsTerm (v, tp, tm') ->  find_strict_vars_object tm' g (IdSet.add v d) (*ABS0*)
  | _ -> IdSet.empty

(*Check if all terms in tms are ids and if they are all bounded (in delta)*)
and all_ids_are_strict tms d checked : bool =
  match tms with
  | [] -> true
  | (IdTerm v)::tms' -> IdSet.mem v d || not (IdSet.mem v checked) || all_ids_are_strict tms' d (IdSet.add v checked)
  | _ -> false

(* the union of all strict variables found in tms *)
and union_sv_subterms tms g d =
  List.fold_left (fun vars tm -> IdSet.union (find_strict_vars_object tm g d) vars) IdSet.empty tms

(*finalize the set of strict variables with dependency information*)
and finalize s dep = IdSet.empty


    
(*test cases*)
let id1 = Const "int";;
let id2 = Const "double";;
let tp1 = IdType (Const "int");;
let tp2 = IdType (Const "double");;
let w = Var ("w", tp2);;
let x = Var ("x", tp1);;
let y = Var ("y", tp2);;
let z = Var ("z", tp2);;
let dp = Hashtbl.create 16;;
let u = IdSet.empty;;
let u = IdSet.add w u;;
let u = IdSet.add x u;;
let u = IdSet.add x u;;

add_dep dp y u;;
topairlist dp;;

let u = IdSet.add y u;;
add_dep dp x u;;                     
topairlist dp;;
