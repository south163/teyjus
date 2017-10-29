(** Implements the strictness check for translating LF types. *)


   
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
                     

(* ------------------------------------------------------------------- *)
open Set
open List
open Hashtbl

let compare_id i i' = Pervasives.compare (string_of_id i) (string_of_id i')
                    
module OrderedId = struct
  type t = id
  let compare = compare_id
end
module IdSet = Set.Make(OrderedId)
type idset = IdSet.t

       
type idMap = (id, idset) Hashtbl.t

let setcopy set = IdSet.fold (fun x s -> IdSet.add x s) set IdSet.empty;;
let mapcopy map = Hashtbl.fold (fun k v m -> Hashtbl.add m k (setcopy v); m) map (Hashtbl.create 16);; 

  
(*convert a set to a list *)
let tolist = fun s -> IdSet.fold (fun x l -> x::l) s [];; 
(*convert a map to a list of pairs*) 
let topairlist = fun h -> Hashtbl.fold (fun k v acc -> (k, (tolist v)) :: acc) h [];;



type dependency = idMap (* id -> P(id), if x -> {y}, then if x is strict, then y is strict*)
type delta = idset (* bounded variables in a type *)
type gamma = idset (* context *)
type aposanntype = Pos of (id * aneganntype) list * id * term list
and aneganntype = Neg of (id * aposanntype) list * id * term list * idset

           

(* annotate tp given a context gamma*)
let rec find_strict_vars_pos tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_pos_rec tp g in
  	let s_final = finalize s dep in
  		(Pos (ann_pairs, c, tms), s_final)

and find_strict_vars_neg tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_neg_rec tp g in
  	let s_final = finalize s dep in
  		(Neg (ann_pairs, c, tms, (IdSet.diff s_final g)), s_final)

              
(* find (incomplete) strict variables, dependency information, and elements to construct pos_ann_type 
        given positive type and context g *)
and find_strict_vars_pos_rec tp g =
  match tp with
    PiType (x, tpA, tpB) -> let (ann_tpA, sA) = find_strict_vars_neg tpA (setcopy g)
                          in let (s, dep, ann_pairs, tc, tms) =  find_strict_vars_pos_rec tpB  (IdSet.add x g)
                             in (s, (add_dep dep x (IdSet.union sA g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms)
  | ImpType (tpA, tpB) -> find_strict_vars_pos_rec tpB g                       
  | IdType t -> (IdSet.empty, Hashtbl.create 16, [], t, [])

(* find (incomplete) strict variables, dependency information, and elements to construct neg_ann_type 
        given negative type and context g *)
and find_strict_vars_neg_rec tp g =
  match tp with
  | PiType (x, tpA, tpB) -> let (ann_tpA, sA) = find_strict_vars_pos tpA (setcopy g)
                            in let (s, dep, ann_pairs, tc, tms) =  find_strict_vars_neg_rec tpB  (IdSet.add x (setcopy g))
                               in (s, (add_dep dep x (IdSet.union sA g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms)
  | ImpType (tpA, tpB) -> find_strict_vars_neg_rec tpB g                         
  | IdType t -> (IdSet.empty, Hashtbl.create 16, [], t, [])


(* find all the strict variables in a list of terms *)
and union_fsvo_terms tms g =
  match tms with
  | [] -> IdSet.empty
  | tm :: tms' -> IdSet.union (find_strict_vars_term tm (setcopy g) IdSet.empty) (union_fsvo_terms tms' g)

                
(* add x to (dep[v] : list) for each v in l *)
and add_dep (dep : dependency) (x : id) (l : idset) =
  IdSet.fold (fun v (tbl : dependency) ->
      match  Hashtbl.find_opt tbl v with
      | None -> Hashtbl.add tbl v (IdSet.singleton x); tbl
      | Some set -> Hashtbl.replace tbl v (IdSet.add x set);
                    tbl)  l dep

(*returns the set of strict variables in a term*)
and find_strict_vars_term tm g d =
  match tm with
  | AppTerm (v, tms) -> if (all_ids_are_strict tms d IdSet.empty)
                        then IdSet.singleton v (*Init0*)
                        else if not (IdSet.mem v g)
                        then union_sv_subterms tms g d (*App0*)
                        else IdSet.empty
  | AbsTerm (v, tp, tm') ->  find_strict_vars_term tm' g (IdSet.add v d) (*ABS0*)
  | _ -> IdSet.empty

(*Check if all terms in tms are ids and if they are all bounded (in delta)*)
and all_ids_are_strict tms d checked : bool =
  match tms with
  | [] -> true
  | (IdTerm v)::tms' -> IdSet.mem v d && not (IdSet.mem v checked) && all_ids_are_strict tms' d (IdSet.add v checked)
  | _ -> false

(* the union of all strict variables found in tms *)
and union_sv_subterms tms g d =
  List.fold_left (fun vars tm -> IdSet.union (find_strict_vars_term tm g (setcopy d)) vars) IdSet.empty tms

(*finalize the set of strict variables with dependency information*)
and finalize s dep =
  let s' = IdSet.union (IdSet.fold (fun v set ->
                             match (Hashtbl.find_opt dep v) with
                             | None -> set
                             | Some s -> IdSet.union s set) s IdSet.empty) s
  in if s' = s then s'
     else finalize s' dep;;
       

(* test case *)
let idtp1 = Const "tp1";;
let idtp2 = Const "tp2";;
let idtp3 = Const "tp3";;

let tp1 = IdType idtp1;;
let tp2 = IdType idtp2;;
let tp3 = IdType idtp3;;

let v1 = Var ("v1", tp1);;
let v2 = Var ("v2", tp2);;
let v3 = Var ("v3", tp2);;

let tm1 = AbsTerm (v3 , tp2 , AppTerm (v1, [IdTerm v3]));;
let tm2 = IdTerm v2;;

let tycon = Const "tycon";;

let tmlist = [tm1; tm2];;

let testtp = PiType (v1, tp1, (PiType (v2, tp2, AppType (tycon, tmlist))));;

(* [v1].[v2]. c ({v3}.v1 v3)(v2) *)
let test = find_strict_vars_neg testtp IdSet.empty;;

let print_neg_ann_type tp =
  match tp with
    Neg (a, b, c, s) -> (a, b, c, tolist s);;

print_neg_ann_type (fst test);;
