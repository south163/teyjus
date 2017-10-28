(** Implements the strictness check for translating LF types. *)

open Set

   
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
                     


let compare_id i i' = Pervasives.compare (get_id_name i) (get_id_name i')
                   
let compare_id_pairs p p' =
  let v = compare_id (fst p) (fst p') in
  match v with
  | 0 -> compare_id (snd p) (snd p')
  | _ -> v 

                    
module OrderedId = struct
  type t = id
  let compare = compare_id
end

module OrderedIdPair = struct
  type t = (id * id)
  let compare = compare_id_pairs
end

module IdSet = Set.Make(OrderedId)

type idset = IdSet.t

module IdPairSet = Set.Make(OrderedIdPair)     
             
type idpairset = IdPairSet.t

type aposanntype = Pos of (id * aneganntype) list * id * term list

and aneganntype = Neg of (id * aposanntype) list * id * term list * idset

type dependency = idpairset             
type delta = idset
type gamma = idset;;

let random_func s n =
  print_string "Hello, ";
  print_string s;
  flush stdout;
  1+n
;;

let add_dep (dep : idpairset) (x : id) (l : idset) =
  dep;;


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
  | AppType (c, tms) -> ((union_fsvo_terms tms g), IdPairSet.empty, [], c, tms)
  | _ -> (IdSet.empty, IdPairSet.empty, [], none_tycon, [])

and find_strict_vars_neg_rec tp g =
  match tp with
  | PiType (x, tpA, tpB) -> let (ann_tpA, s_A) = find_strict_vars_pos tpA g
                          in let (s, dep, ann_pairs, tc, tms) =  find_strict_vars_neg_rec tpB  (IdSet.add x g) in (s,  (add_dep dep x (IdSet.union s_A g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), IdPairSet.empty, [], c, tms)
  | _ -> (IdSet.empty, IdPairSet.empty, [], none_tycon, [])

and union_fsvo_terms tms g =
  match tms with
  | [] -> IdSet.empty
  | tm :: tms' -> IdSet.union (find_strict_vars_object tm g IdSet.empty) (union_fsvo_terms tms' g)

and find_strict_vars_object tm g d = IdSet.empty

and finalize s dep = s;;


    

      
