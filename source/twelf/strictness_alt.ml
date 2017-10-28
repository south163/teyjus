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

type idset = id list;;

type idpairset = (id * id) list;;



type aposanntype = Pos of (id * aneganntype) list * id * term list

and aneganntype = Neg of (id * aposanntype) list * id * term list * idset

type dependency = idpairset
                   
type delta = idset

type gamma = idset;;

(* Set operations, will be updated after using set instead of list *)
let union s s' = [];;

let intersect s s' = [];;

let diff s s' = [];;

let rec add_dep (x : id) (l : id list) : (id*id) list =
  match l with
  | [] -> ([] : (id * id) list)
  | v :: l' -> (x, v) :: (add_dep x l')


let none_tycon = Const "None";;


let rec find_strict_vars_pos tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_pos_rec tp g in
  	let s_final = finalize s dep in
  		(Pos (ann_pairs, c, tms), s_final)

and find_strict_vars_neg tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_neg_rec tp g in
  	let s_final = finalize s dep in
  		(Neg (ann_pairs, c, tms, (diff s_final g)), s_final)
  		
and find_strict_vars_pos_rec tp g =
  match tp with
    PiType (x, tpA, tpB) -> let (ann_tpA, s_A) = find_strict_vars_neg tpA g
                          in let (s, dep, ann_pairs, tc, tms) =  find_strict_vars_pos_rec tpB  (x::g)
                             in (s, union dep (add_dep x (intersect s_A g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), [], [], c, tms)
  | _ -> ([], [], [], none_tycon, [])

and find_strict_vars_neg_rec tp g =
  match tp with
  | PiType (x, tpA, tpB) -> let (ann_tpA, s_A) = find_strict_vars_pos tpA g
                          in let (s, dep, ann_pairs, tc, tms) =  (find_strict_vars_neg_rec tpB (x::g)) in (s, union dep (add_dep x (intersect s_A g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | AppType (c, tms) -> ((union_fsvo_terms tms g), [], [], c, tms)
  | _ -> ([], [], [], none_tycon, [])

and  union_fsvo_terms tms g =
  match tms with
  | [] -> []
  | tm :: tms' -> union (find_strict_vars_object tm g []) (union_fsvo_terms tms' g)

and find_strict_vars_object tm g d = []

and finalize s dep = s;;


    

      
