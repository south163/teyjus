(** Implements the strictness check for translating LF types. *)
open Set;;


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



type aposanntype = Pos of (id * aneganntype) list * typ * term list

and aneganntype = Neg of (id * aposanntype) list * typ * term list * idset

and dependency = Dep of idpairset

and found = Found of idset
                   
and delta = D of idset

and gamma = G of idset;;

let union s s' = nil;;

let intersect s s' = nil;;

let diff s s' = nil;;

let add_dep x l =
  match l with
  | nil -> nil
  | v :: l' -> (x, v) :: (add_dep x l')


let none_tycon = Const "None";;                         

let find_strict_vars_pos ty g =
  let (s, dep, ann_pairs, c, M) = find_strict_vars_pos_rec ty g nil in
  	let s_final = finalize s dep in
  		(pos ann_pairs c M), s_final

and find_strict_vars_neg tp g =
  let (s, dep, ann_pairs, c, M) = find_strict_vars_neg_rec ty g nil in
  	let s_final = finalize s dep in
  		(neg ann_pairs c M (diff s_final g)), s_final
  		
and find_strict_vars_pos_rec tp g =
  match tp with
  | PiType v tp_A tp_B -> let (ann_tp_A, S_A) = find_strict_vars_neg tp_A g
                          in let (S, dep, ann_pairs, tc, terms) =  find_strict_vars_pos_rec tp_B  v::g
                             in (S, union dep (add_dep x (intersect S_A g)), (x, ann_tp_A)::ann_pairs, tc, terms)
  | AppType c terms -> ((union_fsvo_terms terms g), nil, nil, c, terms)
  | _ -> (nil, nil, nil, none_tycon, nil)

and find_strict_vars_neg_rec tp g =
  match tp with
  | PiType v tp_A tp_B -> let (ann_tp_A, S_A) = find_strict_vars_pos tp_A g
                          in let (S, dep, ann_pairs, tc, terms) =  find_strict_vars_neg_rec tp_B  v::g
                             in (S, union dep (add_dep x (intersect S_A g)), (x, ann_tp_A)::ann_pairs, tc, terms)
  | AppType c terms -> ((union_fsvo_terms terms g), nil, nil, c, terms)
  | _ -> (nil, nil, nil, none_tycon, nil)

and  union_fsvo_terms terms g =
  match terms with
  | nil -> nil
  | term :: terms' -> union (find_strict_vars_object term g nil) (union_fsvo_terms terms' g)

  	

    

      
