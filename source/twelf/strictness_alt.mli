
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

type tycon = id;;
            
type idset = id list;;

type idpairset = (id * id) list;;

type pos_ann_type = Pos of (id * aneganntype) list * id * term list

and neg_ann_type = Neg of (id * aposanntype) list * id * term list * id list

type dependency = idpairset

type delta = idset

 (* context *)
type gamma = idset;;


val find_strict_vars_pos : typ -> gamma -> pos_ann_type * id list

val find_strict_vars_neg : typ -> gamma -> neg_ann_type *  id list

val find_strict_vars_pos_rec : typ -> gamma -> (id list * dependency * (id * neg_ann_type) list * tycon * term list)

val find_strict_vars_neg_rec : typ -> gamma -> (id list * dependency * (id * pos_ann_type) list * tycon * term list)

val find_strict_vars_object : term -> gamma -> delta -> id list

val finalize : id list -> dependency -> id list
