
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
            
module IdSet = Set.Make(OrderedId)

type idset = IdSet.t

module IdPairSet = Set.Make(OrderedIdPair)     
             
type idpairset = IdPairSet.t

type dependency = idpairset

type delta = idset

 (* context *)
type gamma = idset

type pos_ann_type = Pos of (id * aneganntype) list * id * term list
and neg_ann_type = Neg of (id * aposanntype) list * id * term list * idset



val find_strict_vars_pos : typ -> gamma -> aposanntype * idset
val find_strict_vars_neg : typ -> gamma -> aneganntype * idset
