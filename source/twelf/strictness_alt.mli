(** Interface for Strictness Check **)
val compare_id : Symb.symbol -> Symb.symbol -> int
module OrderedId : sig
  type t
  val compare : t -> t -> int
  end
                     
module IdSet : Set.S

type idset = IdSet.t
type idMap = (Symb.symbol, idset) Hashtbl.t

(*Definition of types involved in the strictness functions*)
type dependency = idMap (* id -> P(id), if x -> {y}, then if x is strict, then y is strict*)
type delta = idset (* bounded variables in a term *)
type gamma = idset (* context *)

type aposanntype = Pos of (Symb.symbol * aneganntype) list * Lfabsyn.id * Lfabsyn.term list
and aneganntype = Neg of (Symb.symbol * aposanntype) list * Lfabsyn.id * Lfabsyn.term list * idset

val find_strict_vars_pos : Lfabsyn.typ -> idset -> (aposanntype * idset)
val find_strict_vars_neg : Lfabsyn.typ -> idset -> (aneganntype * idset)
