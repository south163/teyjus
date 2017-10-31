(** Interface for Strictness Check **)

type symbset
type symbMap

type aposanntype = Pos of (Symb.symbol * aneganntype) list * Lfabsyn.id * Lfabsyn.term list
and aneganntype = Neg of (Symb.symbol * aposanntype) list * Lfabsyn.id * Lfabsyn.term list * symbset

val find_strict_vars_pos : Lfabsyn.typ -> symbset -> (aposanntype * symbset)
val find_strict_vars_neg : Lfabsyn.typ -> symbset -> (aneganntype * symbset)
