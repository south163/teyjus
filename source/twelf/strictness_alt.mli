(** Interface for Strictness Check **)
val compare_symb : Symb.symbol -> Symb.symbol -> int

module OrderedSymb : sig
  type t = Symb.symbol
  val compare : Symb.symbol -> Symb.symbol -> int
  end

module SymbSet : (Set.S with type elt = OrderedSymb.t)

type symbset = SymbSet.t

type aposanntype = Pos of (Symb.symbol * aneganntype) list * Lfabsyn.id * Lfabsyn.term list
and aneganntype = Neg of (Symb.symbol * aposanntype) list * Lfabsyn.id * Lfabsyn.term list * symbset

val find_strict_vars_pos : Lfabsyn.typ -> symbset -> (aposanntype * symbset)
val find_strict_vars_neg : Lfabsyn.typ -> symbset -> (aneganntype * symbset)

val printset : symbset -> unit
