(** Interface for Strictness Check **)
val compare_symb : Symb.symbol -> Symb.symbol -> int
module OrderedSymb : sig
  type t
  val compare : t -> t -> int
  end
                     
module SymbSet = Set.S

type symbset = SymbSet.t
type symbMap = (Symb.symbol, symbset) Hashtbl.t

(*Definition of types involved in the strictness functions*)
type dependency = symbMap (* id -> P(id), if x -> {y}, then if x is strict, then y is strict*)
type delta = symbset (* bounded variables in a term *)
type gamma = symbset (* context *)

type aposanntype = Pos of (Symb.symbol * aneganntype) list * Lfabsyn.id * Lfabsyn.term list
and aneganntype = Neg of (Symb.symbol * aposanntype) list * Lfabsyn.id * Lfabsyn.term list * symbset

val find_strict_vars_pos : Lfabsyn.typ -> symbset -> (aposanntype * symbset)
val find_strict_vars_neg : Lfabsyn.typ -> symbset -> (aneganntype * symbset)
