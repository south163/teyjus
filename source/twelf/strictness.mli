(** Implements the strictness check for translating LF types. *)

(** Checks if the given variable appears strictly in the given type. *)
val appears_strict : Symb.symbol -> Lfabsyn.typ -> bool
