(* use ocaml formatting to print LF terms more nicely *)

val pr_id : Format.formatter -> Lfabsyn.id -> unit
val pr_term : Format.formatter -> Lfabsyn.term -> unit
val pr_typ : Format.formatter -> Lfabsyn.typ -> unit

val print_id : Lfabsyn.id -> unit
val print_term : Lfabsyn.term -> unit
val print_typ : Lfabsyn.typ -> unit

val string_of_id : Lfabsyn.id -> string
val string_of_term : Lfabsyn.term -> string
val string_of_typ : Lfabsyn.typ -> string
