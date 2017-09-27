(* use ocaml formatting to print LF terms more nicely *)

val pr_id : Format.formatter -> Lfabsyn.id -> unit
val pr_term : Format.formatter -> Lfabsyn.term -> unit
val pr_typ : Format.formatter -> Lfabsyn.typ -> unit
val pr_kind : Format.formatter -> Lfabsyn.kind -> unit
val pr_typefam : Format.formatter -> Lfabsyn.typefam -> unit
val pr_obj : Format.formatter -> Lfabsyn.obj -> unit
val pr_query : Format.formatter -> Lfabsyn.query -> unit
val pr_query_explicit : Format.formatter -> Lfabsyn.query -> unit
val pr_solution : Format.formatter -> Lfabsyn.solution -> unit

  
val print_id : Lfabsyn.id -> unit
val print_term : Lfabsyn.term -> unit
val print_typ : Lfabsyn.typ -> unit

val string_of_id : Lfabsyn.id -> string
val string_of_term : Lfabsyn.term -> string
val string_of_typ : Lfabsyn.typ -> string

val pr_iterm : Lfabsyn.typefam Symboltable.table -> (Symb.symbol * int) Symboltable.table -> Format.formatter -> Lfabsyn.term -> unit
val pt_ityp : Lfabsyn.typefam Symboltable.table -> (Symb.symbol * int) Symboltable.table -> Format.formatter -> Lfabsyn.typ -> unit
