(** Describes a representation for fully explicit LF signatures. *)

(** An LF signature.
    (table of type-level declarations, 
     table of object-level declarations) *)
type signature = Signature of (Lfabsyn.typefam Symboltable.table * Lfabsyn.obj Symboltable.table)

val string_of_sig : signature -> string
val string_of_sig_implicit : signature -> string

val get_typetable : signature -> Lfabsyn.typefam Symboltable.table
val get_objtable : signature -> Lfabsyn.obj Symboltable.table
