(* use ocaml formatting to print LF terms more nicely *)


(* These general printing functions do not make use of fixity or 
   implicit information from the signature *)

val pr_id : Format.formatter -> Lfabsyn.id -> unit
val pr_term : Format.formatter -> Lfabsyn.term -> unit
val pr_typ : Format.formatter -> Lfabsyn.typ -> unit
val pr_kind : Format.formatter -> Lfabsyn.kind -> unit
val pr_typefam : Format.formatter -> Lfabsyn.typefam -> unit
val pr_obj : Format.formatter -> Lfabsyn.obj -> unit
val pr_query : Format.formatter -> Lfabsyn.query -> unit
val pr_query_explicit : Format.formatter -> Lfabsyn.query -> unit
val pr_solution : Format.formatter -> Lfabsyn.solution -> unit
val pr_posanntype : Format.formatter -> Strictness.aposanntype -> unit
val pr_neganntype : Format.formatter -> Strictness.aneganntype -> unit
                                                            
val print_id : Lfabsyn.id -> unit
val print_term : Lfabsyn.term -> unit
val print_typ : Lfabsyn.typ -> unit
val print_kind : Lfabsyn.kind -> unit
val print_typefam : Lfabsyn.typefam -> unit
val print_obj : Lfabsyn.obj -> unit
val print_query : Lfabsyn.query -> unit
val print_query_explicit : Lfabsyn.query -> unit
val print_solution : Lfabsyn.solution -> unit

val string_of_id : Lfabsyn.id -> string
val string_of_term : Lfabsyn.term -> string
val string_of_typ : Lfabsyn.typ -> string
val string_of_kind : Lfabsyn.kind -> string
val string_of_typefam : Lfabsyn.typefam -> string
val string_of_obj : Lfabsyn.obj -> string
val string_of_query : Lfabsyn.query -> string
val string_of_query_explicit : Lfabsyn.query -> string
val string_of_solution : Lfabsyn.solution -> string
val string_of_posanntype : Strictness.aposanntype -> string
val string_of_neganntype : Strictness.aneganntype -> string


  
(* General printing functions for implicit LF expressions *)
  
val pr_term_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.term -> unit
val pr_typ_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.typ -> unit
val pr_kind_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.kind -> unit
val pr_typefam_implicit :  Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.typefam -> unit
val pr_obj_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.obj -> unit
val pr_query_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.query -> unit
val pr_query_exp_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.query -> unit
val pr_solution_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Format.formatter -> Lfabsyn.solution -> unit


(* Printing to stdout for implicit LF expressions *)

val print_term_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.term -> unit
val print_typ_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.typ -> unit
val print_kind_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.kind -> unit
val print_typefam_implicit :  Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.typefam -> unit
val print_obj_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.obj -> unit
val print_query_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.query -> unit
val print_query_exp_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.query -> unit
val print_solution_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.solution -> unit


(* Printing to a string for implicit LF expressions *)
  
val string_of_term_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.term -> string
val string_of_typ_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.typ -> string
val string_of_kind_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.kind -> string
val string_of_typefam_implicit :  Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.typefam -> string
val string_of_obj_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.obj -> string
val string_of_query_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.query -> string
val string_of_query_exp_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.query -> string
val string_of_solution_implicit : Lfabsyn.typefam Symboltable.table -> Lfabsyn.obj Symboltable.table -> Lfabsyn.solution -> string
