(** Descibes the abstract syntax representation for LF. *)

val maxPrec : int
val minPrec : int

(** A type-level declaration. 
    (constant symbol, kind, fixity, associativity, precedence,
    associated object constants, # implicit elements) *)
type typefam = TypeFam of (Symb.symbol * kind * fixity * assoc * int * Symb.symbol list ref * int)

(** An object-level declaration.
    (constant symbol, type, fixity, associativity, precedence, # implicit arguments) *)
and obj = Object of (Symb.symbol * typ * fixity * assoc * int * int)

and query = Query of (symb * typ) list * symb * typ

and solution = (symb * term) list * (term * term) list

and fixity =
  Infix
| Prefix
| Postfix
| NoFixity

and assoc =
  None
| Right
| Left

and kind =
  PiKind of (Symb.symbol * typ * kind * bool)
| Type 

and typ =
  PiType of (Symb.symbol * typ * typ * bool)
| AppType of (id * term list)
| IdType of (id)
| Unknown

and term =
  AbsTerm of (Symb.symbol * typ * term)
| AppTerm of (id * term list)
| IdTerm of (id)

and id =
| Const of (Symb.symbol)
| Var of (Symb.symbol * typ) 
| LogicVar of (Symb.symbol * typ)

val string_of_typefam : typefam -> string
val string_of_obj : obj -> string
val string_of_kind : kind -> string
val string_of_typ : typ -> string
val string_of_term : term -> string
val string_of_id : id -> string
val string_of_query : query -> string
val string_of_query' : query -> string
val string_of_solution : typefam Symboltable.table -> obj Symboltable.table-> solution -> string

val get_id_symb : id -> Symb.symbol
  
val get_typefam_implicit : typefam -> int
val get_obj_implicit : obj -> int

val get_typefam_name : typefam -> string
val get_obj_name : obj -> string
val get_id_name : id -> string

val get_typefam_kind : typefam -> kind
val get_obj_typ : obj -> typ

val get_typ_head : typ -> id
