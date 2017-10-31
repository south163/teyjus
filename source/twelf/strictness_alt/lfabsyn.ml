(** Descibes the abstract syntax representation for LF. *)

let maxPrec = 9999
let minPrec = 0

(** A type-level declaration. 
    (constant symbol, kind, fixity, associativity, precedence,
    associated object constants, # implicit elements) *)
type typefam = TypeFam of (Symb.symbol * kind * fixity * assoc * int * Symb.symbol list ref * int)

(** An object-level declaration.
    (constant symbol, type, fixity, associativity, precedence, # implicit arguments) *)
and obj = Object of (Symb.symbol * typ * fixity * assoc * int * int)

and query = Query of (Symb.symbol * typ) list * Symb.symbol * typ

and solution = (Symb.symbol * term) list * (term * term) list

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

let rec get_id_name id =
  match id with
      Const(s) 
    | Var(s,_) 
    | LogicVar(s,_) -> Symb.name s


and string_of_kind k =
  match k with
      PiKind(s,ty,body,dep) ->
        (if dep
         then 
           "({ " ^ (Symb.name s) ^ " : " ^ (string_of_typ ty) ^ "} "
         else
           "(" ^ (string_of_typ ty) ^ " -> ")
        ^ (string_of_kind body) ^ ")"
    | Type -> "type"

and string_of_typ ty =
  match ty with
      PiType(s,t1,t2,dep) ->
        (if dep
         then
            "({ " ^ (Symb.name s) ^ " : " ^ (string_of_typ t1) ^ "} "
         else
            "(" ^ (string_of_typ t1) ^ " -> ")
        ^ (string_of_typ t2) ^ ")"
    | AppType(t,tms) ->
        let tmlist =
          List.fold_left (fun s tm -> s ^ " " ^ (string_of_term tm)) "" tms
        in
        "(" ^ (string_of_id t) ^ " " ^ tmlist ^ ")"
    | IdType(id) ->
        string_of_id id
    | Unknown -> ""

and string_of_term tm =
  match tm with
      AbsTerm(s, Unknown, body) ->
        "([" ^ (Symb.name s) ^ "] " ^ (string_of_term body) ^ ")"
    | AbsTerm(s,ty,body) -> 
        "([" ^ (Symb.name s) ^ " : " ^ (string_of_typ ty) ^"] " ^ (string_of_term body) ^ ")"
    | AppTerm(head, tms) -> 
        let tmlist = 
          List.fold_left (fun s t -> s ^ " " ^ (string_of_term t)) "" tms
        in
        "(" ^ (string_of_id head) ^ " " ^ tmlist ^ ")"
    | IdTerm(id) -> string_of_id id

and string_of_id id =
  match id with
      Const(s)
    | Var(s,_) 
    | LogicVar(s,_) -> Symb.name s

let get_id_symb id =
  match id with
      Const(s)
    | Var(s,_)
    | LogicVar(s,_) -> s
      
let string_of_query (Query(_,s,ty)) =
  (Symb.name s) ^ " : " ^ (string_of_typ ty)

let string_of_query' (Query(fvars,s,ty)) =
  let bndrs = List.fold_left (fun s (symb,ty) -> s^(Symb.name symb)^" : "^(string_of_typ ty)^".") "" fvars in
  bndrs ^ (string_of_query(Query(fvars,s,ty)))



