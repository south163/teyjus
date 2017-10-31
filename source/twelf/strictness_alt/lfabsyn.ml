(** Descibes the abstract syntax representation for LF. *)

let maxPrec = 9999
let minPrec = 0

(** A type-level declaration. 
    (constant name, kind, fixity, associativity, precedence,
    associated object constants, # implicit elements) *)
type typefam = TypeFam of (id * kind * fixity * assoc * int * (obj ref) list ref * int)

(** An object-level declaration.
    (constant name, type, fixity, associativity, precedence, # implicit arguments) *)
and obj = Object of (id * typ * fixity * assoc * int * int)

and query = Query of (id * typ) list * id * typ

and solution = (id * term) list * (term * term) list

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
  PiKind of (id * typ * kind)
| ImpKind of (typ * kind)
| Type 

and typ =
  PiType of (id * typ * typ)
| AppType of (id * term list)
| ImpType of (typ * typ)
| IdType of (id)

and term =
  AbsTerm of (id * typ * term)
| AppTerm of (id * term list)
| IdTerm of (id)

and id =
| Const of (string)
| Var of (string * typ) 
| LogicVar of (string * typ)


let rec string_of_kind k =
  match k with
      PiKind(id,ty,body) -> 
        "({ " ^ (string_of_id id) ^ " : " ^ (string_of_typ ty) ^ "} " ^ (string_of_kind body) ^ ")"
    | ImpKind(ty,body) -> 
        "(" ^ (string_of_typ ty) ^ " -> " ^ (string_of_kind body) ^ ")"
    | Type -> "type"

and string_of_typ ty =
  match ty with
      PiType(id,t1,t2) -> 
        "({ " ^ (string_of_id id) ^ " : " ^ (string_of_typ t1) ^ "} " ^ (string_of_typ t2) ^ ")"
    | AppType(t,tms) ->
        let tmlist =
          List.fold_left (fun s tm -> s ^ " " ^ (string_of_term tm)) "" tms
        in
        "(" ^ (string_of_id t) ^ " " ^ tmlist ^ ")"
    | ImpType(t1,t2) ->
        "(" ^ (string_of_typ t1) ^ " -> " ^ (string_of_typ t2) ^ ")"
    | IdType(id) ->
        string_of_id id

and string_of_term tm =
  match tm with
      AbsTerm(id,ty,body) -> 
        "([" ^ (string_of_id id) ^ " : " ^ (string_of_typ ty) ^"] " ^ (string_of_term body) ^ ")"
    | AppTerm(head, tms) -> 
        let tmlist = 
          List.fold_left (fun s t -> s ^ " " ^ (string_of_term t)) "" tms
        in
        "(" ^ (string_of_id head) ^ " " ^ tmlist ^ ")"
    | IdTerm(id) -> string_of_id id

and string_of_id id =
  match id with
      Const(n)
    | Var(n,_) 
    | LogicVar(n,_) -> n

and get_id_name id =
  match id with
      Const(n) -> n
    | Var(n,_) -> n
    | LogicVar(n,_) -> n

