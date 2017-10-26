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
  PiKind of (id * typ * kind * bool)
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

let get_id_name id =
  match id with
      Const(s) 
    | Var(s,_) 
    | LogicVar(s,_) -> Symb.name s

let rec string_of_typefam (TypeFam(s,k,_,_,_,_,implicit)) =
  (Symb.printName s) ^ " : " ^ (string_of_kind (skip_kind implicit k)) ^ "."

and string_of_obj (Object(s,ty,_,_,_,implicit)) =
  (Symb.printName s) ^ " : " ^ (string_of_typ (skip_typ implicit ty)) ^ "."

and skip_kind k knd =
  match (k, knd) with
      (0, _) -> knd
    | (n, PiKind(_,_,body,_)) -> skip_kind (k - 1) body
    | _ -> 
      Errormsg.error Errormsg.none ("Error: attempting to skip implicit argument(s) in non-function kind: "^(string_of_kind knd));
      knd

and skip_typ k ty =
  match k, ty with
      (0, _) -> ty
    | (n, PiType(_,_,t2,_)) -> skip_typ (n-1) t2
    | _ -> 
      Errormsg.error Errormsg.none ("Error: attempting to skip implicit argument(s) in non-function type: "^(string_of_typ ty));
      ty

and string_of_kind k =
  match k with
      PiKind(s,ty,body,dep) ->
        (if dep
         then 
           "({ " ^ (Symb.printName s) ^ " : " ^ (string_of_typ ty) ^ "} "
         else
           "(" ^ (string_of_typ ty) ^ " -> ")
        ^ (string_of_kind body) ^ ")"
    | Type(_) -> "type"

and string_of_typ ty =
  match ty with
      PiType(s,t1,t2,dep) ->
        (if dep
         then
            "({ " ^ (Symb.printName s) ^ " : " ^ (string_of_typ t1) ^ "} "
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
    | Unknown ->
        Errormsg.err Errormsg.none ("Error: Attempting to print an unknown type."); ""

and string_of_term tm =
  match tm with
      AbsTerm(s, Unknown, body) ->
        "([" ^ (Symb.printName s) ^ "] " ^ (string_of_term body) ^ ")"
    | AbsTerm(s,ty,body) -> 
        "([" ^ (Symb.printName s) ^ " : " ^ (string_of_typ ty) ^"] " ^ (string_of_term body) ^ ")"
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
    | LogicVar(s,_) -> Symb.printName s

let get_id_symb id =
  match id with
      Const(s)
    | Var(s,_)
    | LogicVar(s,_) -> s
      
let string_of_query (Query(_,id,ty)) =
  (string_of_id id) ^ " : " ^ (string_of_typ ty)

let string_of_query' (Query(fvars,id,ty)) =
  let bndrs = List.fold_left (fun s (name,ty) -> s^(string_of_id name)^" : "^(string_of_typ ty)^".") "" fvars in
  bndrs ^ (string_of_query(Query(fvars,id,ty)))


let rec skip k l =
  match k, l with
      0, _ -> l
    | n, (x :: l') -> skip (n-1) l'
    | _ ->
        Errormsg.error Errormsg.none "Attempting to skip in empty argument list.";
        l

let rec string_of_term_implicit types objs tm =
  let rec aux tm =
    match tm with
      AbsTerm(symb, Unknown, body) -> "([" ^ (Symb.printName symb) ^"] "^ (aux body) ^ ")"
    | AbsTerm(symb,ty,body) -> 
        "([" ^ (Symb.printName symb) ^ " : " ^ (string_of_typ_implicit types objs ty) ^"] " ^ (aux body) ^ ")"
    | AppTerm(head, tms) -> 
        let h_symb = get_id_symb head in
        let k =
          (match (Symboltable.lookup objs h_symb) with
             Some(Object(_,_,_,_,_,implicit)) -> implicit
           | None -> 0 ) (* head is not object constant *)
        in
        let tmlist = 
          List.fold_left (fun s t -> s ^ " " ^ (aux t)) "" (skip k tms)
        in
        "(" ^ (Symb.printName h_symb) ^ " " ^ tmlist ^ ")"
    | IdTerm(id) -> string_of_id id
  in
  aux tm
and string_of_typ_implicit types objs ty =
  let rec aux ty =
    match ty with
      PiType(symb,t1,t2,dep) ->
        (if dep
         then
            "({ " ^ (Symb.printName symb) ^ " : " ^ (aux t1) ^ "} "
         else
            "(" ^ (aux t1) ^ " -> ")
        ^ (aux t2) ^ ")"
    | AppType(t,tms) ->
        let h_symb = get_id_symb t in
        (match Symboltable.lookup types h_symb with
             Some(TypeFam(_,_,_,_,_,_,k)) ->
               let tmlist =
                 List.fold_left (fun s tm -> s ^ " " ^ (string_of_term_implicit types objs tm)) "" (skip k tms)
               in
               "(" ^ (string_of_id t) ^ " " ^ tmlist ^ ")"
           | None ->
               Errormsg.error Errormsg.none ("No entry in type table for application head " ^ h_name);
               string_of_typ ty )
    | IdType(id) ->
        string_of_id id
  in
  aux ty

let string_of_solution types objs (subst, disprs) =
  let string_of_subst subst =
    let rec string_of_subst_aux sub =
      match sub with
          ((id,tm) :: sub') when (get_id_name id) = "" -> string_of_subst_aux sub'
        | ((id,tm) :: sub') ->
            (string_of_id id) ^ " = " ^ (string_of_term_implicit types objmap tm) ^ "\n" ^ (string_of_subst_aux sub')
        | [] -> ""
    in
    if subst = []
    then 
      ""
    else
      "The answer substitution:\n" ^ (string_of_subst_aux subst)
  in
  let string_of_disprs disprs =
    let rec string_of_disprs_aux prs =
      match prs with
          ((t1,t2) :: prs') ->
            "<" ^ (string_of_term_implicit types objs t1) ^ ", " ^ 
              (string_of_term_implicit types objs t2) ^ ">\n" ^ (string_of_disprs_aux prs')
        | [] -> ""
    in
    if disprs = []
    then 
      ""
    else
      "The remaining disagreement pairs list:\n" ^ (string_of_disprs_aux disprs)
  in 
  (string_of_subst subst) ^ "\n" ^ (string_of_disprs disprs)

let get_typefam_implicit (TypeFam(_,_,_,_,_,_,p)) = p
let get_obj_implicit (Object(_,_,_,_,_,p)) = p


let get_typefam_name (TypeFam(name,_,_,_,_,_,_)) = string_of_id name
let get_obj_name (Object(name,_,_,_,_,_)) = string_of_id name


let get_typefam_kind (TypeFam(_,k,_,_,_,_,_)) = k
let get_obj_typ (Object(_,t,_,_,_,_)) = t

let rec get_typ_head t =
  match t with
      PiType(_,_,t',_) -> get_typ_head t'
    | AppType (h,_) -> h
    | IdType(h) -> h
