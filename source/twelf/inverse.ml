(** a solution consists of a substitution and a disagreement set. *)
type lpsolution = (Absyn.atypesymbol * Absyn.aterm) list * (Absyn.aterm * Absyn.aterm) list


(** generate a new name for a bound variable.
    checks for clashes with 
      - constants in LF sig
      - logic variables in query
**)
let bvar_namegen_count = ref 0
 (* free vars must start with uppercase letter in twelf,
    so we actually don't need to check against the fvars *)
let bvar_namegen_base = ref "x"
let generate_name metadata fvars =
  let isName name = Metadata.isName metadata name
    (*if (Metadata.isName metadata name)
    then true
    else Option.isSome (Table.find (Symbol.symbol name) fvars)*)
  in
  let rec generate_aux () =
    let name = bvar_namegen_count := (!bvar_namegen_count) + 1;
               (!bvar_namegen_base) ^ (string_of_int (!bvar_namegen_count)) in
    if isName name
    then generate_aux ()
    else name
  in
  generate_aux ()
let reset_namegen_count () = bvar_namegen_count := 0


  
(** apply_subst : (Syb.symbol * Lfabsyn.term) -> Lfabsyn.typ -> (Symb.symbol * Lfabsyn.typ) list ->
                  Lfabsyn.typ 
    Apply an instantiation to an LF type. Keep track of bound varaibles to avoid capture. **)
let rec apply_subst subst typ bvars =
  let rec app_ty ty bvars =
    match ty with
      Lfabsyn.PiType(s,bty,tybody,dep) ->
        Lfabsyn.PiType(s,app_ty bty bvars, app_ty tybody ((s,bty):: bvars),dep)
    | Lfabsyn.AppType(id,args) ->
        let args' = List.map (fun arg -> apply_subst_tm subst arg bvars) args in
        Lfabsyn.AppType(id, args')
    | Lfabsyn.IdType(_) -> ty 
  in
  if List.length subst = 0 then typ else
  app_ty typ bvars
    
and apply_subst_tm subst tm bvars =
  let rec reduce head args sub =
    match (head, args) with
      (_, []) -> head
    | (Lfabsyn.IdTerm(id), _) ->
        apply_subst_tm sub (Lfabsyn.AppTerm(id, args)) []
    | (Lfabsyn.AppTerm(id,args'),_) ->
        apply_subst_tm sub (Lfabsyn.AppTerm(id, List.append args' args)) []
    | (Lfabsyn.AbsTerm(s,bty,body), (a::args')) ->
        reduce body args' ((s,a) :: sub)
  and app_tm tm bvars =
    match tm with
      Lfabsyn.IdTerm(id) ->
        app_id id bvars
    | Lfabsyn.AbsTerm(s,bty,body) ->
        Lfabsyn.AbsTerm(s,apply_subst subst bty bvars, app_tm body ((s,bty)::bvars) )
    | Lfabsyn.AppTerm(id,args) ->
        let h = app_id id bvars in
        let args' = List.map (fun a -> app_tm a bvars) args in
        reduce h args' []
  (*        Lfabsyn.AppTerm(app_id id bvars, List.map (fun a -> app_tm a bvars) args) *)
  and app_id id bvars =
    match id with
      Lfabsyn.Const(s) -> Lfabsyn.IdTerm(id)
    | Lfabsyn.Var(s,_) 
    | Lfabsyn.LogicVar(s,_) ->
        if (List.exists (fun (s',ty) -> Symb.id s = Symb.id s') bvars)
        then
          Lfabsyn.IdTerm(id)
        else
          try
            let (s',tm) = List.find (fun (s',tm) -> Symb.id s = Symb.id s') subst in
            app_tm tm []
          with Not_found -> Lfabsyn.IdTerm(id)
  in
  if List.length subst = 0 then tm else
  app_tm tm bvars

          
(** Lookup the corresponding LF type for the given const/var term. **)
let get_type (Lfsig.Signature(_,objs)) metadata fvars bvars lpterm =
  match lpterm with
    Absyn.ConstantTerm(c,_,_) ->
      let Some(lfsymb) = Metadata.getLF metadata (Absyn.getConstantSymbol c) in
      let Some(obj) = Symboltable.lookup objs lfsymb in
      Lfabsyn.get_obj_typ obj
  | Absyn.BoundVarTerm(Absyn.DBIndex(i),_) ->
      let (_,lfty) = List.nth bvars (i-1) in
      lfty
  | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb),_) ->
      let Some(lfty) = Table.find (Absyn.getTypeSymbolSymbol tysymb) fvars in
      lfty
        
(** invert_subst : Lfsig.signature -> Metadata.metadata -> Lfabsyn.typ Table.SymbolTable.t ->
                     (Symb.symbol * Lfabsyn.typ) list ->  (Symb.symbol * Lfabsyn.term) list
    Invert an answer substitution. Substitution terms will have the form:
      X |-> lam x1 ... lam xn h u1 ... um
    where u1 ... um are distinct bound vraiables and h is a constant, one of the bound
    variables or a new logic variable. So from the known LF type of X we are able to 
    type the substitution term. **)
let invert_subst (Lfsig.Signature(types, objs)) metadata fvars subst =
  (** invert_term: (Symb.symbol * Lfabsyn.typ) list -> Lfabsyn.typ -> Absyn.aterm -> 
                   Lfabsyn.term
      Invert a given term at the given type under the given context of bound vraiables. **)
  let rec invert_term bvars (lftype, tysub) lpterm =
    match lpterm with
      Absyn.ConstantTerm(c,_,p) ->
        let Some(c') = Symboltable.lookup objs (Option.get (Metadata.getLF metadata (Absyn.getConstantSymbol c))) in
        Lfabsyn.IdTerm(Lfabsyn.Const(Lfabsyn.get_obj_symb c'))
    | Absyn.BoundVarTerm(Absyn.DBIndex(i),p) ->
        let (s, ty) = List.nth bvars (i-1) in
        Lfabsyn.IdTerm(Lfabsyn.Var(s, ty))
    | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb), p) ->
        Lfabsyn.IdTerm(Lfabsyn.LogicVar(Symb.symbol (Absyn.getTypeSymbolName tysymb), lftype))
    | Absyn.ApplicationTerm(_, _) ->
        let (lphead, lpargs) = Absyn.getTermApplicationHeadAndArguments lpterm in
        (* If a new free variable we know it will be applied to distinct bound vars.
           So will not need to find a type for it. *)
        if (Absyn.isTermFreeVariable lphead &&
            Option.isNone(Table.find (Absyn.getTypeSymbolSymbol (Absyn.getTermFreeVariableTypeSymbol lphead)) fvars))
        then
          let s = Symb.symbol (Absyn.getTypeSymbolName (Absyn.getTermFreeVariableTypeSymbol lphead)) in
          let lfargs = List.map (invert_term bvars (Lfabsyn.Unknown, tysub)) lpargs in
          Lfabsyn.AppTerm(Lfabsyn.LogicVar(s, Lfabsyn.Unknown), lfargs)
        (* If anything else, we have a type to invert arguments with. *)
        else
          let rec trans_arglst typ subst args =
            (match (typ, args) with
              ((Lfabsyn.PiType(s,bty,tybody,dep)),(arg :: args')) ->
                 let lfarg = invert_term bvars (bty, subst) arg in
                 (lfarg :: trans_arglst tybody ((s, lfarg) :: subst) args')
            | (_,[]) -> [] )
          in
          let lfheadty = get_type (Lfsig.Signature(types, objs)) metadata fvars bvars lphead in
          let Lfabsyn.IdTerm(lfhead) = invert_term bvars (lfheadty, tysub) lphead in
          let lfargs = trans_arglst lfheadty [] lpargs in
          Lfabsyn.AppTerm(lfhead, lfargs)
    | Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,body), p) ->
        let bvar_symb = Symb.symbol (generate_name metadata fvars) in
        let Lfabsyn.PiType(s,ty,tybody,dep) = apply_subst tysub lftype bvars in
        let lfbody = invert_term ((s,ty) :: bvars) (tybody, []) body in
        Lfabsyn.AbsTerm(s,ty,lfbody)
  in
  (* Auxilory function to build the LF answer substitution from 
     the inverting the LP substitution. *)
  let rec aux lpsubst lfsubst =
    match lpsubst with
    | ((tysymb, _) :: lpsubst') when Absyn.getTypeSymbolName tysymb = "_" ->
       aux lpsubst' lfsubst
    | ((tysymb, tm) :: lpsubst') ->
        let Some(t) = Table.find (Absyn.getTypeSymbolSymbol tysymb) fvars in
        let ty = apply_subst lfsubst t [] in
        aux lpsubst'
            ((Symb.symbol (Absyn.getTypeSymbolName tysymb),
                invert_term [] (ty,[]) tm) :: lfsubst)
    | [] -> lfsubst
  in
  aux subst []

(** invert_disprs : Lfsig.signature -> Metadata.metadata -> Lfabsyn.typ Table.SymbolTable.t ->
                    (Lfabsyn.term * Lfabsyn.term) list 
    Invert a list of disagreement pairs. Because we cannot guarantee LF types for these
    they are inverted as untyped terms. **)
let invert_disprs (Lfsig.Signature(types, objs)) metadata fvars disprs =
  let rec invert_term bvars lptm =
    match lptm with
      Absyn.ConstantTerm(c,_,_) ->
        let Some(c') = Symboltable.lookup objs (Option.get (Metadata.getLF metadata (Absyn.getConstantSymbol c))) in
        Lfabsyn.IdTerm(Lfabsyn.Const(Lfabsyn.get_obj_symb c'))
    | Absyn.BoundVarTerm(Absyn.DBIndex(i),_) ->
        (if List.length bvars < i
         then
           (* if not bound in term use DB index to represent the variable *) 
              Lfabsyn.IdTerm(Lfabsyn.Var(Symb.symbol ("#" ^ (string_of_int i)), Lfabsyn.Unknown))
         else
           let (s, ty) = List.nth bvars (i-1) in
           Lfabsyn.IdTerm(Lfabsyn.Var(s, ty)) )
    | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb),_) ->
        Lfabsyn.IdTerm(Lfabsyn.LogicVar(Symb.symbol (Absyn.getTypeSymbolName tysymb), Lfabsyn.Unknown))
    | Absyn.ApplicationTerm(_,_) ->
        let (lphead, lpargs) = Absyn.getTermApplicationHeadAndArguments lptm in
        let Lfabsyn.IdTerm(lfhead) = invert_term bvars lphead in
        let lfargs = List.map (invert_term bvars) lpargs in
        Lfabsyn.AppTerm(lfhead, lfargs)
    | Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,body),_) ->
        let bvar_symb = Symb.symbol (generate_name metadata fvars) in
        let lfbody = invert_term ((bvar_symb, Lfabsyn.Unknown) :: bvars) body in
        Lfabsyn.AbsTerm(bvar_symb,Lfabsyn.Unknown,lfbody)
  in
  let rec aux lpdisprs lfdisprs =
    match lpdisprs with
      ((lptm1, lptm2) :: lpdisprs') ->
        aux lpdisprs' ((invert_term [] lptm1, invert_term [] lptm2) :: lfdisprs)
    | [] -> lfdisprs
  in
  aux disprs []
  
let invert lfsig metadata fvars (subst, disprs) =
  (invert_subst lfsig metadata fvars subst, invert_disprs lfsig metadata fvars disprs)
