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

(** apply_subst : (Syb.symbol * Lfabsyn.term) -> Lfabsyn.typ -> Lfabsyn.typ
    Because of dependencies may need to apply substitution to the LF types.**)
let apply_subst subst typ = typ

let get_type signature metadata fvars bvars lpterm = Lfabsyn.Unknown
  
(** invert_subst : Lfsig.signature -> Metadata.metadata -> Lfabsyn.typ Table.SymbolTable.t ->
                     (Symb.symbol * Lfabsyn.typ) list ->  (Symb.symbol * Lfabsyn.term) list
    Invert an answer substitution. Substitution terms will have the form:
      X |-> lam x1 ... lam xn h u1 ... um
    where u1 ... um are distinct bound vraiables and h is a constant, one of the bound
    variables or a new logic variable. So from the known LF type of X we are able to 
    type the substitution term. **)
let invert_subst (Lfsig.Signature(types, objs)) metadata fvars subst =
  (** invert_term: (Symb.symbol * Lfabsyn.typ) list -> Lfabsyn.typ -> Absyn.aterm -> 
                   Lfabsyn.term option
      Invert a given term at the given type under the given context of bound vraiables. **)
  let rec invert_term bvars (lftype, tysub) lpterm =
    match lpterm with
      Absyn.ConstantTerm(c,_,p) ->
        (match Metadata.getLF metadata (Absyn.getConstantSymbol c) with
          Some(symb) ->
            (match Symboltable.lookup objs symb with
              Some(c') ->
                Some(Lfabsyn.IdTerm(Lfabsyn.Const(Lfabsyn.get_obj_symb c')))
            | None ->
                Errormsg.error Errormsg.none ("No entry found in LF signature for constant "^(Symb.name symb));
                None)
        | None ->
            Errormsg.error Errormsg.none ("No mapping found for LP constant "^(Absyn.getConstantName c));
            None)
    | Absyn.BoundVarTerm(Absyn.DBIndex(i),p) ->
        let (s, ty) = List.nth bvars (i-1) in
        Some(Lfabsyn.IdTerm(Lfabsyn.Var(s, ty)))
    | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb), p) ->
        Some(Lfabsyn.IdTerm(Lfabsyn.LogicVar(Symb.symbol (Absyn.getTypeSymbolName tysymb), lftype)))
    | Absyn.ApplicationTerm(_, _) ->
        let (lphead, lpargs) = Absyn.getTermApplicationHeadAndArguments lpterm in
        (* If a new free variable we know it will be applied to distinct bound vars.
           So will not need to find a type for it. *)
        if (Absyn.isTermFreeVariable lphead &&
            Option.isNone(Table.find (Absyn.getTypeSymbolSymbol (Absyn.getTermFreeVariableTypeSymbol lphead)) fvars))
        then
          let s = Symb.symbol (Absyn.getTypeSymbolName (Absyn.getTermFreeVariableTypeSymbol lphead)) in
          let lfargs = List.map (fun arg -> Option.get (invert_term bvars (Lfabsyn.Unknown, tysub) arg)) lpargs in
          Some(Lfabsyn.AppTerm(Lfabsyn.LogicVar(s, Lfabsyn.Unknown), lfargs))
        (* If anything else, we have a type to invert arguments with. *)
        else
          let rec trans_arglst pity subst args =
            (match args with
              (arg :: args') ->
                (match pity with
                  Lfabsyn.PiType(s,bty,tybody,dep) ->
                    let Some(lfarg) = invert_term bvars (bty, subst) arg in
                    (lfarg :: trans_arglst tybody ((s, lfarg) :: subst) args')
                | _ ->
                  Errormsg.error Errormsg.none ("Error: invert_term: Type of head does not match number of arguments.");
                   [])
            | [] -> [] )
          in
          let lfheadty = get_type (Lfsig.Signature(types, objs)) metadata fvars bvars lphead in
          let Some(Lfabsyn.IdTerm(lfhead)) = invert_term bvars (lfheadty, tysub) lphead in
          let lfargs = trans_arglst lfheadty [] lpargs in
          Some(Lfabsyn.AppTerm(lfhead, lfargs))
    | Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,body), p) ->
        let bvar_symb = Symb.symbol (generate_name metadata fvars) in
        (match apply_subst tysub lftype with
           Lfabsyn.PiType(s, ty, tybody, dep) ->
             (match invert_term ((s, ty) :: bvars) (tybody, []) body with
                Some(lfbody) ->
                  Some(Lfabsyn.AbsTerm(s, ty, lfbody)) 
              | None ->
                  (* if problem translating body push error up to whole term *)
                  None)
         | _ ->
             Errormsg.error Errormsg.none ("Error: invert_term: Inverting abstraction with non-function type.");
             None)
  in
  (* Auxilory function to build the LF answer substitution from 
     the inverting the LP substitution. *)
  let rec aux lpsubst lfsubst =
    match lpsubst with
      ((tysymb, tm) :: lpsubst') ->
        let ty =
          (match Table.find (Absyn.getTypeSymbolSymbol tysymb) fvars with
             Some(t) -> apply_subst lfsubst t
           | None -> (** this should never happen because of constraints **)
               Errormsg.error Errormsg.none ("No entry in free vars found for "^(Absyn.getTypeSymbolName tysymb));
               Lfabsyn.Unknown)
        in
        (match invert_term [] (ty,[]) tm with
          Some(lftm) ->
            aux lpsubst' ((Symb.symbol (Absyn.getTypeSymbolName tysymb), lftm) :: lfsubst)
        | None ->
            (** If there was an error try to continue **)
            aux lpsubst' lfsubst
        )
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
        (match Metadata.getLF metadata (Absyn.getConstantSymbol c) with
          Some(symb) ->
            (match Symboltable.lookup objs symb with
              Some(c') ->
                Some(Lfabsyn.IdTerm(Lfabsyn.Const(Lfabsyn.get_obj_symb c')))
            | None ->
                Errormsg.error Errormsg.none ("No entry found in LF signature for constant "^(Symb.name symb));
                None)
        | None ->
            Errormsg.error Errormsg.none ("No mapping found for LP constant "^(Absyn.getConstantName c));
            None)
    | Absyn.BoundVarTerm(Absyn.DBIndex(i),_) ->
        (if List.length bvars < i
         then
           (* if not bound in term use DB index to represent the variable *) 
              Some(Lfabsyn.IdTerm(Lfabsyn.Var(Symb.symbol ("#" ^ (string_of_int i)), Lfabsyn.Unknown)))
         else
           let (s, ty) = List.nth bvars (i-1) in
           Some(Lfabsyn.IdTerm(Lfabsyn.Var(s, ty))) )
    | Absyn.FreeVarTerm(Absyn.NamedFreeVar(tysymb),_) ->
        Some(Lfabsyn.IdTerm(Lfabsyn.LogicVar(Symb.symbol (Absyn.getTypeSymbolName tysymb), Lfabsyn.Unknown)))
    | Absyn.ApplicationTerm(_,_) ->
        let (lphead, lpargs) = Absyn.getTermApplicationHeadAndArguments lptm in
        let Some(Lfabsyn.IdTerm(lfhead)) = invert_term bvars lphead in
        let lfargs = List.map (fun x -> Option.get (invert_term bvars x)) lpargs in
        Some(Lfabsyn.AppTerm(lfhead, lfargs))
    | Absyn.AbstractionTerm(Absyn.NestedAbstraction(_,body),_) ->
        let bvar_symb = Symb.symbol (generate_name metadata fvars) in
        let Some(lfbody) = invert_term ((bvar_symb, Lfabsyn.Unknown) :: bvars) body in
        Some(Lfabsyn.AbsTerm(bvar_symb,
                             Lfabsyn.Unknown,
                             lfbody))
  in
  let rec aux lpdisprs lfdisprs =
    match lpdisprs with
      ((lptm1, lptm2) :: lpdisprs') ->
        aux lpdisprs' ((Option.get (invert_term [] lptm1), Option.get (invert_term [] lptm2)) :: lfdisprs)
    | [] -> lfdisprs
  in
  aux disprs []
  
let invert lfsig metadata fvars (subst, disprs) =
  (invert_subst lfsig metadata fvars subst, invert_disprs lfsig metadata fvars disprs)
