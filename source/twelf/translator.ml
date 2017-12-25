(** Translators for translating LF specifications into LP programs. *)

module type Translator =
sig
  (** Translate the given LF signature into an LP signature. *)
  val translate : Lfsig.signature ->
                    (Metadata.metadata *
                      Absyn.akind Table.SymbolTable.t *
                      Absyn.aconstant Table.SymbolTable.t *
                      Absyn.aterm list)

  val translate_query : Lfabsyn.query -> Metadata.metadata ->
                          Absyn.akind Table.SymbolTable.t ->
                          Absyn.aconstant Table.SymbolTable.t -> (Absyn.aterm * Absyn.atypesymbol list)
end

let currentTranslation = ref "optimized"
let set_translation s =
  match s with
      "naive"
    | "optimized" ->
        currentTranslation := s
    | _ -> Errormsg.warning Errormsg.none ("Invalid translation: " ^ s)

let get_translation () = !currentTranslation

(* Generate unique names for variables generated during
   translation. *)
let newVarCount = ref 0
let newVar () =
  let vname = "X_" ^ (string_of_int !newVarCount) in
  let _ = newVarCount := !newVarCount + 1 in
  vname

(* Construct the two kinds lftype and lfobj *)
let lftypeStr = "lf_type"
let lftypeSymb = Symbol.symbol lftypeStr
let lftype = Absyn.Kind(lftypeSymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none)

let lfobjStr = "lf_object"
let lfobjSymb = Symbol.symbol lfobjStr
let lfobj = Absyn.Kind(lfobjSymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none)

(* Construct the two default predicates istype and hastype *)
let istypeStr = "istype"
let istypeSymb = Symbol.symbol istypeStr
let istype = Absyn.Constant(istypeSymb, ref Absyn.NoFixity, ref 0, ref true, ref false, ref false,
			    ref false, ref false, ref false,
			    ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lftype,[]),
						  	             Absyn.ApplicationType(Pervasive.kbool,[])),
					             ref None, ref false))),
			    ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)

let hastypeStr = "hastype"
let hastypeSymb = Symbol.symbol hastypeStr
let hastype = Absyn.Constant(hastypeSymb, ref Absyn.NoFixity, ref 0, ref true, ref false, ref false,
			     ref false, ref false, ref false,
			     ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lfobj,[]),
							              Absyn.ArrowType(Absyn.ApplicationType(lftype, []),
									              Absyn.ApplicationType(Pervasive.kbool,[]))),
					              ref None, ref false))),
			     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)

(* makeApp: takes a head term `h' and a list of argument terms `a1', `a2', ..., `an' 
            and returns an application term `(((h a1) a2) ... an)'
*)
let makeApp h args =
  List.fold_left (fun t a -> Absyn.ApplicationTerm(Absyn.CurriedApplication(t,a),Errormsg.none))
                 h
                 args

(** Flatten an LF kind into a simple type. *)
let rec flatten_kind k =
  match k with
      Lfabsyn.PiKind(_, ty, body,_) ->
        Absyn.ArrowType((flatten_type ty), (flatten_kind body))
    | Lfabsyn.Type ->
        Absyn.ApplicationType(lftype,[])
(** Flatten an LF type into a simple type. *)
and flatten_type t =
  match t with
      Lfabsyn.PiType(_, ty, body,dep) ->
        Absyn.ArrowType((flatten_type ty), (flatten_type body))
    | Lfabsyn.AppType(_,_)
    | Lfabsyn.IdType(_) -> Absyn.ApplicationType(lfobj,[])

(** Generate a new (unused) name 
    Takes three arguments: metadata mapping indicating the unavailable symbols
    used by constants, collection of used symbols for bound variables, and 
    the original symbol (used in gnerating the new name) **)
let gen_name metadata names s =
  let check_constants s =
    Option.isSome (Metadata.getLF metadata s)
  in let check_names s =
    List.exists (fun x -> x = s) names
  in
  let rec aux s i =
    let s' = Symbol.symbol (s ^ (string_of_int i)) in
    if (check_constants s') || (check_names s')
    then aux s (i+1)
    else s'
  in
  (* MKS:
     This could be more intelligent, changing x1 to x2 rather than x11,
     if we checked out the initial name more first *)
  aux (Symbol.name s) 0
                                                
(** Encode an LF term into a simply typed term. *)
let rec encode_term constants metadata vars names sub tm =
    match tm with
      Lfabsyn.AbsTerm(s,ty,t) ->
        let (s',sub') =
          let s' = Symbol.symbol (Symb.name s) in
          let s'' = try snd (List.find (fun (x,y) -> x = s') sub)
                    with Not_found -> s' in
          if List.exists (fun x -> x = s'') names
          then
            let s''' = gen_name metadata names s'' in
            (s''', (s'',s''')::sub)
          else
            (s'', sub)
        in
        let names' = s' :: names in
        let bvar = Absyn.BoundVar(s',ref None,ref false,ref (Some(flatten_type ty))) in
        let vars' = Table.add s' bvar vars in
          Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar,
							encode_term constants metadata vars' names' sub' t),
				Errormsg.none)
      | Lfabsyn.AppTerm(head,tms) ->
          let transhead = encode_term constants metadata vars names sub (Lfabsyn.IdTerm(head)) in
          let transtms = List.map (encode_term constants metadata vars names sub) tms in
          makeApp transhead transtms
      | Lfabsyn.IdTerm(id) ->
          match id with
              Lfabsyn.Const(s) ->
                (match (Metadata.getLP metadata s) with
                  Some(s') ->
                    (match (Table.find s' constants) with
                       Some(c) ->
                         Absyn.ConstantTerm(c, [], Errormsg.none)
                     | None ->
                         Errormsg.error Errormsg.none
                           ("No constant found for LP symbol: '" ^ (Symbol.printName s') ^
                            "' in LF term: '" ^ (PrintLF.string_of_term tm) ^ "'");
                         Absyn.ErrorTerm)
                | None ->
                    Errormsg.error Errormsg.none
                      ("No mapping found for LF constant `" ^ (Symb.name s) ^ "`.");
                      Absyn.ErrorTerm)
            | Lfabsyn.Var(s,t) ->
               let s' =
                 let s = Symbol.symbol (Symb.name s) in
                 try snd (List.find (fun (x,y) -> x = s) sub)
                 with Not_found -> s
               in
               (match (Table.find s' vars) with
                  Some(tysymb) -> Absyn.makeBoundVarTerm tysymb Errormsg.none
                | None ->
                   Errormsg.error Errormsg.none ("No variable named `"^(Symb.name s)^"' found in scope.");
                         Absyn.ErrorTerm)
            | Lfabsyn.LogicVar(s,t) ->
                  (match (Table.find (Symbol.symbol (Symb.name s)) vars) with
                       Some(tysymb) -> Absyn.makeFreeVarTerm tysymb Errormsg.none
                     | None ->
                         Errormsg.error Errormsg.none
                                        ("No variable named `"^(Symb.name s)^"' found in scope.");
                         Absyn.ErrorTerm)

(** Encode an LF kind as a term.
      @returns a function that when applied to the encoding of an LF
               constant `a' produces a term encoding the judgement
               `a : k'. *)
let rec encode_kind opt metadata consttbl vars names sub k =
  match k with
      Lfabsyn.PiKind(s,ty,k,dep) ->
      fun m ->
          let (s', sub') =
            let s' = Symbol.symbol (Symb.name s) in
            let s'' = try snd (List.find (fun (x,y) -> x = s') sub)
                      with Not_found -> s' in
            if List.exists (fun x -> x = s'') names
            then let s''' = gen_name metadata names s'' in (s''', (s'', s''')::sub)
            else (s'', sub)
          in
          let names' = s' :: names in
          let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type ty))) in
          let vartm = Absyn.makeBoundVarTerm (bvar) Errormsg.none in
          let pos_tp =
            if opt
            then fst (Strictness.find_strict_vars_pos ty Strictness.SymbSet.empty)
            else Strictness.PosNone
          in
          let l = (encode_type_positive opt metadata consttbl vars names' sub ty pos_tp) vartm in
          let r = (encode_kind opt metadata consttbl vars names' sub' k) (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant,[],Errormsg.none)) [l;r] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant,[],Errormsg.none)) [abstm]
    | Lfabsyn.Type ->
        let istype =
          (match Table.find istypeSymb consttbl with
               Some(c) -> c
             | None -> istype)
        in
        fun m ->
          Absyn.ApplicationTerm(Absyn.CurriedApplication(Absyn.ConstantTerm(istype, [], Errormsg.none), m), Errormsg.none)

(** Encode an LF type as a term repsenting a clause.
      @returns a function that when applied to the encoding of an LF
               constant `c' produces a term encoding the judgement
               `c : t'. *)

and encode_type_negative opt metadata consttbl vars names sub ty neg_tp =
  match ty with
      Lfabsyn.PiType(s,typ,body,dep) ->
      fun m ->
      (
        let (s', sub') =
          let s' = Symbol.symbol (Symb.name s) in
          let s'' = try snd (List.find (fun (x,y) -> x = s') sub)
                    with Not_found -> s' 
          in
          if List.exists (fun x -> x = s'') names
          then let s''' = gen_name metadata names s'' in (s''', (s'',s''')::sub)
          else (s'', sub)
        in
        let names' = s' :: names in
          match neg_tp with
          | Strictness.Neg (binders, tycon, tms, stricts) ->
            let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type ty))) in
            let vars' = Table.add s' bvar vars in
            let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
            let neg_tp' = Strictness.Neg(List.tl binders, tycon, tms, stricts) in
            let r = (encode_type_negative opt metadata consttbl vars' names' sub' body neg_tp') (makeApp m [vartm]) in
            let bodytm =
              if (Strictness.SymbSet.mem s stricts)
              then
                r
              else
                let l = (encode_type_positive opt metadata consttbl vars' names' sub typ (snd (List.hd binders))) vartm in
                makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
            in
            let abstm =
              Absyn.AbstractionTerm(
                Absyn.NestedAbstraction(bvar, bodytm),
                Errormsg.none)
            in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
          | Strictness.NegNone ->
            let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type ty))) in
            let vars' = Table.add s' bvar vars in
            let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
            let r = (encode_type_negative opt metadata consttbl vars' names' sub' body neg_tp) (makeApp m [vartm]) in
            let bodytm =
              let l = (encode_type_positive opt metadata consttbl vars' names' sub typ Strictness.PosNone) vartm in
              makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
            in
            let abstm =
              Absyn.AbstractionTerm(
                Absyn.NestedAbstraction(bvar, bodytm),
                Errormsg.none)
            in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
        )
  | Lfabsyn.AppType(id,tms) ->
        let hastype =
          (match Table.find hastypeSymb consttbl with
               Some(c) -> c
             | None -> hastype)
        in
        fun m ->
          (match (Metadata.getLP metadata (Lfabsyn.get_id_symb id)) with
               Some(s') ->
                 (match Table.find s' consttbl with
                      Some(c) ->
                        let lptms = List.map (encode_term consttbl metadata vars names sub) tms in
                        let tytm = makeApp (Absyn.ConstantTerm(c,[],Errormsg.none)) lptms in
                        makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;tytm]
                    | None ->
                        Errormsg.error Errormsg.none
                                       ("No constant found for LP symbol: '" ^ (PrintLF.string_of_id id) ^
                                            "' in LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none
                                ("No mapping found for LF constant: '" ^ (PrintLF.string_of_id id) ^
                                     "' in LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
                 Absyn.ErrorTerm)
    | Lfabsyn.IdType(id) ->
        let hastype =
          (match Table.find hastypeSymb consttbl with
               Some(c) -> c
             | None -> hastype)
        in
        fun m ->
          (match (Metadata.getLP metadata (Lfabsyn.get_id_symb id)) with
               Some(s) ->
                 (match Table.find s consttbl with
                      Some(c) -> makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;Absyn.ConstantTerm(c,[],Errormsg.none)]
                    | None ->
                        Errormsg.error Errormsg.none
                                       ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
                        Absyn.ErrorTerm)
             | None ->
                 Errormsg.error Errormsg.none
                                ("No mapping found for LF constant: '" ^ (PrintLF.string_of_id id) ^ "'");
                 Absyn.ErrorTerm)

(** Similar to {!encode_type_negative} but generates a term representing
    a goal rather than a clause. *)
and encode_type_positive opt metadata consttbl vars names sub ty pos_tp =
  match ty with
    Lfabsyn.PiType(s,typ,body,dep) ->
    fun m ->
    (
        let (s', sub') =
          let s' = Symbol.symbol (Symb.name s) in
          let s'' = try snd (List.find (fun (x,y) -> x = s') sub)
                    with Not_found -> s' 
          in
          if List.exists (fun x -> x = s'') names
          then let s''' = gen_name metadata names s'' in (s''', (s'',s''')::sub)
          else (s'', sub)
        in
        let names' = s' :: names in
        match pos_tp with
        | Strictness.Pos (binders, tycon, tms) ->
          let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type ty))) in
          let vars' = Table.add s' bvar vars in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let l = (encode_type_negative opt metadata consttbl vars' names' sub typ (snd(List.hd binders))) vartm in
          let pos_tp' = Strictness.Pos(List.tl binders, tycon, tms) in
          let r = (encode_type_positive opt metadata consttbl vars' names' sub' body pos_tp') (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
        | Strictness.PosNone ->
          let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type ty))) in
          let vars' = Table.add s' bvar vars in
          let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
          let l = (encode_type_negative opt metadata consttbl vars' names' sub typ Strictness.NegNone) vartm in
          let r = (encode_type_positive opt metadata consttbl vars' names' sub' body pos_tp) (makeApp m [vartm]) in
          let bodytm = makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r] in
          let abstm =
            Absyn.AbstractionTerm(
              Absyn.NestedAbstraction(bvar, bodytm),
              Errormsg.none)
          in
          makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none)) [abstm]
      )
  | Lfabsyn.AppType(id,tms) ->
    let hastype =
      (match Table.find hastypeSymb consttbl with
         Some(c) -> c
       | None -> hastype)
    in
    fun m ->
      (match (Metadata.getLP metadata (Lfabsyn.get_id_symb id)) with
         Some(s) ->
         (match Table.find s consttbl with
            Some(c) ->
            let lptms = List.map (encode_term consttbl metadata vars names sub) tms in
            let tytm = makeApp (Absyn.ConstantTerm(c,[],Errormsg.none)) lptms in
            makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;tytm]
          | None ->
            Errormsg.error Errormsg.none
              ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
            Absyn.ErrorTerm)
       | None ->
         Errormsg.error Errormsg.none
           ("No mapping found for LF constant: '" ^ (Lfabsyn.get_id_name id) ^ "'");
         Absyn.ErrorTerm)
  | Lfabsyn.IdType(id) ->
    let hastype =
      (match Table.find hastypeSymb consttbl with
         Some(c) -> c
       | None -> hastype)
    in
    fun m ->
      (match (Metadata.getLP metadata (Lfabsyn.get_id_symb id)) with
         Some(s) ->
         (match Table.find s consttbl with
            Some(c) ->
            makeApp (Absyn.ConstantTerm(hastype, [], Errormsg.none)) [m;Absyn.ConstantTerm(c,[],Errormsg.none)]
          | None ->
            Errormsg.error Errormsg.none
              ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ "'");
            Absyn.ErrorTerm)
       | None ->
         Errormsg.error Errormsg.none
           ("No mapping found for LF constant: '" ^ (PrintLF.string_of_id id) ^ "'");
         Absyn.ErrorTerm)

(* the translated constants are not assigned fixity *)
  (*
let trans_fixity fix assoc = 
  match (fix, assoc) with
      (Lfabsyn.NoFixity,_) -> Absyn.NoFixity
    | (Lfabsyn.Infix,Lfabsyn.Left) -> Absyn.Infixl
    | (Lfabsyn.Infix,Lfabsyn.Right) -> Absyn.Infixr
    | (Lfabsyn.Infix,_) -> Absyn.Infix
    | (Lfabsyn.Prefix,Lfabsyn.Right) -> Absyn.Prefixr
    | (Lfabsyn.Prefix,_) -> Absyn.Prefix
    | (Lfabsyn.Postfix,Lfabsyn.Left) ->	Absyn.Postfixl
    | (Lfabsyn.Postfix,_) -> Absyn.Postfix
  *)

(* set up the name mapping for constants.
     Per typefam:
       create mapping for the type-level constant,
       per object associated with type fam:
         create mapping for the object-level constant *)
let initialize_metadata types objs =
  let perType symb (Lfabsyn.TypeFam(s,_,_,_,_,_,_)) metadata =
    Metadata.new_mapping metadata s
  in
  let perObj symb (Lfabsyn.Object(s,_,_,_,_,_)) metadata =
    Metadata.new_mapping metadata s
  in
  Symboltable.fold objs perObj (Symboltable.fold types perType Metadata.empty)

(* add constants for each type and each object-level constant to the constant table being built. *)
let initialize_constants metadata types objs =
  let perObj symb (Lfabsyn.Object(s, ty,fix,assoc,prec,_)) constants =
    let lpsymb = Option.get (Metadata.getLP metadata s) in
    let objconst =
      Absyn.Constant(lpsymb, ref Absyn.NoFixity, ref 0,
                     ref true, ref false, ref false, ref false, ref false, ref false,
                     ref (Some(Absyn.Skeleton(flatten_type ty, ref None, ref false))),
                     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
                     ref 0, Errormsg.none)
    in
    Table.add lpsymb objconst constants
  in
  let perType symb (Lfabsyn.TypeFam(s,kind,fix,assoc,prec,objs,_)) constants =
    let lpsymb = Option.get (Metadata.getLP metadata symb) in
    let tyconst =
      Absyn.Constant(lpsymb, ref Absyn.NoFixity, ref 0,
	  	     ref true, ref false, ref false, ref false, ref false, ref false,
		     ref (Some(Absyn.Skeleton(flatten_kind kind, ref None, ref false))),
		     ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
		     ref 0, Errormsg.none)
    in
    Table.add lpsymb tyconst constants
  in
  Symboltable.fold objs perObj
                   (Symboltable.fold types perType
                                     (Table.add hastypeSymb hastype
					        (Table.add istypeSymb istype
						           Table.empty)))


(* Process each type level declaration and each corresponding object level declaration. *)
let process strictness metadata constants types objs =
  let perObj (Lfabsyn.Object(symb,typ,_,_,_,_) as o) clauselst =
    match (Symboltable.lookup objs symb) with
      Some(Lfabsyn.Object(symb,typ,_,_,_,_) as o) ->
      (match (Metadata.getLP metadata symb) with
           Some(s) ->
             (match (Table.find s constants) with
                  Some(c) ->
                    let aterm = Absyn.ConstantTerm(c, [], Errormsg.none) in
                    let (neg_typ, str_vars) =
                      if strictness
                      then
                        (Strictness.find_strict_vars_neg typ (Strictness.SymbSet.empty))
                      else (Strictness.NegNone, Strictness.SymbSet.empty)
                    in 
                    let clause = (encode_type_negative strictness metadata constants Table.empty [] [] typ neg_typ) aterm in
                    List.append clauselst [clause]
                | None ->
                    Errormsg.error Errormsg.none
                                   ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^
                                        "' from LF object decl: '" ^ (PrintLF.string_of_obj o) ^ "'");
                    clauselst)
         | None ->
             Errormsg.error Errormsg.none
                            ("No mapping found for LF constant: '" ^ (Symb.name symb) ^
                                 "' from LF object decl: '" ^ (PrintLF.string_of_obj o) ^ "'");
           clauselst)
    | None ->
        Errormsg.error Errormsg.none
                       ("No object constnat named '" ^ (Symb.name symb) ^"' found in table.");
        clauselst
  in
  let perType symb ((Lfabsyn.TypeFam(symb,kind,_,_,_,objects,_)) as t) clauselst =
    match (Metadata.getLP metadata symb) with
        Some(s) ->
          (match (Table.find s constants) with
               Some(c) ->
               List.fold_left (fun c o -> match Symboltable.lookup objs o with
                                            Some(o') -> perObj o' c
                                          | None -> c) clauselst (!objects)
             | None ->
                 Errormsg.error Errormsg.none 
                                ("No constant found for LP symbol: '" ^ (Symbol.printName s) ^ 
                                     "' from LF type decl: '" ^ (PrintLF.string_of_typefam t) ^ "'");
                 clauselst)
      | None ->
          Errormsg.error Errormsg.none 
                         ("No mapping found for LF constant: '" ^ (Symb.name symb) ^
                              "' from LF type decl: '" ^ (PrintLF.string_of_typefam t) ^ "'");
          clauselst
  in
  Symboltable.fold types perType []


let process_query fvars (prooftermSymb, querytype) metadata constTab strictness =
  let get_fvars pairs =
    let f (tysymbs, table) (s,t) =
      let tysymb = Absyn.ImplicitVar((Symbol.symbol (Symb.name s)), ref None, ref true, ref (Some(flatten_type t))) in
      let table' = Table.add (Absyn.getTypeSymbolSymbol tysymb) tysymb table in
        ((tysymb :: tysymbs), table')
    in
    List.fold_left f ([], Table.empty) pairs
  in
  let (fvarlist, typesymbTable) = get_fvars fvars in
  let pt_typsymb = Absyn.ImplicitVar(Symbol.symbol (Symb.name prooftermSymb), ref None, ref true, ref (Some(flatten_type querytype))) in
  let typesymbTable' = Table.add (Absyn.getTypeSymbolSymbol pt_typsymb) pt_typsymb typesymbTable in
  let varterm = Absyn.makeFreeVarTerm pt_typsymb Errormsg.none in
  let pos_tp =
    if strictness
    then fst (Strictness.find_strict_vars_pos querytype Strictness.SymbSet.empty)
    else Strictness.PosNone
  in
  let enctype =  (encode_type_positive strictness metadata constTab typesymbTable (List.map (fun (x,y) -> Symbol.symbol (Symb.name x)) fvars) [] querytype pos_tp) varterm in
  (enctype, pt_typsymb :: fvarlist)
  
module NaiveTranslation : Translator =
struct
  let translate (Lfsig.Signature(types, objs)) =
    let metadata = initialize_metadata types objs in
    let kinds = Table.add (Symbol.symbol lfobjStr) lfobj (Table.add (Symbol.symbol lftypeStr) lftype Table.empty) in
    let constants = initialize_constants metadata types objs in
    let clauses = process false metadata constants types objs in
    (metadata, kinds, constants, clauses)

  let translate_query (Lfabsyn.Query(vars, symb, ty)) metadata kindTab constTab =
    process_query vars (symb, ty) metadata constTab false
end

module OptimizedTranslation : Translator =
struct
  (** Run the optimizations which are on. *)
  let run_optimizations sign =
    let specialized = 
      if (Optimization.Specialize.get ())
        then Optimization.Specialize.run_optimization sign
        else sign
    in
    let swapped =
      if (Optimization.Swap.get ())
        then Optimization.Swap.run_optimization specialized
        else specialized
    in
    swapped

  let optimize tm =
    let specialized =
      if (Optimization.Specialize.get ())
        then Optimization.Specialize.optimize tm
        else tm
    in
    let swapped =
      if (Optimization.Swap.get ())
        then Optimization.Swap.optimize specialized
        else specialized
    in
    swapped

  let translate (Lfsig.Signature(types, objs)) =
    let metadata = initialize_metadata types objs in
    let kinds = Table.add lfobjSymb lfobj (Table.add lftypeSymb lftype Table.empty) in
    let constants = initialize_constants metadata types objs in
    let clauses = process true metadata constants types objs in
    let solun = (metadata, kinds, constants, clauses) in
    run_optimizations solun


  let translate_query (Lfabsyn.Query(vars, ptSymb, ty)) metadata kindTab constTab =
    let (unop_query, fvars) = process_query vars (ptSymb, ty) metadata constTab true in
    (optimize unop_query, fvars)
end
