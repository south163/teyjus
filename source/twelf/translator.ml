(** Translators for translating LF specifications into LP programs. *)

(* rev_imp takes a term representing a clause and reverses the order 
   of all the iplications. Only needed while teyjus code solves
   implications in reverse order.*)
let makeApp h args =
  List.fold_left (fun t a -> Absyn.ApplicationTerm(Absyn.CurriedApplication(t,a),Errormsg.none))
                 h
                 args
let makeAll body tysymb =
  makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
          [Absyn.AbstractionTerm(Absyn.NestedAbstraction(tysymb, body), Errormsg.none)]
let makeImp l r =
  makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
    
let rev_imp tm =
  let rec aux uvars tms t =
    match t with
    | Absyn.ApplicationTerm(
        Absyn.CurriedApplication(
          Absyn.ConstantTerm(c,_,_),
          Absyn.AbstractionTerm(Absyn.NestedAbstraction(tysymb,body),_)),_)
          when c = Pervasive.allConstant ->
      aux (tysymb :: uvars) tms body
    | Absyn.ApplicationTerm(
        Absyn.CurriedApplication(
          Absyn.ApplicationTerm(
            Absyn.CurriedApplication(Absyn.ConstantTerm(c,_,_),l),_), r),_)
          when c = Pervasive.implConstant ->
      aux uvars ((aux' [] [] l) :: tms) r
    | _ ->
      List.fold_left makeAll
                    (List.fold_right makeImp tms t)
                    uvars 
(*    | _ ->
      (List.fold_left (fun f tm ->(fun d ->f (makeImp tm d))) (fun x -> x) tms) t  *)
(*    | _ ->
      List.fold_right (fun tysymb t -> makeAll t tysymb)
                      uvars
      (List.fold_right makeImp tms t)  *)
  and aux' uvars tms t =
    match t with
    | Absyn.ApplicationTerm(
        Absyn.CurriedApplication(
          Absyn.ConstantTerm(c,_,_),
          Absyn.AbstractionTerm(Absyn.NestedAbstraction(tysymb,body),_)),_)
          when c = Pervasive.allConstant ->
      (aux' (tysymb :: uvars) tms body)
    | Absyn.ApplicationTerm(
        Absyn.CurriedApplication(
          Absyn.ApplicationTerm(
            Absyn.CurriedApplication(Absyn.ConstantTerm(c,_,_),l),_), r),_)
          when c = Pervasive.implConstant ->
      aux' uvars ((aux [] [] l) :: tms) r 
    | _ ->
      List.fold_left makeAll (List.fold_left (fun x y -> makeImp y x) t tms) uvars      
  in
  aux [] [] tm
  
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
      "original"
    | "optimized" ->
        currentTranslation := s
    | _ -> Errormsg.warning Errormsg.none ("Invalid translation: " ^ s)

let get_translation () = !currentTranslation

(* option to run linear heads optimization after translating a clause. *)
let linearization = ref false
let linearize b = linearization := b

(* makeApp: takes a head term `h' and a list of argument terms `a1', `a2', ..., `an' 
            and returns an application term `(((h a1) a2) ... an)'
*)
let makeApp h args =
  List.fold_left (fun t a -> Absyn.ApplicationTerm(Absyn.CurriedApplication(t,a),Errormsg.none))
                 h
                 args

                    
                                                
(** Generate a new (unused) name 
    Takes three arguments: metadata mapping indicating the unavailable symbols
    used by constants, collection of used symbols for bound variables, and 
    the original symbol (used in gnerating the new name) **)
let gen_name metadata vars s =
  let check_constants s =
    Option.isSome (Metadata.getLF metadata s)
  in let check_names s =
    Option.isSome (Table.find s vars)
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



(* sets of names *)                   
module StringSet = Set.Make(String)

  
module OriginalTranslation : Translator =
struct

  (* Construct the two kinds lftype and lfobj *)
  let lftypeStr = "lf_type"
  let lftypeSymb = Symbol.symbol lftypeStr
  let lftype = Absyn.Kind(lftypeSymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none)

  let lfobjStr = "lf_object"
  let lfobjSymb = Symbol.symbol lfobjStr
  let lfobj = Absyn.Kind(lfobjSymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none)

  (* Construct the two default predicate hastype *)
  let hastypeStr = "hastype"
  let hastypeSymb = Symbol.symbol hastypeStr
  let hastype = Absyn.Constant(hastypeSymb, ref Absyn.NoFixity, ref 0, ref true, ref false, ref false,
  			       ref true, ref false, ref false,
  			       ref (Some(Absyn.Skeleton(Absyn.ArrowType(Absyn.ApplicationType(lftype,[]),
							                Absyn.ArrowType(Absyn.ApplicationType(lfobj, []),
									                Absyn.ApplicationType(Pervasive.kbool,[]))),
					                ref None, ref false))),
			       ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant, ref 0, Errormsg.none)

  (** Encode an LF kind as a simple type. *)
  let rec encode_kind k =
    match k with
        Lfabsyn.PiKind(_, ty, body,_) ->
          Absyn.ArrowType((encode_type ty), (encode_kind body))
      | Lfabsyn.Type ->
          Absyn.ApplicationType(lftype,[])
  (** Encode an LF type as a simple type. *)
  and encode_type t =
    match t with
        Lfabsyn.PiType(_, ty, body,dep) ->
          Absyn.ArrowType((encode_type ty), (encode_type body))
      | Lfabsyn.AppType(_,_)
      | Lfabsyn.IdType(_) -> Absyn.ApplicationType(lfobj,[])
                                                  
  (** Encode an LF term into a simply typed term. *)
  let rec encode_term constants metadata vars sub tm =
      match tm with
        Lfabsyn.AbsTerm(s,ty,t) ->
          let (s',sub') =
            let s' = Symbol.symbol (Symb.name s) in
            if Option.isSome (Table.find s' vars)
            then
              let s'' = gen_name metadata vars s' in
              (s'', (s',s'')::sub)
            else
              (s', sub)
          in
          let bvar = Absyn.BoundVar(s',ref None,ref false,ref (Some(encode_type ty))) in
          let vars' = Table.add s' bvar vars in
            Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar,
                                                          encode_term constants metadata vars' sub' t),
                                  Errormsg.none)
        | Lfabsyn.AppTerm(head,tms) ->
            let transhead = encode_term constants metadata vars sub (Lfabsyn.IdTerm(head)) in
            let transtms = List.map (encode_term constants metadata vars sub) tms in
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
                 if Symb.name s = "_"
                 then
                   Absyn.makeFreeVarTerm (Absyn.AnonymousImplicitVar(Symbol.symbol "_", ref None,
                                                                     ref false, ref (Some(encode_type t))))
                                         Errormsg.none
                 else
                   (match (Table.find (Symbol.symbol (Symb.name s)) vars) with
                      Some(tysymb) -> Absyn.makeFreeVarTerm tysymb Errormsg.none
                    | None ->
                       Errormsg.error Errormsg.none
                                      ("No logic variable named `"^(Symb.name s)^"' found in scope.");
                       Absyn.ErrorTerm)


  (* add constants for each type and each object-level constant to the constant table being built. *)
  let initialize_constants metadata types objs =
    let perObj symb (Lfabsyn.Object(s, ty,fix,assoc,prec,_)) constants =
      let lpsymb = Option.get (Metadata.getLP metadata s) in
      let objconst =
        Absyn.Constant(lpsymb, ref Absyn.NoFixity, ref 0,
                       ref false, ref false, ref false, ref false, ref false, ref false,
                       ref (Some(Absyn.Skeleton(encode_type ty, ref None, ref false))),
                       ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
                       ref 0, Errormsg.none)
      in
      Table.add lpsymb objconst constants
    in
    let perType symb (Lfabsyn.TypeFam(s,kind,fix,assoc,prec,objs,_)) constants =
      let lpsymb = Option.get (Metadata.getLP metadata symb) in
      let tyconst =
        Absyn.Constant(lpsymb, ref Absyn.NoFixity, ref 0,
	  	       ref false, ref false, ref false, ref false, ref false, ref false,
		       ref (Some(Absyn.Skeleton(encode_kind kind, ref None, ref false))),
		       ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
  		       ref 0, Errormsg.none)
      in
      Table.add lpsymb tyconst constants
    in
    Symboltable.fold objs perObj
                     (Symboltable.fold types perType (Table.add hastypeSymb hastype Table.empty))        
  
  let rec encode_pos metadata constants vars sub ty =
    match ty with
    | Lfabsyn.PiType(s,ty,body,dep) ->
       let (s',sub') =
         let s' = Symbol.symbol (Symb.name s) in
         if Option.isSome (Table.find s' vars)
         then let s'' = gen_name metadata vars s' in
              (s'', (s',s'')::sub)
         else (s', sub)
       in
       let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(encode_type ty))) in
       let vars' = Table.add s' bvar vars in
       let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
         (fun m -> 
            let bodytm =
              let l = (encode_neg metadata constants vars' sub ty) vartm in
              let r = (encode_pos metadata constants vars' sub' body) (makeApp m [vartm]) in
              makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
            in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
                    [Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar, bodytm), Errormsg.none)]  )
    | Lfabsyn.AppType(Lfabsyn.Const(s),tms) ->
       (match Metadata.getLP metadata s with
        | Some(s') ->
           (match Table.find s' constants with
            | Some(c) ->
               let tms' = List.map (encode_term constants metadata vars sub) tms in
               let typtm = makeApp (Absyn.ConstantTerm(c,[],Errormsg.none)) tms' in
               (fun m ->
                makeApp (Absyn.ConstantTerm(Option.get (Table.find hastypeSymb constants), [], Errormsg.none))
                        [typtm;m] )
            | None ->
               Errormsg.error Errormsg.none
                              ("No constant found for LP symbol: '" ^ (Symbol.printName s') ^
                                 "' when translating LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
               (fun m -> Absyn.ErrorTerm) ) 
        | None ->
           Errormsg.error Errormsg.none
                          ("No mapping found for the LF cosntant: '" ^ (Symb.name s) ^
                             "' in LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
           (fun m -> Absyn.ErrorTerm) ) 
    | Lfabsyn.IdType(Lfabsyn.Const(s)) ->
       (match Metadata.getLP metadata s with
        | Some(s') ->
           (match Table.find s' constants with
            | Some(c) ->
               (fun m ->
               makeApp (Absyn.ConstantTerm(Option.get (Table.find hastypeSymb constants),[],Errormsg.none))
                       [Absyn.ConstantTerm(c,[],Errormsg.none);m] )
            | None ->
               Errormsg.error Errormsg.none
                              ("No constant found for LP symbol: '" ^ (Symbol.printName s') ^
                                 "' when translating LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
               (fun m -> Absyn.ErrorTerm) ) 
        | None ->
           Errormsg.error Errormsg.none
                          ("No mapping found for the LF cosntant: '" ^ (Symb.name s) ^
                             "' in LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
           (fun m -> Absyn.ErrorTerm) )
  and encode_neg metadata constants vars sub ty =
    match ty with
    | Lfabsyn.PiType(s,ty,body,dep) ->
       let (s',sub') =
         let s' = Symbol.symbol (Symb.name s) in
         if Option.isSome (Table.find s' vars)
         then let s'' = gen_name metadata vars s' in
              (s'', (s',s'')::sub)
         else (s', sub)
       in
       let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(encode_type ty))) in
       let vars' = Table.add s' bvar vars in
       let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
       (* NOTE: The dependency information is accurate for how we
                translate. The type reconstruction code from Twelf
                performs the appropriate analysis to ensure that by
                this point we do not need to check if a variable
                appears in the type.
                (see piDepend in abstract.ml) *)
       if dep
       then
         (fun m ->
            let r = (encode_neg metadata constants vars' sub' body) (makeApp m [vartm]) in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
                    [Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar, r), Errormsg.none)]  )
       else
         (fun m -> 
            let bodytm =
              let l = (encode_pos metadata constants vars' sub ty) vartm in
              let r = (encode_neg metadata constants vars' sub' body) (makeApp m [vartm]) in
              makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
            in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
                    [Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar, bodytm), Errormsg.none)]  )
    | Lfabsyn.AppType(Lfabsyn.Const(s),tms) ->
       (match Metadata.getLP metadata s with
        | Some(s') ->
           (match Table.find s' constants with
            | Some(c) ->
               let tms' = List.map (encode_term constants metadata vars sub) tms in
               let typtm = makeApp (Absyn.ConstantTerm(c,[],Errormsg.none)) tms' in
               (fun m ->
                makeApp (Absyn.ConstantTerm(Option.get (Table.find hastypeSymb constants), [], Errormsg.none))
                        [typtm;m] )
            | None ->
               Errormsg.error Errormsg.none
                              ("No constant found for LP symbol: '" ^ (Symbol.printName s') ^
                                 "' when translating LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
               (fun m -> Absyn.ErrorTerm) ) 
        | None ->
           Errormsg.error Errormsg.none
                          ("No mapping found for the LF cosntant: '" ^ (Symb.name s) ^
                             "' in LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
           (fun m -> Absyn.ErrorTerm) ) 
    | Lfabsyn.IdType(Lfabsyn.Const(s)) ->
       (match Metadata.getLP metadata s with
        | Some(s') ->
           (match Table.find s' constants with
            | Some(c) ->
               (fun m ->
               makeApp (Absyn.ConstantTerm(Option.get (Table.find hastypeSymb constants),[],Errormsg.none))
                       [Absyn.ConstantTerm(c,[],Errormsg.none);m] )
            | None ->
               Errormsg.error Errormsg.none
                              ("No constant found for LP symbol: '" ^ (Symbol.printName s') ^
                                 "' when translating LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
               (fun m -> Absyn.ErrorTerm) ) 
        | None ->
           Errormsg.error Errormsg.none
                          ("No mapping found for the LF cosntant: '" ^ (Symb.name s) ^
                             "' in LF type: '" ^ (PrintLF.string_of_typ ty) ^ "'");
           (fun m -> Absyn.ErrorTerm) )
       
  (* Process each type level declaration and each corresponding object level declaration. *)
  let process metadata constants types objs =
    let perObj (Lfabsyn.Object(symb,typ,_,_,_,_) as o) clauselst =
      match (Symboltable.lookup objs symb) with
        Some(Lfabsyn.Object(symb,typ,_,_,_,_) as o) ->
        (match (Metadata.getLP metadata symb) with
             Some(s) ->
               (match (Table.find s constants) with
                    Some(c) ->
                      let aterm = Absyn.ConstantTerm(c, [], Errormsg.none) in
                      let clause = (encode_neg metadata constants Table.empty [] typ) aterm in
                      let clause = rev_imp clause in
                      List.append clauselst
                                  [if (!linearization)
                                   then Linearize.linearize clause
                                   else clause]
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


  let process_query fvars (prooftermSymb, querytype) metadata constTab =
    let get_fvars pairs =
      let f (tysymbs, table) (s,t) =
        let tysymb = Absyn.ImplicitVar((Symbol.symbol (Symb.name s)), ref None, ref true, ref (Some(encode_type t))) in
        let table' = Table.add (Absyn.getTypeSymbolSymbol tysymb) tysymb table in
          ((tysymb :: tysymbs), table')
      in
      List.fold_left f ([], Table.empty) pairs
    in
    let (fvarlist, typesymbTable) = get_fvars fvars in
    let pt_typsymb = Absyn.ImplicitVar(Symbol.symbol (Symb.name prooftermSymb), ref None, ref true, ref (Some(encode_type querytype))) in
    let typesymbTable' = Table.add (Absyn.getTypeSymbolSymbol pt_typsymb) pt_typsymb typesymbTable in
    let varterm = Absyn.makeFreeVarTerm pt_typsymb Errormsg.none in
    let enctype =  (encode_pos metadata constTab typesymbTable' [] querytype) varterm in
    (enctype, pt_typsymb :: fvarlist)

  
  let translate (Lfsig.Signature(types, objs)) =
    let metadata = initialize_metadata types objs in
    let kinds = Table.add (Symbol.symbol lfobjStr) lfobj (Table.add (Symbol.symbol lftypeStr) lftype Table.empty) in
    let constants = initialize_constants metadata types objs in
    let clauses = process metadata constants types objs in
    (metadata, kinds, constants, clauses)

  let translate_query (Lfabsyn.Query(vars, symb, ty)) metadata kindTab constTab =
    process_query vars (symb, ty) metadata constTab 
end

module OptimizedTranslation : Translator =
struct
                                
  (** Encode an LF kind as a simple type. *)
  let rec flatten_kind metadata kinds k a =
    match k with
        Lfabsyn.PiKind(_, ty, body,_) ->
          Absyn.ArrowType((flatten_type metadata kinds ty), (flatten_kind metadata kinds body a))
      | Lfabsyn.Type -> (* a -> o *)
          Absyn.ArrowType(Absyn.ApplicationType(a,[]),Absyn.ApplicationType(Pervasive.kbool,[]))
  (** Encode an LF type as a simple type. *)
  and flatten_type metadata kinds t =
    match t with
        Lfabsyn.PiType(_, ty, body,dep) ->
          Absyn.ArrowType((flatten_type metadata kinds ty), (flatten_type metadata kinds body))
      | Lfabsyn.AppType(id,_)
      | Lfabsyn.IdType(id) ->
         let lpsymb =  Option.get (Metadata.getLP metadata (Lfabsyn.get_id_symb id)) in
         let knd = Option.get (Table.find lpsymb kinds) in
         Absyn.ApplicationType(knd,[])

  (** Encode an LF term into a simply typed term. *)
  let rec encode_term metadata kinds constants vars sub tm =
      match tm with
        Lfabsyn.AbsTerm(s,ty,t) ->
          let (s',sub') =
            let s' = Symbol.symbol (Symb.name s) in
            if Option.isSome (Table.find s' vars)
            then
              let s'' = gen_name metadata vars s' in
              (s'', (s',s'')::sub)
            else
              (s', sub)
          in
          let bvar = Absyn.BoundVar(s',ref None,ref false,ref (Some(flatten_type metadata kinds ty))) in
          let vars' = Table.add s' bvar vars in
            Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar,
                                                          encode_term metadata kinds constants vars' sub' t),
                                  Errormsg.none)
        | Lfabsyn.AppTerm(head,tms) ->
            let transhead = encode_term metadata kinds constants vars sub (Lfabsyn.IdTerm(head)) in
            let transtms = List.map (encode_term metadata kinds constants vars sub) tms in
            makeApp transhead transtms
        | Lfabsyn.IdTerm(id) ->
            match id with
              Lfabsyn.Const(s) ->
                 let s' = Option.get (Metadata.getLP metadata s) in
                 let c = Option.get (Table.find s' constants) in
                 Absyn.ConstantTerm(c, [], Errormsg.none)
            | Lfabsyn.Var(s,t) ->
                 let s' =
                   let s = Symbol.symbol (Symb.name s) in
                   try snd (List.find (fun (x,y) -> x = s) sub)
                   with Not_found -> s
                 in
                 let tysyb = Option.get (Table.find s' vars) in
                 Absyn.makeBoundVarTerm tysyb Errormsg.none
            | Lfabsyn.LogicVar(s,t) ->
                 if Symb.name s = "_"
                 then
                   Absyn.makeFreeVarTerm (Absyn.AnonymousImplicitVar(Symbol.symbol "_", ref None,
                                                                     ref false, ref (Some(flatten_type metadata kinds t))))
                                         Errormsg.none
                 else
                   let tysymb = Option.get (Table.find (Symbol.symbol (Symb.name s)) vars) in
                   Absyn.makeFreeVarTerm tysymb Errormsg.none

                                         
  let rec encode_pos metadata kinds constants vars sub ty =
    match ty with
    | Lfabsyn.PiType(s,ty,body,dep) ->
       let (s',sub') =
         let s' = Symbol.symbol (Symb.name s) in
         if Option.isSome (Table.find s' vars)
         then let s'' = gen_name metadata vars s' in
              (s'', (s',s'')::sub)
         else (s', sub)
       in
       let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type metadata kinds ty))) in
       let vars' = Table.add s' bvar vars in
       let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
         (fun m -> 
            let bodytm =
              let l = (encode_neg metadata kinds constants vars' sub ty) vartm in
              let r = (encode_pos metadata kinds constants vars' sub' body) (makeApp m [vartm]) in
              makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
            in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
                    [Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar, bodytm), Errormsg.none)]  )
    | Lfabsyn.AppType(Lfabsyn.Const(s),tms) ->
       let s' = Option.get (Metadata.getLP metadata s) in
       let c = Option.get (Table.find s' constants) in
       let tms' = List.map (encode_term metadata kinds constants vars sub) tms in
       (fun m ->
            makeApp (Absyn.ConstantTerm(c, [], Errormsg.none)) (tms' @ [m]) )
    | Lfabsyn.IdType(Lfabsyn.Const(s)) ->
       let s' = Option.get (Metadata.getLP metadata s) in
       let c = Option.get (Table.find s' constants) in
       (fun m -> makeApp (Absyn.ConstantTerm(c, [], Errormsg.none)) [m] )
  and encode_neg metadata kinds constants vars sub ty =
    match ty with
    | Lfabsyn.PiType(s,ty,body,dep) ->
       let (s',sub') =
         let s' = Symbol.symbol (Symb.name s) in
         if Option.isSome (Table.find s' vars)
         then let s'' = gen_name metadata vars s' in
              (s'', (s',s'')::sub)
         else (s', sub)
       in
       let bvar = Absyn.BoundVar(s', ref None, ref false, ref (Some(flatten_type metadata kinds ty))) in
       let vars' = Table.add s' bvar vars in
       let vartm = Absyn.makeBoundVarTerm bvar Errormsg.none in
       (* NOTE: The dependency information is accurate for how we
                translate. The type reconstruction code from Twelf
                performs the appropriate analysis to ensure that by
                this point we do not need to check if a variable
                appears in the type. 
                (see piDepend in abstract.ml) *)
       if dep
       then
         (fun m ->
            let r = (encode_neg metadata kinds constants vars' sub' body) (makeApp m [vartm]) in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
                    [Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar, r), Errormsg.none)]  )
       else
         (fun m -> 
            let bodytm =
              let l = (encode_pos metadata kinds constants vars' sub ty) vartm in
              let r = (encode_neg metadata kinds constants vars' sub' body) (makeApp m [vartm]) in
              makeApp (Absyn.ConstantTerm(Pervasive.implConstant, [], Errormsg.none)) [l;r]
            in
            makeApp (Absyn.ConstantTerm(Pervasive.allConstant, [], Errormsg.none))
                    [Absyn.AbstractionTerm(Absyn.NestedAbstraction(bvar, bodytm), Errormsg.none)]  )
    | Lfabsyn.AppType(Lfabsyn.Const(s),tms) ->
       let s' = Option.get (Metadata.getLP metadata s) in
       let c = Option.get (Table.find s' constants) in
       let tms' = List.map (encode_term metadata kinds constants vars sub) tms in
       (fun m ->
            makeApp (Absyn.ConstantTerm(c, [], Errormsg.none)) (tms' @ [m]) )
    | Lfabsyn.IdType(Lfabsyn.Const(s)) ->
       let s' = Option.get (Metadata.getLP metadata s) in
       let c = Option.get (Table.find s' constants) in
       (fun m -> makeApp (Absyn.ConstantTerm(c, [], Errormsg.none)) [m] )

         
  let translate (Lfsig.Signature(types, objs)) =
    let metadata = initialize_metadata types objs in
    let initialize_kinds typ_tbl tyfam kind_tbl =
      let tySymb = Option.get (Metadata.getLP metadata (Lfabsyn.get_typefam_symb tyfam)) in
      let typ = Absyn.Kind(tySymb, Some(0), ref 0, Absyn.GlobalKind, Errormsg.none) in
      (Table.add tySymb typ kind_tbl)
    in
    let kinds = Symboltable.fold types initialize_kinds Table.empty in
    (* add constants for each type and each object-level constant to the constant table being built. *)
    let constants =
      let perObj symb (Lfabsyn.Object(s, ty,fix,assoc,prec,_)) constants =
        let lpsymb = Option.get (Metadata.getLP metadata s) in
        let objconst =
          Absyn.Constant(lpsymb, ref Absyn.NoFixity, ref 0,
                         ref false, ref false, ref false, ref false, ref false, ref false,
                         ref (Some(Absyn.Skeleton(flatten_type metadata kinds ty, ref None, ref false))),
                         ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
                         ref 0, Errormsg.none)
        in
        Table.add lpsymb objconst constants
      in
      let perType symb (Lfabsyn.TypeFam(s,kind,fix,assoc,prec,objs,_)) constants =
        let lpsymb = Option.get (Metadata.getLP metadata symb) in
        let a = Option.get (Table.find lpsymb kinds) in
        let tyconst =
          Absyn.Constant(lpsymb, ref Absyn.NoFixity, ref 0,
	    	         ref true, ref false, ref false, ref false, ref false, ref false,
	  	         ref (Some(Absyn.Skeleton(flatten_kind metadata kinds kind a, ref None, ref false))),
	  	         ref 0, ref None, ref None, ref None, ref Absyn.GlobalConstant,
  	  	         ref 0, Errormsg.none)
        in  
        Table.add lpsymb tyconst constants
      in
      Symboltable.fold objs perObj
                       (Symboltable.fold types perType Table.empty)        
    in
    (* Process each type level declaration and each corresponding object level declaration. *)
    let clauses =
      let perObj (Lfabsyn.Object(symb,typ,_,_,_,_) as o) clauselst =
        let Lfabsyn.Object(symb,typ,_,_,_,_) = Option.get (Symboltable.lookup objs symb) in
        let s = Option.get (Metadata.getLP metadata symb) in
        let c = Option.get (Table.find s constants) in
        let aterm = Absyn.ConstantTerm(c, [], Errormsg.none) in
        let clause = (encode_neg metadata kinds constants Table.empty [] typ) aterm in
        let clause = rev_imp clause in
        List.append clauselst
                    [if (!linearization)
                     then Linearize.linearize clause
                     else clause]
      in
      let perType symb ((Lfabsyn.TypeFam(symb,kind,_,_,_,objects,_)) as t) clauselst =
        let s = Option.get (Metadata.getLP metadata symb) in
        let c = Option.get (Table.find s constants) in
        List.fold_left (fun c o -> let o' = Option.get (Symboltable.lookup objs o) in perObj o' c)
                       clauselst
                       (!objects)
      in
      Symboltable.fold types perType []
    in
    (metadata, kinds, constants, clauses)

  let translate_query (Lfabsyn.Query(fvars, prooftermSymb, querytype)) metadata kindTab constTab =
    let get_fvars pairs =
      let f (tysymbs, table) (s,t) =
        let tysymb = Absyn.ImplicitVar((Symbol.symbol (Symb.name s)), ref None, ref true, ref (Some(flatten_type metadata kindTab t))) in
        let table' = Table.add (Absyn.getTypeSymbolSymbol tysymb) tysymb table in
          ((tysymb :: tysymbs), table')
      in
      List.fold_left f ([], Table.empty) pairs
    in
    let (fvarlist, typesymbTable) = get_fvars fvars in
    let pt_typsymb = Absyn.ImplicitVar(Symbol.symbol (Symb.name prooftermSymb), ref None, ref true, ref (Some(flatten_type metadata kindTab querytype))) in
    let typesymbTable' = Table.add (Absyn.getTypeSymbolSymbol pt_typsymb) pt_typsymb typesymbTable in
    let varterm = Absyn.makeFreeVarTerm pt_typsymb Errormsg.none in
    let enctype =  (encode_pos metadata kindTab constTab typesymbTable' [] querytype) varterm in
    (enctype, pt_typsymb :: fvarlist)
end
