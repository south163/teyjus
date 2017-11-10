(* the Twelf context for parsing & type reconstruction *)
let context : Names.namespace option ref = ref None


let constraintsMsg (cnstrL) =
  "Typing ambiguous -- unresolved constraints\n" (*^ Print.cnstrsToString cnstrL*)
  
(* parse the implicit LF (Twelf-style) signature *)
let spine_map f s =
  let rec aux s =
    match s with
        IntSyn.Nil -> []
      | IntSyn.App(e, s') -> 
        let e' = f e in
        (e' :: aux s')
      | IntSyn.SClo(s',IntSyn.Shift(0)) -> aux s'
  in
  aux s 

    (* moving from Twelf structures to my structures *)
let rec exp_to_kind bvars e =
  match e with
      IntSyn.Uni (IntSyn.Type) -> Lfabsyn.Type
    | IntSyn.Pi ((IntSyn.Dec(name_op,ty),_), body) ->
        let t = exp_to_type bvars ty in
        if Option.isSome name_op
        then
          let b = exp_to_kind ((Symb.symbol (Option.get name_op),t) :: bvars) body in
          Lfabsyn.PiKind(Symb.symbol (Option.get name_op), t, b,true)
        else
          (* need to generate a name *)
          let name = Names.skonstName "A" in
          let b = exp_to_kind ((Symb.symbol name,t) :: bvars) body in
          Lfabsyn.PiKind(Symb.symbol name, t, b, false)
    | _ ->
        Errormsg.error Errormsg.none ("exp_to_kind: This expression is not a valid kind.");
        (* trying to continue on errors *)
        Lfabsyn.Type

and exp_to_type bvars e =
  match e with
      IntSyn.Pi ((IntSyn.Dec(name_op,ty),_), body) ->
        let t = exp_to_type bvars ty in
        if Option.isSome name_op
        then
          let s = Symb.symbol (Option.get name_op) in
          let b = exp_to_type ((s,t)::bvars) body in
          Lfabsyn.PiType(s, t, b, true)
        else
          (* need to generate a name *)
          let name = Names.skonstName "A" in
          let s = Symb.symbol name in
          let b = exp_to_type ((s,t)::bvars) body in
          Lfabsyn.PiType(s, t, b, false)
    | IntSyn.Root(IntSyn.Const(cid),IntSyn.Nil) ->
        let Names.Qid(_,name) = Names.constQid(cid) in
        Lfabsyn.IdType(Lfabsyn.Const(Symb.symbol name))
    | IntSyn.Root(IntSyn.Const(cid), spine) ->
        let Names.Qid(_,name) = Names.constQid(cid) in
        let args = spine_map (exp_to_term bvars) spine in
        Lfabsyn.AppType(Lfabsyn.Const(Symb.symbol name),args)
    | IntSyn.EClo(e', IntSyn.Shift(_)) -> exp_to_type bvars e'
    | IntSyn.EClo (e',sub) -> 
        let Lfabsyn.IdType(id) = exp_to_type bvars e' in
        Lfabsyn.AppType(id, List.rev (sub_to_args bvars sub))
    | IntSyn.Redex(exp, IntSyn.Nil) ->
        exp_to_type bvars exp
    | _ ->
        Errormsg.error Errormsg.none ("exp_to_type: This expression type: `"^(IntSyn.exp_to_string e)^"' is not a valid type.");
        (* try to continue with dummy type? *)
        Lfabsyn.IdType(Lfabsyn.Const(Symb.symbol "dummy"))


and exp_to_term bvars e =
  match e with
      IntSyn.Lam(IntSyn.Dec(name_op,ty), body) ->
        let t = exp_to_type bvars ty in
        let name =
          (match name_op with
             Some(name) ->
               name
           | None ->
               (* get a new name *)
               Names.skonstName "x")
        in
        let s = Symb.symbol name in
        let b = exp_to_term ((s,t) :: bvars) body in
        Lfabsyn.AbsTerm(s, t, b)
    | IntSyn.Root(h,IntSyn.Nil) ->
        (match h with
             IntSyn.Const(cid) ->
               let Names.Qid(_,name) = Names.constQid(cid) in
               Lfabsyn.IdTerm(Lfabsyn.Const(Symb.symbol name))
           | IntSyn.BVar(i) ->
               let (s, t) = List.nth bvars (i-1) in
               Lfabsyn.IdTerm(Lfabsyn.Var(s,t))
           | _ ->
               Errormsg.error Errormsg.none ("exp_to_term: This head has an unexpected form.");
               (* try to continue with dummy term? *)
               Lfabsyn.IdTerm(Lfabsyn.Const(Symb.symbol "dummy")))
    | IntSyn.Root(h,spine) ->
        let args = spine_map (exp_to_term bvars) spine in
        (match h with
             IntSyn.Const(cid) ->
               let Names.Qid(_,name) = Names.constQid(cid) in
               Lfabsyn.AppTerm(Lfabsyn.Const(Symb.symbol name), args)
           | IntSyn.BVar(i) ->
               let (s, t) = List.nth bvars (i-1) in
               Lfabsyn.AppTerm(Lfabsyn.Var(s,t), args)
           | _ ->
               Errormsg.error Errormsg.none ("exp_to_term: This head has an unexpected form.");
               (* try to continue with dummy term? *)
               Lfabsyn.IdTerm(Lfabsyn.Const(Symb.symbol "dummy")))
    | IntSyn.EVar(r,IntSyn.Null,ty,c) when  !c = [] ->
        if Option.isSome (!r)
        then
          exp_to_term bvars (Option.get (!r))
        else
          let t = exp_to_type bvars ty in
          let name = Names.evarName (IntSyn.Null, e) in 
          Lfabsyn.IdTerm(Lfabsyn.LogicVar(Symb.symbol name, t))
    | IntSyn.EVar(r,dctx,ty,c) when !c = [] ->
        if Option.isSome(!r)
        then
          exp_to_term bvars (Option.get (!r))
        else
          let name = Names.evarName (IntSyn.Null, e) in
          let ty_head = exp_to_type bvars ty in
          let ty = build_type_from_dctx bvars ty_head dctx in
          Lfabsyn.IdTerm(Lfabsyn.LogicVar(Symb.symbol name, ty))
    | IntSyn.EClo(e', IntSyn.Shift(_)) -> exp_to_term bvars e'
    | IntSyn.EClo (e',sub) -> 
        let (Lfabsyn.IdTerm(id)) = exp_to_term bvars e' in
        Lfabsyn.AppTerm(id, List.rev (sub_to_args bvars sub))
    | IntSyn.Redex(exp, IntSyn.Nil) ->
        exp_to_term bvars exp
    | IntSyn.Redex(exp, spine) ->
        let e' = Whnf.normalize (e, IntSyn.Shift 0) in
        exp_to_term bvars e'
    | _ ->
        Errormsg.error Errormsg.none ("exp_to_term: This expression: `"^(IntSyn.exp_to_string e)^"' is not a valid term.");
        (* try to continue with dummy term? *)
        Lfabsyn.IdTerm(Lfabsyn.Const(Symb.symbol "dummy"))   
and sub_to_args bvars sub =
  match sub with
      IntSyn.Dot(IntSyn.Idx(k), sub') -> 
        let (s, ty) = List.nth bvars (k-1) in
        (Lfabsyn.IdTerm(Lfabsyn.Var(s, ty)) :: (sub_to_args bvars sub'))
    | IntSyn.Dot(IntSyn.Exp(e), sub') -> (exp_to_term bvars e) :: (sub_to_args bvars sub')
    | IntSyn.Shift(k) -> []
and build_type_from_dctx bvars ty ctx =
  match ctx with
      IntSyn.Null -> ty
    | IntSyn.Decl(ctx', IntSyn.Dec(None,e)) ->
        let t = exp_to_type bvars e in
        let name = Names.skonstName "A" in
        build_type_from_dctx bvars (Lfabsyn.PiType(Symb.symbol name, t, ty, false)) ctx'
    | IntSyn.Decl(ctx', IntSyn.Dec(Some(name),e)) ->
        let t = exp_to_type bvars e in
        build_type_from_dctx bvars (Lfabsyn.PiType(Symb.symbol name, t, ty, true)) ctx'

let conDec_to_typeFam (IntSyn.ConDec(name, id, implicit, _, kind, _)) =
  let k = exp_to_kind [] kind in 
  Lfabsyn.TypeFam(Symb.symbol name, k, Lfabsyn.NoFixity, Lfabsyn.None, 0, ref [], implicit)

let conDec_to_obj (IntSyn.ConDec(name, id, implicit, _, ty, _)) =
  let typ = exp_to_type [] ty in
  let Lfabsyn.Const(tyhead) = Lfabsyn.get_typ_head typ in
  (Lfabsyn.Object(Symb.symbol name, typ, Lfabsyn.NoFixity, Lfabsyn.None, 0, implicit), tyhead)

let update_fixity_t (Lfabsyn.TypeFam(s,k,f,a,p,objs,i)) fixity =
  match fixity with
    Fixity.Nonfix ->
      Lfabsyn.TypeFam(s,k,Lfabsyn.NoFixity,Lfabsyn.None,0,objs,i)
  | Fixity.Infix(Fixity.Strength prec,Fixity.None) ->
      Lfabsyn.TypeFam(s,k,Lfabsyn.Infix,Lfabsyn.None,prec,objs,i)
  | Fixity.Infix(Fixity.Strength prec,Fixity.Left) ->
      Lfabsyn.TypeFam(s,k,Lfabsyn.Infix,Lfabsyn.Left,prec,objs,i)
  | Fixity.Infix(Fixity.Strength prec,Fixity.Right) ->
      Lfabsyn.TypeFam(s,k,Lfabsyn.Infix,Lfabsyn.Right,prec,objs,i)
  | Fixity.Prefix(Fixity.Strength prec) ->
      Lfabsyn.TypeFam(s,k,Lfabsyn.Prefix,Lfabsyn.None,prec,objs,i)
  | Fixity.Postfix(Fixity.Strength prec) ->
      Lfabsyn.TypeFam(s,k,Lfabsyn.Postfix,Lfabsyn.None,prec,objs,i)
  
let update_fixity_o (Lfabsyn.Object(s,t,f,a,p,i)) fixity =
  match fixity with
    Fixity.Nonfix ->
      Lfabsyn.Object(s,t,Lfabsyn.NoFixity,Lfabsyn.None,0,i)
  | Fixity.Infix(Fixity.Strength prec,Fixity.None) ->
      Lfabsyn.Object(s,t,Lfabsyn.Infix,Lfabsyn.None,prec,i)
  | Fixity.Infix(Fixity.Strength prec,Fixity.Left) ->
      Lfabsyn.Object(s,t,Lfabsyn.Infix,Lfabsyn.Left,prec,i)
  | Fixity.Infix(Fixity.Strength prec,Fixity.Right) ->
      Lfabsyn.Object(s,t,Lfabsyn.Infix,Lfabsyn.Right,prec,i)
  | Fixity.Prefix(Fixity.Strength prec) ->
      Lfabsyn.Object(s,t,Lfabsyn.Prefix,Lfabsyn.None,prec,i)
  | Fixity.Postfix(Fixity.Strength prec) ->
      Lfabsyn.Object(s,t,Lfabsyn.Postfix,Lfabsyn.None,prec,i)

  
let query_to_query (queryty, name_op, evars) =
  let ptName = match name_op with Some(n) -> n | None -> "" in
  let f l (IntSyn.EVar(e,ctx,ty,c),name) = 
    let ty_head = exp_to_type [] ty in
    let t = 
      match ctx with
          IntSyn.Null -> ty_head
        | _ -> build_type_from_dctx [] ty_head ctx
    in
    (Symb.symbol name, t) :: l
  in
  let qty = exp_to_type [] queryty in
  let fvars = List.fold_left f [] evars in
  Lfabsyn.Query(fvars, Symb.symbol ptName, qty)

let installConDec fromCS (conDec, ((fileName, ocOpt) as fileNameocOpt), r) =
  let cid = IntSyn.sgnAdd conDec in
  let _ = (try
    match (fromCS, !context) with
      (IntSyn.Ordinary, Some namespace) -> Names.insertConst (namespace, cid)
    | (IntSyn.Clause, Some namespace) -> Names.insertConst (namespace, cid)
    | _ -> ()
    with Names.Error msg -> raise (Names.Error (Paths.wrap (r, msg))))
  in
  let _ = Names.installConstName cid in
  cid
    
let parse_file filename lfsig =
  let inchann = open_in filename in
  let parseStream = Parser.parseStream inchann in
  let _ = context := Some(Names.newNamespace ()) in
  let readDec stream (Lfsig.Signature(types, objs)) =
    let rec aux stream types objs =
      match Tparsing.Parsing.Lexer'.Stream'.expose stream with
          Tparsing.Parsing.Lexer'.Stream'.Empty -> (types, objs)
        | Tparsing.Parsing.Lexer'.Stream'.Cons((Parser.AbbrevDec(condec), r), stream') ->
           let (optConDec, ocOpt) = ReconCondec.condecToConDec(condec, Paths.Loc (filename, r), true) in
           (match optConDec with
              Some(conDec') -> 
                let cid = installConDec IntSyn.Ordinary (conDec', (filename, ocOpt), r) in
                aux stream' types objs
            | None -> aux stream' types objs)
        | Tparsing.Parsing.Lexer'.Stream'.Cons((Parser.ConDec(condec), r), stream') ->
            let (conDec_op, occTree_op) = ReconCondec.condecToConDec (condec, Paths.Loc(filename, r), false) in
            (match conDec_op with
               Some(IntSyn.ConDec(_,_,_,_,_,_) as conDec) ->
                 let cid = installConDec IntSyn.Ordinary (conDec, (filename, occTree_op), r) in
                   (match IntSyn.conDecUni conDec with
                        IntSyn.Kind ->
                          let typefam = conDec_to_typeFam conDec in
                          let types' = Symboltable.insert types (Lfabsyn.get_typefam_symb typefam) typefam in
                          aux stream' types' objs
                      | IntSyn.Type ->
                          let (obj, target) = conDec_to_obj conDec in
                          let objs' = Symboltable.insert objs (Lfabsyn.get_obj_symb obj) obj in
                          match Symboltable.lookup types target with
                             None -> Errormsg.error Errormsg.none ("Type constructor "^(Symb.name target)^" not found in signature.");
                                     (* try to continue if error *)
                                     aux stream' types objs
                           | Some(Lfabsyn.TypeFam(a,b,c,d,e,objs,f)) ->       
                               let _ = objs := (List.append !objs [Lfabsyn.get_obj_symb obj]) in
                               aux stream' types objs')
             | Some(conDec) ->
                 let cid = installConDec IntSyn.Ordinary (conDec, (filename, occTree_op), r) in
                 aux stream' types objs
             | None ->  
               aux stream' types objs)
        | Tparsing.Parsing.Lexer'.Stream'.Cons((Parser.FixDec((qid,r),fixity), _), stream') ->
          (try
             match Names.constLookup qid with
               None -> raise (Names.Error ("Undeclared identifier "
                                          ^ Names.qidToString (Option.get (Names.constUndef qid))
                                          ^ " in fixity declaration"))
             | Some cid ->
               let _ = Names.installFixity (cid, fixity) in
(*
               let _ =  if !Global.chatter >= 3
                 then msg ((if !Global.chatter >= 4 then "%" else "")
                          ^ Names.Fixity.toString fixity ^ " "
                          ^ Names.qidToString (Names.constQid cid) ^ ".\n")
                 else () in
*)
               let conDec = IntSyn.sgnLookup cid in
               let name = IntSyn.conDecName conDec in
               let (types', objs') =
                 if (IntSyn.conDecUni conDec) = IntSyn.Kind
                 then
                   (match Symboltable.lookup types (Symb.symbol name) with
                     Some(tyfam) ->
                       (Symboltable.insert types (Symb.symbol name) (update_fixity_t tyfam fixity),
                        objs)
                   | None ->
                     Errormsg.error Errormsg.none ("Type constructor `"^name^"' not found in signature.");
                     (types, objs) )
                 else (* IntSyn.conDecUni conDec = IntSyn.Type *)
                   (match Symboltable.lookup objs (Symb.symbol name) with
                     Some(obj) ->
                       (types,
                        Symboltable.insert objs (Symb.symbol name) (update_fixity_o obj fixity))
                   | None ->
                     Errormsg.error Errormsg.none ("Type constructor `"^name^"' not found in signature.");
                     (types, objs) )
               in
               aux stream' types' objs'
           with Names.Error msg -> raise (Names.Error (Paths.wrap (r, msg))))
    in
    let (types', objs') = aux stream types objs in
    Lfsig.Signature(types', objs')
  in
  let lfsig = readDec parseStream lfsig in
  close_in inchann; lfsig
  
let parse_config filename =
  let path = Filename.dirname filename in
  let inchann = open_in filename in
  let rec get_files filelist =
    try
      let fn = Filename.concat path (input_line inchann) in
      if Sys.file_exists fn
      then get_files (fn :: filelist)
      else (print_endline ("Warning: Skipping file in config: `"^ fn ^"'.");  get_files filelist)
    with End_of_file -> filelist
  in 
  let filelist = get_files [] in
  List.fold_right parse_file filelist (Lfsig.Signature(Symboltable.empty, Symboltable.empty))
                    
let parse_sig filename =
  try
    if Filename.check_suffix filename ".cfg"
    then Some(parse_config filename)
    else if (Filename.check_suffix filename ".lf") || (Filename.check_suffix filename ".elf")
    then Some(parse_file filename (Lfsig.Signature(Symboltable.empty, Symboltable.empty)))
    else None 
  with
    Failure(s) -> (print_endline ("Error: " ^ s ^ "."); None)

let parse_query parseStream =
  match Tparsing.Parsing.Lexer'.Stream'.expose parseStream with
    Tparsing.Parsing.Lexer'.Stream'.Cons(query, parseStream') ->
     (try
        let (ty, name_op, evars) = ReconQuery.queryToQuery(query, Paths.Loc ("stdIn", Paths.Reg(0,0))) in
        let query = query_to_query (ty, name_op, evars) in
        Some(query)
      with _ -> None )
  | Tparsing.Parsing.Lexer'.Stream'.Empty -> None

                    
(* parse the implicit LF (Twelf-style) query *)
let parse_queryT () =
  let parseStream = Parser.parseTerminalQ("["^"top"^"] ?- ", "    ") in
  parse_query parseStream

(* parse a query from a string *)
let parse_queryStr s =
  let parseStream = Parser.parseStringQ s in
  parse_query parseStream
