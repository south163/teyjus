(* This is taken from the Twelf implementation *)

  let delayedList : (unit -> unit) list ref = ref []

  let clearDelayed () = (delayedList := [])
  let addDelayed f = (delayedList := f::(!delayedList))
  let runDelayed () =
        let rec run' l =
          match l with
              [] -> ()
            | (h::t) -> (run' t; h ())
        in
        run' (!delayedList)

  exception Error of string

  let errorCount = ref 0
  let errorFileName = ref "no file"

  let errorThreshold = ref (Some (20))
  let exceeds args =
    match args with
      (i, None) -> false
    | (i, Some(j)) -> i > j

  let resetErrors (fileName) =
      (errorCount := 0;
       errorFileName := fileName)

  let die (r) =
        raise (Error (Paths.wrap (r,
                      " " ^ (string_of_int (!errorCount))
                      ^ " error" ^ (if !errorCount > 1 then "s" else "")
                      ^ " found")))
      
  let checkErrors (r) =
       if !errorCount > 0 then die (r) else ()

  (* Since this structure uses a non-standard error reporting mechanism,
     any errors reported here while chatter = 1 will be printed
     in between the "[Loading file ..." message and the closing "]",
     instead of after the closing "]".  If we don't emit a newline
     when chatter = 1, the first such error will appear on the same line
     as "[Loading file ...", terribly confusing the Emacs error parsing code.
   *)
  let chatterOneNewline () =
      if !Global.chatter = 1 && !errorCount = 1
        then Msg.message "\n"
      else ()

  let fatalError (r, msg) =
      (errorCount := !errorCount + 1;
       chatterOneNewline ();
       Msg.message (!errorFileName ^ ":" ^ Paths.wrap (r, msg) ^ "\n");
       die (r))       
      
  let error (r, msg) =
      (errorCount := !errorCount + 1;
       chatterOneNewline ();
       Msg.message (!errorFileName ^ ":" ^ Paths.wrap (r, msg) ^ "\n");
       if exceeds (!errorCount, !errorThreshold)
          then die (r)
       else ())


  (* this is a hack, i know *)
  let queryMode = ref false

  let headConDec arg =
    match arg with
        IntSyn.Const c -> IntSyn.sgnLookup c
      | IntSyn.Skonst c -> IntSyn.sgnLookup c
      | IntSyn.Def d -> IntSyn.sgnLookup d
      | IntSyn.NSDef d -> IntSyn.sgnLookup d
      | IntSyn.FgnConst (_, cd) -> cd
      (* others impossible by invariant *)

 (* lowerType (G, (V, s)) = (G', a)
     if   G0 |- V : type and G |- s : G0
     and  G |- V[s] = {{G1}} a : type
     then G' = G, G1 *)
  let rec lowerTypeW args =
    match args with
        (g, (IntSyn.Pi ((d, _), v), s)) ->
          let d' = IntSyn.decSub (d, s) in
            lowerType (IntSyn.Decl (g, d'), (v, IntSyn.dot1 s))
      | (g, (v,s)) -> (g, IntSyn.EClo (v,s))
  and lowerType (g, vs) = lowerTypeW (g, Whnf.whnfExpandDef vs)

  (* raiseType (G, V) = {{G}} V *)
  let rec raiseType args =
    match args with
        (IntSyn.Null, v) -> v
      | (IntSyn.Decl (g, d), v) -> raiseType (g, IntSyn.Pi ((d, IntSyn.Maybe), v))

    let evarApxTable : Approx.exp Symboltable.table ref = ref Symboltable.empty
    let fvarApxTable : Approx.exp Symboltable.table ref = ref Symboltable.empty

    let fvarTable : IntSyn.exp Symboltable.table ref = ref Symboltable.empty

    let varReset () = (evarApxTable := Symboltable.empty;
                       fvarApxTable := Symboltable.empty;
                       fvarTable := Symboltable.empty)

    let getEVarTypeApx name =
      match Symboltable.lookup (!evarApxTable) (Symb.symbol name) with
          Some v -> v
        | None -> 
            (match Names.getEVarOpt name with
                 Some (IntSyn.EVar (_, _, v, _)) ->
                   let (v', _ (* Type *)) = Approx.classToApx (v) in
                   evarApxTable := Symboltable.insert (!evarApxTable) (Symb.symbol name) v';
                   v'
               | None ->
                   let v = Approx.newCVar () in
                   evarApxTable := Symboltable.insert (!evarApxTable) (Symb.symbol name) v;
                   v)

    let getFVarTypeApx name =
      match Symboltable.lookup (!fvarApxTable) (Symb.symbol name) with
          Some v -> 
            v
        | None ->
            let v = Approx.newCVar () in
            fvarApxTable := Symboltable.insert (!fvarApxTable) (Symb.symbol name) v;
            v

    let getEVar (name, allowed) =
      match Names.getEVarOpt name with
          Some ((IntSyn.EVar (_, g, v, _)) as x) -> (x, raiseType (g, v))
        | None ->
            let v = Option.get (Symboltable.lookup (!evarApxTable) (Symb.symbol name)) in
            let v' = Approx.apxToClass (IntSyn.Null, v, Approx.aType, allowed) in
            let (g'', v'') = lowerType (IntSyn.Null, (v', IntSyn.id)) in
            let x = IntSyn.newEVar (g'', v'') in
            Names.addEVar (x, name);
            (x, v')

    let getFVarType (name, allowed) =
      match Symboltable.lookup (!fvarTable) (Symb.symbol name) with
          Some v -> 
            v
        | None ->
            let v = getFVarTypeApx name (* Option.get (Symboltable.lookup (!fvarApxTable) (Symb.symbol name))  *)in
            let v' = Approx.apxToClass (IntSyn.Null, v, Approx.aType, allowed) in
            fvarTable := Symboltable.insert (!fvarTable) (Symb.symbol name) v';
            v'



  type job =
      Jnothing
    | Jand of job * job
    | Jwithctx of ExtSyn.dec IntSyn.ctx * job
    | Jterm of ExtSyn.term
    | Jclass of ExtSyn.term
    | Jof of ExtSyn.term * ExtSyn.term
    | Jof' of ExtSyn.term * IntSyn.exp

  let jnothing = Jnothing
  let jand (j1,j2) = Jand(j1,j2)
  let jwithctx (ctx, j) = Jwithctx (ctx, j)
  let jterm t = Jterm(t)
  let jclass t = Jclass(t)
  let jof (t1,t2) = Jof(t1,t2)
  let jof' (t, e) = Jof'(t, e)

  let rec termRegion arg =
    match arg with
      (ExtSyn.Internal (u, v, r)) -> r 
    | (ExtSyn.Constant (h, r)) -> r
    | (ExtSyn.Bvar (k, r)) -> r
    | (ExtSyn.Evar (name, r)) -> r
    | (ExtSyn.Fvar (name, r)) -> r
    | (ExtSyn.Typ (r)) -> r
    | (ExtSyn.Arrow (tm1, tm2)) ->
        Paths.join (termRegion tm1, termRegion tm2)
    | (ExtSyn.Pi (tm1, tm2)) ->
        Paths.join (decRegion tm1, termRegion tm2)
    | (ExtSyn.Lam (tm1, tm2)) ->
        Paths.join (decRegion tm1, termRegion tm2)
    | (ExtSyn.App (tm1, tm2)) ->
        Paths.join (termRegion tm1, termRegion tm2)
    | (ExtSyn.Hastype (tm1, tm2)) ->
        Paths.join (termRegion tm1, termRegion tm2)
    | (ExtSyn.Mismatch (tm1, tm2, _, _)) ->
        termRegion tm2
    | (ExtSyn.Omitted (r)) -> r
    | (ExtSyn.Lcid (_, _, r)) -> r
    | (ExtSyn.Ucid (_, _, r)) -> r
    | (ExtSyn.Quid (_, _, r)) -> r
    | (ExtSyn.Scon (_, r)) -> r
    | (ExtSyn.Omitapx (u, v, l, r)) -> r
    | (ExtSyn.Omitexact (u, v, r)) -> r

  and decRegion (ExtSyn.Dec (name, tm, r)) = r

  let rec ctxRegion arg =
    match arg with
        (IntSyn.Null) -> None
      | (IntSyn.Decl (g, tm)) ->
          ctxRegion' (g, decRegion tm)

  and ctxRegion' args =
    match args with
        (IntSyn.Null, r) -> Some r
      | (IntSyn.Decl (g, tm), r) ->
          ctxRegion' (g, Paths.join (r, decRegion tm))

  type 'a ctx = 'a IntSyn.ctx
  type dec = Dec of string option * Approx.exp | NDec of string option
  


    let filterLevel (tm, l, max, msg) =
      let notGround = Approx.makeGroundUni l in
      let Approx.Level i = Approx.whnfUni l in
      if i > max
      then fatalError (termRegion tm,
                       "Level too high\n" ^ msg) 
      else if notGround
      then error (termRegion tm,
                  "Ambiguous level\n" ^
                  "The level of this term could not be inferred\n" ^
                  "Defaulting to " ^
                  (match i with
                       1 -> "object"
                     | 2 -> "type family"
                     | 3 -> "kind") ^
                  " level")
      else ()
  
    let findOmitted ((g : dec IntSyn.ctx), qid, r) =
          (error (r, "Undeclared identifier '"
                     ^ Names.qidToString (Option.get (Names.constUndef qid))
                     ^ "'");
           ExtSyn.omitted (r))

    let rec findBVar' args =
      match args with
          (IntSyn.Null, name, k) -> None
      | (IntSyn.Decl (g, Dec (None, _)), name, k) ->
          findBVar' (g, name, k+1)
      | (IntSyn.Decl (g, NDec _), name, k) ->
          findBVar' (g, name, k+1)
      | (IntSyn.Decl (g, Dec (Some(name'), _)), name, k) ->
          if name = name' then Some (k)
          else findBVar' (g, name, k+1)
  
    let findBVar fc (g, qid, r) =
        (match Names.unqualified qid with
             None -> fc (g, qid, r)
          | Some name ->
              (match findBVar' (g, name, 1) with
                    None -> fc (g, qid, r)
                  | Some k -> ExtSyn.Bvar (k, r)))

    let findConst fc (g, qid, r) =
        (match Names.constLookup qid with
              None -> fc (g, qid, r)
            | Some cid ->
	      (match IntSyn.sgnLookup cid with
		    IntSyn.ConDec _ -> ExtSyn.Constant (IntSyn.Const cid, r)
	          | IntSyn.ConDef _ -> ExtSyn.Constant (IntSyn.Def cid, r)
		  | IntSyn.AbbrevDef _ -> ExtSyn.Constant (IntSyn.NSDef cid, r)
		  | _ -> 
		    (error (r, "Invalid identifier\n"
			    ^ "Identifier `" ^ Names.qidToString qid
			    ^ "' is not a constant, definition or abbreviation");
		     ExtSyn.omitted (r))))

    let findCSConst fc (g, qid, r) =
       match Names.unqualified qid with
           None -> fc (g, qid, r)
         | Some name ->
(* trying to avoid CSManager...
              (match CSManager.parse name with
                    None -> fc (g, qid, r)
                  | Some (cs, conDec) ->
                      constant (IntSyn.FgnConst (cs, conDec), r)))
*)
             fc (g, qid, r)

    let findEFVar fc (g, qid, r) =
        (match Names.unqualified qid with
              None -> fc (g, qid, r)
            | Some name -> (if !queryMode then ExtSyn.evar else ExtSyn.fvar) (name, r))

    let findLCID x = findBVar (findConst (findCSConst findOmitted)) x
    let findUCID x = findBVar (findConst (findCSConst (findEFVar findOmitted))) x
    let findQUID x = findConst (findCSConst findOmitted) x


    let rec inferApx (g, tm) =
      match (g, tm) with
        (g, ExtSyn.Internal (u, v, r)) ->
          let (u', v', l') = Approx.exactToApx (u, v) in
          (tm, u', v', l')

      | (g,ExtSyn.Lcid (ids, name, r)) ->
          let qid = Names.Qid (ids, name) in
          inferApx (g, findLCID (g, qid, r))
      | (g, ExtSyn.Ucid (ids, name, r)) ->
          let qid = Names.Qid (ids, name) in
          inferApx (g, findUCID (g, qid, r))
      | (g, ExtSyn.Quid (ids, name, r)) ->
          let qid = Names.Qid (ids, name) in
          inferApx (g, findQUID (g, qid, r))
(* avoiding CSManager ...
      | (g, Scon (name, r)) ->
          (match CSManager.parse name with
                None -> (error (r, "Strings unsupported in current signature");
                         inferApx (g, omitted (r)))
              | Some (cs, conDec) ->
                  inferApx (g, constant (IntSyn.FgnConst (cs, conDec), r)))
*)
      | (g, ExtSyn.Constant (h, r)) ->
          let cd = headConDec h in
          let (u', v', l') = Approx.exactToApx (IntSyn.Root (h, IntSyn.Nil),
                                         IntSyn.conDecType cd) in
          let rec dropImplicit args =
            (match args with
               (v, 0) -> v
             | (Approx.Arrow (_, v), i) -> dropImplicit (v, i-1))
          in
          let v'' = dropImplicit (v', IntSyn.conDecImp cd) in
          (tm, u', v'', l')
      | (g, ExtSyn.Bvar (k, r)) ->
          let Dec (_, v) = IntSyn.ctxLookup (g, k) in
          (tm, Approx.Undefined, v, Approx.aType) 
      | (g, ExtSyn.Evar (name, r)) ->
          (tm, Approx.Undefined, getEVarTypeApx name, Approx.aType)
      | (g, ExtSyn.Fvar (name, r)) ->
          (tm, Approx.Undefined, getFVarTypeApx name, Approx.aType)
      | (g, ExtSyn.Typ (r)) ->
          (tm, Approx.Uni Approx.aType, Approx.Uni Approx.aKind, Approx.aHyperkind)
      | (g, ExtSyn.Arrow (tm1, tm2)) ->
          let l = Approx.newLVar () in
          let (tm1', v1) = checkApx (g, tm1, Approx.Uni Approx.aType, Approx.aKind,
                                     "Left-hand side of arrow must be a type") in
          let (tm2', v2) = checkApx (g, tm2, Approx.Uni l, Approx.Next l,
                                     "Right-hand side of arrow must be a type or a kind") in
          (ExtSyn.Arrow (tm1', tm2'), Approx.Arrow (v1, v2), Approx.Uni l, Approx.Next l)
      | (g, ExtSyn.Pi (tm1, tm2)) ->
          let (tm1', ((Dec (_, v1)) as d)) = inferApxDec (g, tm1) in
          let l = Approx.newLVar () in
          let (tm2', v2) = checkApx (IntSyn.Decl (g, d), tm2, Approx.Uni l, Approx.Next l,
                                     "Body of pi must be a type or a kind") in
          (ExtSyn.Pi (tm1', tm2'), Approx.Arrow (v1, v2), Approx.Uni l, Approx.Next l)
      | (g, ExtSyn.Lam (tm1, tm2)) ->
          let (tm1', ((Dec (_, v1)) as d)) = inferApxDec (g, tm1) in
          let (tm2', u2, v2, l2) = inferApx (Decl (g, d), tm2) in
          (ExtSyn.Lam (tm1', tm2'), u2, Approx.Arrow (v1, v2), l2)
      | (g, ExtSyn.App (tm1, tm2)) ->
          let l = Approx.newLVar () in
          let va = Approx.newCVar () in
          let vr = Approx.newCVar () in
          let (tm1', u1) = checkApx (g, tm1, Approx.Arrow (va, vr), l,
                                     "Non-function was applied to an argument") in
          (* probably a confusing message if the problem is the level: *)
          let (tm2', _) = checkApx (g, tm2, va, Approx.aType,
                                    "Argument type did not match function domain type") in
          (ExtSyn.App (tm1', tm2'), u1, vr, l) 
      | (g, ExtSyn.Hastype (tm1, tm2)) ->
          let l = Approx.newLVar () in
          let (tm2', v2) = checkApx (g, tm2, Approx.Uni l, Approx.Next l,
                                     "Right-hand side of ascription must be a type or a kind") in
          let (tm1', u1) = checkApx (g, tm1, v2, l,
                                     "Ascription did not hold") in
          let _ = addDelayed (fun () -> filterLevel (tm, l, 2, "Ascription can only be applied to objects and type families")) in
          (ExtSyn.Hastype (tm1', tm2'), u1, v2, l)
      | (g, ExtSyn.Omitted (r)) ->
          let l = Approx.newLVar () in
          let v = Approx.newCVar () in
          let u = Approx.newCVar () in(* guaranteed not to be used if l is type *)
          (ExtSyn.Omitapx (u, v, l, r), u, v, l)

    and checkApx (g, tm, v, l, location_msg) =
      let (tm', u', v', l') = inferApx (g, tm) in
      try        
        (Approx.matchUni (l, l');  Approx.matchFun (v, v'); (tm', u'))
      with Approx.Unify problem_msg ->
        let r = termRegion tm in
        let (tm'', u'') = checkApx (g, ExtSyn.Omitted (r), v, l, location_msg) in
        (* just in case *)
        let _ = addDelayed (fun () -> (Approx.makeGroundUni l'; ())) in
        (ExtSyn.Mismatch (tm', tm'', location_msg, problem_msg), u'')

    and inferApxDec (g, Dec (name, tm, r)) =
          let (tm', v1) = checkApx (g, tm, Approx.Uni Approx.aType, Approx.aKind,
                                    "Classifier in declaration must be a type") in
          let d = Dec (name, v1) in
          (ExtSyn.dec (name, tm', r), d)

    let rec inferApxJob args =
      match args with
        (g, Jnothing) -> jnothing
      | (g, Jand (j1, j2)) ->
          jand (inferApxJob (g, j1), inferApxJob (g, j2))
      | (gg, Jwithctx (g, j)) ->
          let rec ia arg =
            (match arg with
                 (IntSyn.Null) -> (gg, IntSyn.Null) 
               | (IntSyn.Decl (g, tm)) ->
                    let (gg', g') = ia (g) in
                    let _ = clearDelayed () in
                    let (tm', d) = inferApxDec (gg', tm) in
                    let _ = runDelayed () in
                   (IntSyn.Decl (gg', d), IntSyn.Decl (g', tm')))
          in
          let (gg', g') = ia (g) in
          jwithctx (g', inferApxJob (gg', j))
      | (g, Jterm (tm)) ->
          let _ = clearDelayed () in
          let (tm', u, v, l) = inferApx (g, tm) in
          let _ = filterLevel (tm', l, 2,
                               "The term in this position must be an object or a type family") in
          let _ = runDelayed () in
          jterm (tm')
      | (g, Jclass (tm)) ->
          let _ = clearDelayed () in
          let l = Approx.newLVar () in
          let (tm', v) = checkApx (g, tm, Approx.Uni l, Approx.Next l,
                                   "The term in this position must be a type or a kind") in
          let _ = filterLevel (tm', Approx.Next l, 3,
                               "The term in this position must be a type or a kind") in
          let _ = runDelayed () in
          jclass (tm')
      | (g, Jof (tm1, tm2)) ->
          let _ = clearDelayed () in
          let l = Approx.newLVar () in
          let (tm2', v2) = checkApx (g, tm2, Approx.Uni l, Approx.Next l,
                                     "The term in this position must be a type or a kind") in
          let (tm1', u1) = checkApx (g, tm1, v2, l,
                                     "Ascription in declaration did not hold") in
          let _ = filterLevel (tm1', l, 2,
                               "The term in this position must be an object or a type family") in
          let _ = runDelayed () in
          jof (tm1', tm2')
      | (g, Jof' (tm1, v)) ->
          let _ = clearDelayed () in
          let l = Approx.newLVar () in
	  let (v2, _) = Approx.classToApx v in
          let (tm1', u1) = checkApx (g, tm1, v2, l,
                                     "Ascription in declaration did not hold") in
          let _ = filterLevel (tm1', l, 2,
                               "The term in this position must be an object or a type family") in
          let _ = runDelayed () in
          jof' (tm1', v)

    let rec ctxToApx arg =
      match arg with
        IntSyn.Null -> IntSyn.Null
      | (IntSyn.Decl (g, IntSyn.NDec x)) ->
          IntSyn.Decl (ctxToApx g, NDec x)
      | (IntSyn.Decl (g, IntSyn.Dec (name, v))) -> 
	    let (v', _) = Approx.classToApx v in
	    IntSyn.Decl (ctxToApx g, Dec (name, v'))

    let inferApxJob' (g, t) =
        inferApxJob (ctxToApx g, t)
 
  (* Final reconstruction job syntax *)

  type job' =
      JNothing
    | JAnd of job' * job'
    | JWithCtx of IntSyn.dec IntSyn.ctx * job'
    | JTerm of (IntSyn.exp * Paths.occExp) * IntSyn.exp * IntSyn.uni
    | JClass of (IntSyn.exp * Paths.occExp) * IntSyn.uni
    | JOf of (IntSyn.exp * Paths.occExp) * (IntSyn.exp * Paths.occExp) * IntSyn.uni
  
  (* This little datatype makes it easier to work with eta-expanded terms
     The idea is that Elim E represents a term U if
       E (s, S) = U[s] @ S *)

  type bidi =
      Elim of ((IntSyn.sub * IntSyn.spine) -> IntSyn.exp)
    | Intro of IntSyn.exp

  let elimSub (e, s) = (fun (s', ss) -> e (IntSyn.comp (s, s'), ss))
  let elimApp (e, u) = (fun (s, ss) -> e (s, IntSyn.App (IntSyn.EClo (u, s), ss)))

  let bvarElim (n) = (fun (s, ss) ->
      (match IntSyn.bvarSub (n, s) with
            IntSyn.Idx (n') -> IntSyn.Root (IntSyn.BVar n', ss)
          | IntSyn.Exp (u) -> IntSyn.Redex (u, ss)))
  let fvarElim (name, v, s) =
        (fun (s', ss) -> IntSyn.Root (IntSyn.FVar (name, v, IntSyn.comp (s, s')), ss))
  let redexElim (u) = (fun (s, ss) -> IntSyn.Redex (IntSyn.EClo (u, s), ss))
  (* headElim (H) = E
     assumes H not Proj _ *)
  let headElim arg =
    match arg with
        (IntSyn.BVar n) -> bvarElim n
      | (IntSyn.FVar(n,e,s)) -> fvarElim (n,e,s)
      | (IntSyn.NSDef d) -> redexElim (IntSyn.constDef d) 
      | (h) ->
        (match IntSyn.conDecStatus (headConDec h) with
            IntSyn.Foreign (csid, f) -> (fun (s, ss) -> f ss)
          | _ -> (fun (s, ss) -> IntSyn.Root (h, ss)))
  (* although internally EVars are lowered intro forms, externally they're
     raised elim forms.
     this conforms to the external interpretation:
     the type of the returned elim form is ([[G]] V) *)
  let evarElim ((IntSyn.EVar _) as x) =
        (fun (s, ss) -> IntSyn.EClo (x, Whnf.spineToSub (ss, s)))

  let rec etaExpandW args =
    match args with
      (e, (IntSyn.Pi (((IntSyn.Dec (_, va) as d), _), vr), s)) ->
        let u1 = etaExpand (bvarElim (1), (va, IntSyn.comp (s, IntSyn.shift))) in
        let d' = IntSyn.decSub (d, s) in
        IntSyn.Lam (d', etaExpand (elimApp (elimSub (e, IntSyn.shift), u1), (vr, IntSyn.dot1 s)))
    | (e, _) -> e (IntSyn.id, IntSyn.Nil)
  and etaExpand (e, vs) = etaExpandW (e, Whnf.whnfExpandDef vs)

  (* preserves redices *)
  let toElim arg =
    match arg with
      (Elim e) -> e
    | (Intro u) -> redexElim u

  let toIntro args =
    match args with
        (Elim e, vs) -> etaExpand (e, vs)
      | (Intro u, vs) -> u

  let rec addImplicit1W (g, e, (IntSyn.Pi ((IntSyn.Dec (_, va), _), vr), s), i (* >= 1 *)) =
        let x = Whnf.newLoweredEVar (g, (va, s)) in
        addImplicit (g, elimApp (e, x), (vr, Whnf.dotEta (IntSyn.Exp (x), s)), i-1)

      (* if no implicit arguments, do not expand Vs!!! *)
  and addImplicit args =
    match args with
        (g, e, (v,s), 0) -> (e, IntSyn.EClo (v,s))
      | (g, e, vs, i) -> addImplicit1W (g, e, Whnf.whnfExpandDef vs, i)


                          

  let delayMismatch (g, v1, v2, r2, location_msg, problem_msg) =
    addDelayed (fun () ->
      let xs = Abstract.collectEVars (g, (v2, IntSyn.id),
                 Abstract.collectEVars (g, (v1, IntSyn.id), [])) in
      let xnames = List.map (fun x -> (x, Names.evarName (IntSyn.Null, x))) xs in
(*
      let v1fmt = formatExp (g, v1) in
      let v2fmt = formatExp (g, v2) in
      let diff = F.Vbox0 0 1
                   [F.String "Expected:", F.Space, V2fmt, F.Break,
                    F.String "Inferred:", F.Space, V1fmt]
      in
      let diff = (case Print.evarCnstrsToStringOpt (Xnames)
                      of NONE => F.makestring_fmt diff
                       | SOME(cnstrs) => F.makestring_fmt diff ^
                                         "\nConstraints:\n" ^ cnstrs)
      in
*)
      error (r2, "Type mismatch\n"
(*                 ^ diff ^ "\n"
*)
                 ^ problem_msg ^ "\n"
                 ^ location_msg))


  let delayAmbiguous (g, u, r, msg) =
    addDelayed (fun () ->
(* no formatter yet
      let ufmt = formatExp (g, u) in
      let amb = Formatter.HVbox [Formatter.String "Inferred:", Formatter.Space, formatExp (g, u)] in
*)
        error (r, "Ambiguous reconstruction\n"
                 (* ^ Formatter.makestring_fmt amb ^ "\n" *)
                  ^ msg))

 let unifyIdem x =
        (* this reset should be unnecessary -- for safety only *)
(*   let _ = Unify.reset () in *)
   let _ = 
     try
       Unify.unify x
     with e ->
         ((*Unify.unwind*) ();
          raise e)
   in
(*   let  _ = Unify.reset () in *)
   ()

  let unifiableIdem x =
    (* this reset should be unnecessary -- for safety only *)
(*    let _ = Unify.reset () in *)
    let ok = Unify.unifiable x in
(*(* not implementing trails so hopefully ok to skip the rest. *) 
    let _ = if ok 
            then Unify.reset () 
            else Unify.unwind () in
*)
    ok
      

  (* tracing code *)

  type traceMode = Progressive | Omniscient
  let trace = ref false
  let traceMode = ref Omniscient

  let report f = 
    match !traceMode with 
        Progressive -> f ()
      | Omniscient -> addDelayed f

  let reportMismatch (g, vs1, vs2, problem_msg) =
    report (fun () ->
      let xs = Abstract.collectEVars (g, vs2,
                 Abstract.collectEVars (g, vs1, [])) in
      let xnames = List.map (fun x -> (x, Names.evarName (IntSyn.Null, x))) xs in
(*
	val eqnsFmt = F.HVbox [F.String "|?", F.Space, formatExp (G, EClo Vs1),
			       F.Break, F.String "=", F.Space, formatExp (G, EClo Vs2)]
	val _ = Msg.message (F.makestring_fmt eqnsFmt ^ "\n")
      let _ = reportConstraints Xnames in
*)
      let _ = Msg.message ("Failed: " ^ problem_msg ^ "\n"
                       ^ "Continuing with subterm replaced by _\n")
      in
        () )

  let reportUnify' (g, vs1, vs2) =
    let xs = Abstract.collectEVars (g, vs2,
             Abstract.collectEVars (g, vs1, [])) in
    let xnames = List.map (fun x -> (x, Names.evarName (IntSyn.Null, x))) xs in
(*    let eqnsFmt = F.HVbox [F.String "|?", F.Space, formatExp (G, EClo Vs1),
 		       F.Break, F.String "=", F.Space, formatExp (G, EClo Vs2)] in
    let _ = Msg.message (F.makestring_fmt eqnsFmt ^ "\n") in *)
    let _ = 
      try
        unifyIdem (g, vs1, vs2)
      with Unify.Unify msg ->
         (Msg.message ("Failed: " ^ msg ^ "\n"
                       ^ "Continuing with subterm replaced by _\n");
           raise (Unify.Unify msg))
    in
(*
    let _ = reportInst Xnames in
    let _ = reportConstraints Xnames in
*)
    ()

  let reportUnify (g, vs1, vs2) =
    match !traceMode with
        Progressive -> reportUnify' (g, vs1, vs2)
      | Omniscient ->
          try
            unifyIdem (g, vs1, vs2)
          with Unify.Unify msg ->
                   (reportMismatch (g, vs1, vs2, msg);
                    raise (Unify.Unify msg))

exception Match

    let rec inferExactN (g, tm) =
      match (g, tm) with
        (g, ExtSyn.Internal (u, v, r)) ->
            (tm, Intro u, v) 
      | (g, ExtSyn.Constant (h, r)) ->
          let cd = headConDec (h) in
          let (e, v) = addImplicit (g, headElim h, (IntSyn.conDecType cd, IntSyn.id), IntSyn.conDecImp cd) in
          (tm, Elim e, v)
      | (g, ExtSyn.Bvar (k, r)) ->
          let IntSyn.Dec (_, v) = IntSyn.ctxDec (g, k) in
          (tm, Elim (bvarElim k), v)
      | (g, ExtSyn.Evar (name, r)) ->
          (* externally EVars are raised elim forms *)
            let (x, v) = 
              try
                getEVar (name, false)
              with Approx.Ambiguous ->
                    let (x, v) = getEVar (name, true) in
                    delayAmbiguous (g, v, r, "Free variable has ambiguous type");
                    (x, v)
            in
            let s = IntSyn.Shift (IntSyn.ctxLength (g)) (* necessary? -kw *) in
            (tm, Elim (elimSub (evarElim x, s)), IntSyn.EClo (v, s))
      | (g, ExtSyn.Fvar (name, r)) ->
          let v = 
            try
              getFVarType (name, false)
            with Approx.Ambiguous ->
                    let v = getFVarType (name, true) in
                    delayAmbiguous (g, v, r, "Free variable has ambiguous type");
                    v
          in
          let s = IntSyn.Shift (IntSyn.ctxLength (g)) in(* necessary? -kw *)
          (tm, Elim (fvarElim (name, v, s)), IntSyn.EClo (v, s))
      | (g, ExtSyn.Typ (r)) ->
          (tm, Intro (IntSyn.Uni IntSyn.Type), IntSyn.Uni IntSyn.Kind)
      | (g, ExtSyn.Arrow (tm1, tm2)) ->
          let (tm1', b1, _ (* Uni Type *)) = inferExact (g, tm1) in
          let d = IntSyn.Dec (None, toIntro (b1, (IntSyn.Uni IntSyn.Type, IntSyn.id))) in
          let (tm2', b2, l) = inferExact (g, tm2) in
          let v2 = toIntro (b2, (l, IntSyn.id)) in
          (ExtSyn.Arrow (tm1', tm2'), Intro (IntSyn.Pi ((d, IntSyn.No), IntSyn.EClo (v2, IntSyn.shift))), l)
      | (g, ExtSyn.Pi (tm1, tm2)) ->
          let (tm1', d) = inferExactDec (g, tm1) in
          let (tm2', b2, l) = inferExact (IntSyn.Decl (g, d), tm2) in
          let v2 = toIntro (b2, (l, IntSyn.id)) in
          (ExtSyn.Pi (tm1', tm2'), Intro (IntSyn.Pi ((d, IntSyn.Maybe), v2)), l)
      | (g, ExtSyn.Lam (tm1, tm2)) ->
          let (tm1', d) = inferExactDec (g, tm1) in
          let (tm2', b2, v2) = inferExact (IntSyn.Decl (g, d), tm2) in
          let u2 = toIntro (b2, (v2, IntSyn.id)) in
          (ExtSyn.Lam (tm1', tm2'), Intro (IntSyn.Lam (d, u2)), IntSyn.Pi ((d, IntSyn.Maybe), v2))
      | (g, ExtSyn.App (tm1, tm2)) ->
          let (tm1', b1, v1) = inferExact (g, tm1) in
          let e1 = toElim (b1) in
          let (IntSyn.Pi ((IntSyn.Dec (_, va), _), vr), s) = Whnf.whnfExpandDef (v1, IntSyn.id) in
          let (tm2', b2) = checkExact (g, tm2, (va, s),
                                       "Argument type did not match function domain type\n(Index object(s) did not match)") in
          let u2 = toIntro (b2, (va, s)) in
          (ExtSyn.App (tm1', tm2'), Elim (elimApp (e1, u2)), IntSyn.EClo (vr, Whnf.dotEta (IntSyn.Exp u2, s)))
      | (g, ExtSyn.Hastype (tm1, tm2)) ->
          let (tm2', b2, l) = inferExact (g, tm2) in
          let v = toIntro (b2, (l, IntSyn.id)) in
          let (tm1', b1) = checkExact (g, tm1, (v, IntSyn.id),
                                      "Ascription did not hold\n(Index object(s) did not match)") in
          (ExtSyn.Hastype (tm1', tm2'), b1, v)
      | (g, ExtSyn.Mismatch (tm1, tm2, location_msg, problem_msg)) ->
          let (tm1', _, v1) = inferExact (g, tm1) in
          let (tm2', b, v) = inferExactN (g, tm2) in
          let _ = if !trace then reportMismatch (g, (v1, IntSyn.id), (v, IntSyn.id), problem_msg) else () in
          let _ = delayMismatch (g, v1, v, termRegion tm2', location_msg, problem_msg) in
          (ExtSyn.Mismatch (tm1', tm2', location_msg, problem_msg), b, v)
      | (g, ExtSyn.Omitapx (u, v, l, r)) ->
          let v' = 
            try
              Approx.apxToClass (g, v, l, false)
            with Approx.Ambiguous ->
                     let v' = Approx.apxToClass (g, v, l, true) in
                     delayAmbiguous (g, v', r, "Omitted term has ambiguous " ^
                       (match Approx.whnfUni l with
                             Approx.Level 1 -> "type"
                           | Approx.Level 2 -> "kind"
                             (* yes, this can happen in pathological cases, e.g.
                                  a : type. b = a : _ _. *)
                             (* FIX: this violates an invariant in printing *)
                           | Approx.Level 3 -> "hyperkind"));
                       v'
          in
          let u' = 
            try
              Approx.apxToExact (g, u, (v', IntSyn.id), false)
            with Approx.Ambiguous ->
                     let u' = Approx.apxToExact (g, u, (v', IntSyn.id), true) in
                     delayAmbiguous (g, u', r, "Omitted " ^
                       (match Approx.whnfUni l with
                             Approx.Level 2 -> "type"
                           | Approx.Level 3 -> "kind") ^ " is ambiguous");
                     u'
          in
          (ExtSyn.Omitexact (u', v', r), Intro u', v')

    and inferExact (g, tm) = inferExactN (g, tm)

    and inferExactDec (g, Dec (name, tm, r)) =
          let (tm', b1, _ (* Uni Type *)) = inferExact (g, tm) in
          let v1 = toIntro (b1, (IntSyn.Uni IntSyn.Type, IntSyn.id)) in
          let d = IntSyn.Dec (name, v1) in
          (ExtSyn.Dec (name, tm', r), d)

    and checkExact1 args =
      match args with
        (g, ExtSyn.Lam (ExtSyn.Dec (name, tm1, r), tm2), vhs) ->
          let (IntSyn.Pi ((IntSyn.Dec (_, va), _), vr), s) = Whnf.whnfExpandDef vhs in
          let ((tm1', b1, _ (* Uni Type *)), ok1) = unifyExact (g, tm1, (va, s)) in
          let v1 = toIntro (b1, (IntSyn.Uni IntSyn.Type, IntSyn.id)) in
          let d = IntSyn.Dec (name, v1) in
          let ((tm2', b2, v2), ok2) =
                if ok1 then checkExact1 (IntSyn.Decl (g, d), tm2, (vr, IntSyn.dot1 s))
                else (inferExact (IntSyn.Decl (g, d), tm2), false)
          in
          let u2 = toIntro (b2, (v2, IntSyn.id)) in
          ((ExtSyn.Lam (ExtSyn.dec (name, tm1', r), tm2'), Intro (IntSyn.Lam (d, u2)), IntSyn.Pi ((d, IntSyn.Maybe), v2)), ok2)
      | (g, ExtSyn.Hastype (tm1, tm2), vhs) ->
          let ((tm2', b2, l), ok2) = unifyExact (g, tm2, vhs) in
          let v = toIntro (b2, (l, IntSyn.id)) in
          let (tm1', b1) = checkExact (g, tm1, (v, IntSyn.id),
                                       "Ascription did not hold\n(Index object(s) did not match)") in
          ((ExtSyn.Hastype (tm1', tm2'), b1, v), ok2)
      | (g, ExtSyn.Mismatch (tm1, tm2, location_msg, problem_msg), vhs) ->
          let (tm1', _, v1) = inferExact (g, tm1) in
          let ((tm2', b, v), ok2) = checkExact1 (g, tm2, vhs) in
          let _ = delayMismatch (g, v1, v, termRegion tm2', location_msg, problem_msg) in
          ((ExtSyn.Mismatch (tm1', tm2', location_msg, problem_msg), b, v), ok2)
      | (g, ExtSyn.Omitapx (u, v (* = vhs *), l, r), vhs) ->
          let (vhs1, vhs2) = vhs in
          let v' = IntSyn.EClo (vhs1, vhs2) in
          let u' = 
            try
              Approx.apxToExact (g, u, vhs, false) 
            with Approx.Ambiguous ->
                     let u' = Approx.apxToExact (g, u, vhs, true) in
                     delayAmbiguous (g, u', r, "Omitted " ^
                       (match Approx.whnfUni l with
                             Approx.Level 2 -> "type"
                           | Approx.Level 3 -> "kind") ^ " is ambiguous");
                     u'
          in
          ((ExtSyn.Omitexact (u', v', r), Intro u', v'), true)
      | (g, tm, vhs) ->
          let (tm', b', v') = inferExact (g, tm) in
          ((tm', b', v'), unifiableIdem (g, vhs, (v', IntSyn.id)))

    and checkExact (g, tm, vs, location_msg) =
        if not (!trace) then
          let ((tm', b', v'), ok) = checkExact1 (g, tm, vs) in
          if ok then (tm', b') 
          else 
            try
              ((unifyIdem (g, (v', IntSyn.id), vs));
              raise Match (* can't happen *))
            with Unify.Unify problem_msg ->
              let r = termRegion tm in
              let u' = toIntro (b', (v', IntSyn.id)) in
              let (uapx, vapx, lapx) = Approx.exactToApx (u', v') in
              let ((tm'', b'', _ (* vs *)), _ (* true *)) =
                    checkExact1 (g, ExtSyn.Omitapx (uapx, vapx, lapx, r), vs) in
              let _ = delayMismatch (g, v', IntSyn.EClo (fst vs, snd vs), r, location_msg, problem_msg) in
              (ExtSyn.Mismatch (tm', tm'', location_msg, problem_msg), b'')

        else
          let (tm', b', v') = inferExact (g, tm) in
          try
            (reportUnify (g, (v', IntSyn.id), vs); (tm', b'))
          with Unify.Unify problem_msg ->
            let r = termRegion tm in
            let u' = toIntro (b', (v', IntSyn.id)) in
            let (uapx, vapx, lapx) = Approx.exactToApx (u', v') in
            let (tm'', b'') =
                  checkExact (g, ExtSyn.Omitapx (uapx, vapx, lapx, r), vs, location_msg) in
            let _ = delayMismatch (g, v', IntSyn.EClo(fst vs, snd vs), r, location_msg, problem_msg) in
            (ExtSyn.Mismatch (tm', tm'', location_msg, problem_msg), b'')

    and unifyExact args =
      match args with
        (g, ExtSyn.Arrow (tm1, tm2), vhs) ->
          let (IntSyn.Pi ((IntSyn.Dec (_, va), _), vr), s) = Whnf.whnfExpandDef vhs in
          let ((tm1', b1, _ (* Uni Type *)), ok1) = unifyExact (g, tm1, (va, s)) in
          let v1 = toIntro (b1, (IntSyn.Uni IntSyn.Type, IntSyn.id)) in
          let d = IntSyn.Dec (None, v1) in
          let (tm2', b2, l) = inferExact (g, tm2) in
          let v2 = toIntro (b2, (l,IntSyn.id)) in
          ((ExtSyn.Arrow (tm1', tm2'), Intro (IntSyn.Pi ((d, IntSyn.No), IntSyn.EClo (v2, IntSyn.shift))), l),
           ok1 && unifiableIdem (Decl (g, d), (vr, IntSyn.dot1 s), (v2, IntSyn.shift)))
      | (g, ExtSyn.Pi (Dec (name, tm1, r), tm2), vhs) ->
          let (IntSyn.Pi ((IntSyn.Dec (_, va), _), vr), s) = Whnf.whnfExpandDef vhs in
          let ((tm1', b1, _ (* Uni Type *)), ok1) = unifyExact (g, tm1, (va, s)) in
          let v1 = toIntro (b1, (IntSyn.Uni IntSyn.Type, IntSyn.id)) in
          let d = IntSyn.Dec (name, v1) in
          let ((tm2', b2, l), ok2) =
                if ok1 then unifyExact (Decl (g, d), tm2, (vr, IntSyn.dot1 s))
                else (inferExact (Decl (g, d), tm2), false) in
          let v2 = toIntro (b2, (l, IntSyn.id)) in
          ((ExtSyn.Pi (ExtSyn.Dec (name, tm1', r), tm2'), Intro (IntSyn.Pi ((d, IntSyn.Maybe), v2)), l), ok2)
        (* lam impossible *)
      | (g, ExtSyn.Hastype (tm1, tm2), vhs) ->
          (* vh : L by invariant *)
          let (tm2', _ (* Uni L *), _ (* Uni (Next L) *)) = inferExact (g, tm2) in
          let ((tm1', b, l), ok1) = unifyExact (g, tm1, vhs) in
          ((ExtSyn.Hastype (tm1', tm2'), b, l), ok1)
      | (g, ExtSyn.Mismatch (tm1, tm2, location_msg, problem_msg), vhs) ->
          let (tm1', _, l1) = inferExact (g, tm1) in
          let ((tm2', b, l), ok2) = unifyExact (g, tm2, vhs) in
          let _ = delayMismatch (g, l1, l, termRegion tm2', location_msg, problem_msg) in
          ((ExtSyn.Mismatch (tm1', tm2', location_msg, problem_msg), b, l), ok2)
      | (g, ExtSyn.Omitapx (v (* = vhs *), l, nL (* Next L *), r), vhs) ->
          (* cannot raise Ambiguous *)
          let l' = Approx.apxToClass (g, l, nL, false) in
          let v' = IntSyn.EClo(fst vhs, snd vhs) in
          ((ExtSyn.Omitexact (v', l', r), Intro v', l'), true)
      | (g, tm, vhs) -> 
          let (tm', b', l') = inferExact (g, tm) in
          let v' = toIntro (b', (l', IntSyn.id)) in
          ((tm', b', l'), unifiableIdem (g, vhs, (v', IntSyn.id)))

    let rec occElim args =
      match args with
        (ExtSyn.Constant (h, r), os, rs, i) ->
          (* should probably treat a constant with Foreign
             attribute as a redex *)
          let r' = List.fold_right (fun a b -> Paths.join (a,b)) rs r in
          (Paths.Root (r', Paths.Leaf r, IntSyn.conDecImp (headConDec h), i, os), r')
      | (ExtSyn.Bvar (k, r), os, rs, i) ->
          let r' = List.fold_right (fun a b -> Paths.join (a,b)) rs r in
          (Paths.Root (r', Paths.Leaf r, 0, i, os), r')
      | (ExtSyn.Fvar (name, r), os, rs, i) ->
          let r' = List.fold_right (fun a b -> Paths.join (a,b)) rs r in
          (Paths.Root (r', Paths.Leaf r, 0, i, os), r')
      | (ExtSyn.App (tm1, tm2), os, rs, i) ->
          let (oc2, r2) = occIntro tm2 in
          occElim (tm1, Paths.App (oc2, os), r2::rs, i+1)
      | (ExtSyn.Hastype (tm1, tm2), os, rs, i) -> occElim (tm1, os, rs, i)
      | (tm, os, rs, i) ->
        (* this is some kind of redex or evar-under-substitution
           also catches simple introduction forms like `type' *)
          let r' = List.fold_right (fun a b -> Paths.join (a,b)) rs (termRegion tm) in
          (Paths.Leaf r', r')

    and occIntro arg =
      match arg with 
        (ExtSyn.Arrow (tm1, tm2)) ->
          let (oc1, r1) = occIntro tm1 in
          let (oc2, r2) = occIntro tm2 in
          let r' = Paths.join (r1, r2) in
          (Paths.Bind (r', Some oc1, oc2), r')
      | (ExtSyn.Pi (Dec (name, tm1, r), tm2)) ->
          let (oc1, r1) = occIntro tm1 in
          let (oc2, r2) = occIntro tm2 in
          let r' = Paths.join (r, r2) in
          (* not quite consistent with older implementation for dec0 *)
          (Paths.Bind (r', Some oc1, oc2), r')
      | (ExtSyn.Lam (Dec (name, tm1, r), tm2)) ->
          let (oc1, r1) = occIntro tm1 in
          let (oc2, r2) = occIntro tm2 in
          let r' = Paths.join (r, r2) in
          (* not quite consistent with older implementation for dec0 *)
          (Paths.Bind (r', Some oc1, oc2), r')
      | (ExtSyn.Hastype (tm1, tm2)) -> occIntro tm1
      | (tm) ->
          (* still doesn't work quite right for the location -> occurrence map? *)
          let (oc, r) = occElim (tm, Paths.Nils, [], 0) in
          (oc, r)
 
    let rec inferExactJob args =
      match args with
          (g, Jnothing) -> JNothing
      | (g, Jand (j1, j2)) ->
          JAnd (inferExactJob (g, j1), inferExactJob (g, j2))
      | (gg, Jwithctx (g, j)) ->
          let rec ie a =
            (match a with
               (IntSyn.Null) -> (gg, IntSyn.Null)
             | (IntSyn.Decl (g, tm)) ->
                 let (gg', gresult) = ie (g) in
                 let (_, d) = inferExactDec (gg', tm) in
                 (Decl (gg', d), Decl (gresult, d)))
          in
          let (gg', gresult) = ie (g) in
          JWithCtx (gresult, inferExactJob (gg', j))
      | (g, Jterm (tm)) ->
          let (tm', b, v) = inferExact (g, tm) in
          let u = toIntro (b, (v, IntSyn.id)) in
          let (oc, r) = occIntro (tm') in
          let rec iu a =
            (match a with
               (IntSyn.Uni IntSyn.Type) -> IntSyn.Kind
             | (IntSyn.Pi (_, v)) -> iu v
             | (IntSyn.Root _) -> IntSyn.Type
             | (IntSyn.Redex (v, _)) -> iu v
             | (IntSyn.Lam (_, v)) -> iu v
             | (IntSyn.EClo (v, _)) -> iu v )
              (* others impossible *)
          in
          JTerm ((u, oc), v, iu v)
      | (g, Jclass (tm)) ->
          let (tm', b, l) = inferExact (g, tm) in
          let v = toIntro (b, (l, IntSyn.id)) in
          let (oc, r) = occIntro (tm') in
          let (IntSyn.Uni l, _) = Whnf.whnf (l, IntSyn.id) in
          JClass ((v, oc), l)
      | (g, Jof (tm1, tm2)) ->
          let (tm2', b2, l2) = inferExact (g, tm2) in
          let v2 = toIntro (b2, (l2, IntSyn.id)) in
          let (tm1', b1) = checkExact (g, tm1, (v2, IntSyn.id),
                                       "Ascription in declaration did not hold\n"
                                       ^ "(Index object(s) did not match)") in
          let u1 = toIntro (b1, (v2, IntSyn.id)) in
          let (oc2, r2) = occIntro tm2' in
          let (oc1, r1) = occIntro tm1' in
          let (IntSyn.Uni l2, _) = Whnf.whnf (l2, IntSyn.id) in
          JOf ((u1, oc1), (v2, oc2), l2)
      | (g, Jof' (tm1, v2)) ->
(*          let (tm2', b2, l2) = inferExact (g, tm2)
          let v2 = toIntro (b2, (l2, id)) *)
          let (tm1', b1) = checkExact (g, tm1, (v2, IntSyn.id),
                                       "Ascription in declaration did not hold\n"
                                       ^ "(Index object(s) did not match)") in
          let u1 = toIntro (b1, (v2, IntSyn.id)) in
(*          let (oc2, r2) = occIntro tm2' *)
          let (oc1, r1) = occIntro tm1' in
(*          let (IntSyn.Uni l2, _) = Whnf.whnf (l2, IntSyn.id) *)
          JOf ((u1, oc1), (v2, oc1), IntSyn.Type)

    let recon' (j) =
          (* we leave it to the context to call Names.varReset
             reason: this code allows reconstructing terms containing
             existing EVars, and future developments might use that *)
          (* context must already have called resetErrors *)
          let _ = Approx.varReset () in
          let _ = varReset () in
          let j' = inferApxJob (IntSyn.Null, j) in
          let _ = clearDelayed () in
          let j'' = inferExactJob (IntSyn.Null, j') in
          let _ = runDelayed () in
          (* we leave it to the context to call checkErrors
             reason: the caller may want to do further processing on
             the "best effort" result returned, even if there were
             errors *)
          j''

    let recon (j) = (queryMode := false; recon' j)
    let reconQuery (j) = (queryMode := true; recon' j)
