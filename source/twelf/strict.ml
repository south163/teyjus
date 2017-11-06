exception Error of string

        (* Definition of normal form (nf) --- see lambda/whnf.fun *)

    (* patSpine (k, S) = B
       
       Invariant: 
       If  G, D |- S : V > V', S in nf
       and |D| = k
       then B iff S = (k1 ; k2 ;...; kn ; NIL), kn <= k, all ki pairwise distinct
    *)
let rec patSpine args =
  match args with
    (_, IntSyn.Nil) -> true
  | (k, IntSyn.App (IntSyn.Root (IntSyn.BVar (k'), IntSyn.Nil), s)) ->
        (* possibly eta-contract? -fp *)
    let rec indexDistinct a =
      (match a with
         (IntSyn.Nil) -> true
       | (IntSyn.App (IntSyn.Root (IntSyn.BVar (k''), IntSyn.Nil), s)) ->
	          k' <> k'' && indexDistinct s
       | _ -> false)
    in 
    k' <= k && patSpine (k, s) && indexDistinct s 
  | _ -> false

    (* strictExp (k, p, U) = B 
       
       Invariant:
       If  G, D |- U : V
       and U is in nf (normal form)
       and |D| = k
       then B iff U is strict in p
    *)
let rec strictExp args =
  match args with
    (_, _, IntSyn.Uni _) -> false
  | (k, p, IntSyn.Lam (d, u)) ->
          (* checking D in this case might be redundant -fp *)
          strictDec (k, p, d) || strictExp (k+1, p+1, u)
  | (k, p, IntSyn.Pi ((d, _), u)) ->
	  strictDec (k, p, d) || strictExp (k+1, p+1, u)
  | (k, p, IntSyn.Root (h, s)) ->
    (match h with
        (IntSyn.BVar (k')) -> 
          if (k' = p) then patSpine (k, s)
          else if (k' <= k) then strictSpine (k, p, s)
	  else false
      | (IntSyn.Const (c)) -> strictSpine (k, p, s)
      | (IntSyn.Def (d))  -> strictSpine (k, p, s)
      | (IntSyn.FgnConst (cs, conDec)) -> strictSpine (k, p, s))
	      (* no other cases possible *)
  | (k, p, IntSyn.FgnExp (cs, ops)) -> false
          (* this is a hack - until we investigate this further   -rv *)                        
    (* no other cases possible *)

    (* strictSpine (k, S) = B 
       
       Invariant:
       If  G, D |- S : V > W
       and S is in nf (normal form)
       and |D| = k
       then B iff S is strict in k
    *)
and strictSpine args =
  match args with
    (_, _, IntSyn.Nil) -> false
  | (k, p, IntSyn.App (u, s)) ->
    strictExp (k, p, u) || strictSpine (k, p, s)
      
and strictDec (k, p, IntSyn.Dec (_, v)) =
  strictExp (k, p, v)
    

    (* strictArgParm (p, U) = B

       Traverses the flexible abstractions in U.
       
       Invariant:
       If   G |- U : V
       and  G |- p : V'
       and  U is in nf
       then B iff argument parameter p occurs in strict position in U
	          which starts with argument parameters
    *)
let rec strictArgParm args =
  match args with
    (p, (IntSyn.Root _ as u)) -> strictExp (0, p, u)
  | (p, (IntSyn.Pi _ as u)) -> strictExp (0, p, u)
  | (p, (IntSyn.FgnExp _ as u)) -> strictExp (0, p, u)
  | (p, IntSyn.Lam (d, u)) -> strictArgParm (p+1, u)

let occToString args =
  match args with
    (Some(ocd), occ) -> Paths.wrap (Paths.occToRegionDef1 ocd occ, "")
  | (None, occ) -> "Error: "

let decToVarName args =
  match args with
    (IntSyn.Dec (None, _)) -> "implicit variable"
  | (IntSyn.Dec (Some(x), _)) -> "variable " ^ x

    (* strictTop ((U, V), ocdOpt) = ()
       
       Invariant:
       condec has form c = U : V where . |- U : V
       and U is in nf (normal form)
       then function returns () if U every argument parameter of U
	    has at least one strict and rigid occurrence in U
       raises Error otherwise

       ocdOpt is an optional occurrence tree for condec for error messages
    *)
let strictTop ((u, v), ocdOpt) =
  let rec strictArgParms args =
    match args with
      (IntSyn.Root (IntSyn.BVar _, _), _, occ) ->
                raise (Error (occToString (ocdOpt, occ) ^ "Head not rigid, use %abbrev"))
    | (IntSyn.Root _, _, _) -> ()
    | (IntSyn.Pi _, _, _) -> ()
    | (IntSyn.FgnExp _, _, _) -> ()
            	  (* may not be sound in general *)
		  (* Wed Aug 25 16:39:57 2004 -fp *)
    | (IntSyn.Lam (d, u'), IntSyn.Pi (_, v'), occ) -> 
      if strictArgParm (1, u')
      then strictArgParms (u', v', Paths.OccBody occ)
      else raise (Error (occToString (ocdOpt, occ)
                                  ^ "No strict occurrence of " ^ decToVarName d ^ ", use %abbrev"))
    | ((IntSyn.Lam _ as u), (IntSyn.Root (IntSyn.Def _, _) as v), occ) ->
      strictArgParms (u, Whnf.normalize (Whnf.expandDef (v, IntSyn.id)), occ)
  in
  strictArgParms (u, v, Paths.OccTop)

let occursInType ((i, v), ocdOpt) =
  let rec oit args =
    match args with
      ((0, v), occ) -> ()
    | ((i, IntSyn.Pi((d,p), v)), occ) ->
      (match Abstract.piDepend ((d,p), v) with
        IntSyn.Pi ((d', IntSyn.Maybe), v) -> oit ((i-1, v), Paths.OccBody occ)
      | _ -> raise (Error (occToString (ocdOpt, occ)
			  ^ "No occurrence of " ^ decToVarName d ^ " in type, use %abbrev")))
    | _ -> ()
  in
  oit ((i, v), Paths.OccTop)
 


let check = strictTop

let checkType = occursInType
