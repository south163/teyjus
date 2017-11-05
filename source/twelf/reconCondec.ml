(* This is taken from the Twelf implementation *) 
  exception Error of string

  (* error (r, msg) raises a syntax error within region r with text msg *)
  let error (r, msg) = raise (Error (Paths.wrap (r, msg)))

    (* condecToConDec (condec, r) = (SOME(cd), SOME(ocd))
     if condec is a named constant declaration with occurrence tree ocd,
     NONE if name or occurrence tree is missing

     Free variables in condec are interpreted universally (as FVars)
     then abstracted as implicit parameters.

     Only works properly when the declaration contains no EVars.
  *)
  (* should printing of result be moved to frontend? *)
  (* Wed May 20 08:08:50 1998 -fp *)
  let condecToConDec c =
    match c with 
        (ExtConDec.Condec(name, tm), Paths.Loc (fileName, r), abbFlag) ->
          let _ = Names.varReset IntSyn.Null in
	  let _ = ReconTerm.resetErrors fileName in 
          let ReconTerm.JClass ((v, oc), l) =
(* Not implementing timers yet so call reconstruction directly *)
(*              (Timers.time Timers.recon ReconTerm.recon) (ReconTerm.jclass tm) in *)
            ReconTerm.recon (ReconTerm.jclass tm) in
	  let _ = ReconTerm.checkErrors (r) in
(* again, not implementing timers yet *)
(*          let (i, v') = try (Timers.time Timers.abstract Abstract.abstractDecImp) v
	                with Abstract.Error (msg)
			       -> raise Abstract.Error (Paths.wrap (r, msg)) in *)
          let (i, v') = try Abstract.abstractDecImp v
                        with Abstract.Error (msg) -> raise (Abstract.Error (Paths.wrap (r, msg))) in
	  let cd = Names.nameConDec (IntSyn.ConDec (name, None, i, IntSyn.Normal, v', l)) in
	  let ocd = Paths.Dec (i, oc) in
(*	  let _ = if !Global.chatter >= 3
		  then Msg.message ((Timers.time Timers.printing Print.conDecToString) cd ^ "\n")
		else () in *)
(* no printing ... yet! *)
(*          let _ = if !Gloval.chatter >= 3
                  then Msg.message ((Print.conDecToString cd) ^ "\n") *)
(* Not implementing the extra type checking pass yet *)
(*	  let _ = if !Global.doubleCheck
		  then (Timers.time Timers.checking TypeCheck.check) (V', IntSyn.Uni L)
		else () in *)
	  (Some(cd), Some(ocd))
      | (ExtConDec.Condef(optName, tm1, tm2Opt), Paths.Loc (fileName, r), abbFlag) ->
          let _ = Names.varReset IntSyn.Null in
	  let _ = ReconTerm.resetErrors fileName in
          let f = (match tm2Opt with
                      None -> ReconTerm.jterm tm1
                    | Some tm2 -> ReconTerm.jof (tm1, tm2)) in
          let f' = (* Timers.time Timers.recon *)ReconTerm.recon f in
          let ((u, oc1), (v, oc2Opt), l) =
               (match f' with
                  ReconTerm.JTerm ((u, oc1), v, l) ->
                    ((u, oc1), (v, None), l)
                | ReconTerm.JOf ((u, oc1), (v, oc2), l) ->
                    ((u, oc1), (v, Some oc2), l))
          in
          let _ = ReconTerm.checkErrors (r) in
 (*
	  let (i, (U'', V'')) =
	        (Timers.time Timers.abstract Abstract.abstractDef) (U, V)
		handle Abstract.Error (msg)
		          => raise Abstract.Error (Paths.wrap (r, msg)) in 
 *)
      	  let (i, (u'', v'')) =
            try
              Abstract.abstractDef (u, v)
	    with Abstract.Error (msg)
		          -> raise (Abstract.Error (Paths.wrap (r, msg))) in 
          let name = match optName with None -> "_" | Some(name) -> name in
	  let ocd = Paths.Def (i, oc1, oc2Opt) in
          let cd = if abbFlag then Names.nameConDec (IntSyn.AbbrevDef (name, None, i, u'', v'', l))
		 else (Strict.check ((u'', v''), Some(ocd));
		       (* stricter checking of types according to Chris Richards Fri Jul  2 16:33:46 2004 -fp *)
		       (* (case optName of NONE => () | _ => Strict.checkType ((i, V''), SOME(ocd))); *)
		       (Names.nameConDec (IntSyn.ConDef (name, None, i, u'', v'', l,
							 IntSyn.ancestor u''))))
	  in
          (*
          let _ = if !Global.chatter >= 3
		  then Msg.message ((Timers.time Timers.printing Print.conDecToString) cd ^ "\n")
	          else () in
	  let _ = if !Global.doubleCheck
		  then ((Timers.time Timers.checking TypeCheck.check) (V'', IntSyn.Uni L);
			(Timers.time Timers.checking TypeCheck.check) (U'', V''))
		  else () in
          *)
  	  let optConDec = match optName with None -> None | Some _ -> Some (cd) in
	  (optConDec, Some(ocd))
