(** Implements the strictness check for translating LF types. *)

(** Check that the argument list contains only unique bound 
    variables. *)
let rec check_args tmlist bndrlist =
  let same_id id1 id2 = Symb.id id1 = Symb.id id2 in
  let match_term tm1 tm2 = 
     match tm1, tm2 with
         Lfabsyn.IdTerm(id1), Lfabsyn.IdTerm(id2)
       | Lfabsyn.IdTerm(id1), Lfabsyn.AppTerm(id2,[])
       | Lfabsyn.AppTerm(id1,[]), Lfabsyn.IdTerm(id2)
       | Lfabsyn.AppTerm(id1,[]), Lfabsyn.AppTerm(id2,[]) -> same_id (Lfabsyn.get_id_symb id1) (Lfabsyn.get_id_symb id2)
       | _ -> false
  in
  match tmlist with
      [] -> true
    | ((Lfabsyn.IdTerm(id) as tm) :: tms)
    | ((Lfabsyn.AppTerm(id,[]) as tm) :: tms) -> 
         (not (List.exists (match_term tm) tms)) && (List.exists (same_id (Lfabsyn.get_id_symb id)) bndrlist)
                                 && (check_args tms bndrlist)
    | _ -> false

(** Look for strict occurences of the given variable in the given 
    type. Collects bound variables in third argument. *)
let rec appears_strict_ty s ty bndrs =
  match ty with
      Lfabsyn.PiType(name,typ,body,dep) ->
        (appears_strict_ty s typ bndrs) || 
        (appears_strict_ty s body (name :: bndrs))
    | Lfabsyn.AppType(name, tms) ->
        let check =
          if Symb.id (Lfabsyn.get_id_symb name) = (Symb.id s)
          then check_args tms bndrs
          else false
        in
        check ||
          (List.exists (fun x -> appears_strict_tm s x bndrs) tms)
    | Lfabsyn.IdType(name) ->
        match (name, s) with
             (Lfabsyn.Var(v1,_), s) 
           | (Lfabsyn.LogicVar(v1,_), s) when (Symb.id v1 = Symb.id s) ->
              true
          | _ ->
              false

(** Look for strict occurences of the given variable in the given 
    term. Collects bound variables in third argument. *)
and appears_strict_tm s tm bndrs =
  match tm with
      Lfabsyn.AbsTerm(s',ty,body) ->
        appears_strict_tm s body (s' :: bndrs)
    | Lfabsyn.AppTerm(head,tms) ->
        let check =
          if Symb.id (Lfabsyn.get_id_symb head) = (Symb.id s)
          then check_args tms bndrs
          else false
        in
        check ||
          (List.exists (fun x -> appears_strict_tm s x bndrs) tms)
    | Lfabsyn.IdTerm(name) ->
        match (name, s) with
             (Lfabsyn.Var(v1,_), s) 
           | (Lfabsyn.LogicVar(v1,_), s) when (Symb.id v1 = Symb.id s) ->
               true
           | _ ->
             false

(** Checks if the given variable appears strictly in the given
    type. *)
let appears_strict s ty =
        appears_strict_ty s ty []
      
