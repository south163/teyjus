(* use ocaml formatting to print LF terms more nicely *)

let imp = "->"
let typekwd = "type"

let kwd ppf s = Format.fprintf ppf "%s" s

(*** These first functions will print LF terms in fully explicit form.
     They do not use the signature to determine implicit arguments or fixity. ***)

let pr_symb ppf s = Format.fprintf ppf "%s" (Symb.name s)
  
let pr_id ppf = function
  | Lfabsyn.Const (s) 
  | Lfabsyn.Var (s,_) 
  | Lfabsyn.LogicVar (s, _) -> pr_symb ppf s

let rec pr_term ppf = function
  | Lfabsyn.AbsTerm (s, Lfabsyn.Unknown, tm) ->
      Format.fprintf ppf "@[<2>[@,%a@,]@ %a@]"
                     pr_symb s pr_term tm
  | Lfabsyn.AbsTerm (s, ty, tm) ->
      Format.fprintf ppf "@[<2>[@,%a@,:@,%a@,]@ %a@]"
                     pr_symb s pr_typ ty pr_term tm
  | Lfabsyn.AppTerm(id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id pr_terms tms
  | Lfabsyn.IdTerm(id) -> pr_id ppf id

and pr_term' ppf = function
  | Lfabsyn.IdTerm (id) -> pr_id ppf id
  | t ->
     Format.fprintf ppf "@[<2>(%a)@]" pr_term t
                        
and pr_terms ppf = function
  | (tm :: tms) ->
      Format.fprintf ppf "@ %a%a" pr_term' tm pr_terms tms
  | _ -> ()

and pr_typ ppf = function
  | Lfabsyn.PiType (s, tyl, tyr,dep) ->
    if dep
    then
      Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
                     pr_symb s pr_typ tyl pr_typ tyr
    else
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]" pr_typ' tyl kwd imp pr_typ tyr
  | Lfabsyn.AppType (id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id pr_terms tms
  | Lfabsyn.IdType (id) -> pr_id ppf id

and pr_typ' ppf = function
  | Lfabsyn.IdType (id) -> pr_id ppf id
  | t -> Format.fprintf ppf "@[<2>(%a)@]" pr_typ t

let rec pr_kind ppf = function
  | Lfabsyn.PiKind (s, ty, k,dep) ->
    if dep
    then
      Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
                     pr_symb s pr_typ ty pr_kind k
    else
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]" pr_typ' ty kwd imp pr_kind k
  | Lfabsyn.Type -> Format.fprintf ppf "%a" kwd typekwd

let pr_typefam ppf (Lfabsyn.TypeFam (s, k, _, _, _, _, _)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_symb s pr_kind k

let pr_obj ppf (Lfabsyn.Object (s, ty, _, _, _, _)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_symb s pr_typ ty

let pr_query ppf (Lfabsyn.Query (_, s, ty)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_symb s pr_typ ty
    
let pr_query_explicit ppf (Lfabsyn.Query (fvars, s, ty)) =
  (** Print the logic variables along with their types **)
  let rec pr_qvars ppf = function
    | ((s,ty) :: vars) -> Format.fprintf ppf "%a :@ %a.@,%a"
      pr_symb s
      pr_typ ty
      pr_qvars vars
    | _ -> ()
  in
  Format.fprintf ppf "@[<4>@[<2>%a@]@,%a :@ %a@]" pr_qvars fvars pr_symb s pr_typ ty

let pr_subst ppf (s, tm) =
  Format.fprintf ppf "@[<4>%a =@ %a@]" pr_symb s pr_term tm

let pr_dispr ppf (tml, tmr) =
  Format.fprintf ppf "@[<4><%a,@ %a>@]" pr_term tml pr_term tmr

let rec pr_substs ppf = function
  | (s :: ss) -> Format.fprintf ppf "@.%a%a" pr_subst s pr_substs ss
  | _ -> ()

let rec pr_disprs ppf = function
  | (d :: dd) -> Format.fprintf ppf "@.%a%a" pr_dispr d pr_disprs dd
    
let pr_solution ppf (subst, disprs) =
  Format.fprintf ppf "@[<v 0>%a@.%a@]" pr_substs subst pr_disprs disprs

  

(* use general formmatting functions to print to stdout *)
let print_id = pr_id Format.std_formatter 
let print_term = pr_term Format.std_formatter 
let print_typ = pr_typ Format.std_formatter 
let print_kind = pr_kind Format.std_formatter
let print_typefam = pr_typefam Format.std_formatter
let print_obj = pr_obj Format.std_formatter
let print_query = pr_query Format.std_formatter
let print_solution = pr_solution Format.std_formatter
let print_query_explicit = pr_query_explicit Format.std_formatter
  
(* use general formatting functions to generate a string *)
let string_of_id id =
  pr_id Format.str_formatter id;
  Format.flush_str_formatter ()
let string_of_term tm  =
  pr_term Format.str_formatter tm;
  Format.flush_str_formatter ()
let string_of_typ ty =
  pr_typ Format.str_formatter ty;
  Format.flush_str_formatter ()
let string_of_kind k  =
  pr_kind Format.str_formatter k;
  Format.flush_str_formatter ()
let string_of_typefam t  =
  pr_typefam Format.str_formatter t;
  Format.flush_str_formatter ()
let string_of_obj o  =
  pr_obj Format.str_formatter o;
  Format.flush_str_formatter ()
let string_of_query q  =
  pr_query Format.str_formatter q;
  Format.flush_str_formatter ()
let string_of_query_explicit q =
  pr_query_explicit Format.str_formatter q;
  Format.flush_str_formatter ()
let string_of_solution sol =
  pr_solution Format.str_formatter sol;
  Format.flush_str_formatter ()

(*** This set of functions is for printing implicit LF given  
     particular type and object tables.                      ***)

let appFixity = Lfabsyn.Infix
let appPrec = Lfabsyn.maxPrec + 1
let appAssoc = Lfabsyn.Left
let bindFixity = Lfabsyn.Prefix (* ? *)
let bindPrec = Lfabsyn.minPrec - 3
let bindAssoc = Lfabsyn.None
let arrFixity = Lfabsyn.Infix
let arrPrec = Lfabsyn.minPrec - 1
let arrAssoc = Lfabsyn.Right
let colFixity = Lfabsyn.Infix
let colPrec = Lfabsyn.minPrec - 2
let colAssoc = Lfabsyn.Left

type atermcontext =
  LeftTermContext
| RightTermContext
| WholeTermContext

let needsParens opfix opassoc opprec context fix assoc prec =
  let checkLE () = opprec <= prec in
  let checkL () = opprec < prec in
  let checkLeft () =
    match (opfix, opassoc) with
      (_, Lfabsyn.None)
    | (Lfabsyn.Infix, Lfabsyn.Left)
    | (Lfabsyn.Postfix, Lfabsyn.Left) -> checkL ()
    | _ -> checkLE ()
  in
  let checkRight () =
    match (opfix, opassoc) with
      (Lfabsyn.Infix, Lfabsyn.Left)
    | (Lfabsyn.Postfix, Lfabsyn.Left) -> checkLE ()
    | _ -> checkL ()
  in
  match context with
    LeftTermContext ->
      (match (fix, assoc) with
        (Lfabsyn.Infix, Lfabsyn.None)
      | (Lfabsyn.Infix, Lfabsyn.Right)
      | (Lfabsyn.Postfix, Lfabsyn.None) -> checkLE ()
      | (Lfabsyn.Infix, Lfabsyn.Left)
      | (Lfabsyn.Postfix, Lfabsyn.Left) -> checkLeft ()
      | _ -> (Errormsg.impossible Errormsg.none "PrintLF.needsParens: invalid fixity"))
  | RightTermContext ->
      (match (fix, assoc) with
        (Lfabsyn.Infix, Lfabsyn.None)
      | (Lfabsyn.Infix, Lfabsyn.Left)
      | (Lfabsyn.Prefix, Lfabsyn.None) -> checkLE ()
      | (Lfabsyn.Infix, Lfabsyn.Right)
      | (Lfabsyn.Prefix, Lfabsyn.Right) -> checkRight ()
      | _ -> (Errormsg.impossible Errormsg.none "PrintLF.needsParens: invalid fixity"))
  | WholeTermContext -> false
    
let rec skip k l =
  match k, l with
    0, _ -> l
  | n, (x :: l') -> skip (n - 1) l'
  | _ ->
    prerr_endline "Attempting to skip in empty argument list.";
    l

(* print a term using implicit LF *)
let rec pr_iterm types objs context fix assoc prec ppf tm =
  (* print a list of arguments *)
  let rec pr_args ppf = function
    | (a :: args) ->
      Format.fprintf ppf "@ %a%a"
        (pr_term RightTermContext appFixity appAssoc appPrec) a
        pr_args args
    | [] -> ()
  (* print an abstraction term *)
  and pr_abs context fix assoc prec ppf = function
    | Lfabsyn.AbsTerm(s, Lfabsyn.Unknown, body) ->
      Format.fprintf ppf "@[<2>[@,%a@,]@ %a@]"
        pr_symb s
        (pr_term RightTermContext bindFixity bindAssoc bindPrec) body
    | Lfabsyn.AbsTerm(s, ty, body) ->
      Format.fprintf ppf "@[<2>[@,%a@,:@,%a@,]@ %a@]"
        pr_symb s
        (* MKS: check this is the right context to use here *)
        (pr_ityp types objs LeftTermContext colFixity colAssoc colPrec) ty
        (pr_term RightTermContext bindFixity bindAssoc bindPrec) body
  (* print an application term *)
  and pr_app context fix assoc prec ppf = function
    | Lfabsyn.AppTerm(Lfabsyn.Const(s), args) ->
      let o = Option.get (Symboltable.lookup objs s) in
      let printargs = skip (Lfabsyn.get_obj_implicit o) args in
      (match o with
        Lfabsyn.Object(_,_,Lfabsyn.NoFixity,opAssoc,opPrec,implicit) ->
          Format.fprintf ppf "@[<hov 2>%a%a@]" pr_symb s pr_args args
      | Lfabsyn.Object(_,_,Lfabsyn.Prefix,opAssoc,opPrec,implicit) ->
        if List.length args = 1
        then pr_prefix opAssoc opPrec context fix assoc prec ppf (s, List.hd args)
        else
          Format.fprintf ppf "@[%a%a@]"
            (pr_prefix opAssoc opPrec LeftTermContext appFixity appAssoc appPrec) (s, List.hd args)
            pr_args (List.tl args)
      | Lfabsyn.Object(_,_,Lfabsyn.Infix,opAssoc,opPrec,implicit) ->
        if List.length args = 2
        then pr_infix opAssoc opPrec context fix assoc prec ppf (s, args)
        else
          Format.fprintf ppf "@[%a%a@]"
            (pr_infix opAssoc opPrec LeftTermContext appFixity appAssoc appPrec) (s, args)
            pr_args (List.tl (List.tl args))
      | Lfabsyn.Object(_,_,Lfabsyn.Postfix,opAssoc,opPrec,implicit) ->
        if List.length args = 1
        then pr_postfix opAssoc opPrec context fix assoc prec ppf (s, List.hd args)
        else
          Format.fprintf ppf "@[%a%a@]"
            (pr_postfix opAssoc opPrec LeftTermContext appFixity appAssoc appPrec) (s, List.hd args)
            pr_args (List.tl args)     )
    | Lfabsyn.AppTerm(id, []) ->
      pr_id ppf id 
    | Lfabsyn.AppTerm(id, args) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id pr_args args
  and pr_prefix opassoc opprec context fix assoc prec ppf (s, a) =
    let f ppf (s,a) =
      Format.fprintf ppf "%a@ %a" pr_symb s (pr_term RightTermContext Lfabsyn.Prefix opassoc opprec) a
    in
    if needsParens Lfabsyn.Prefix opassoc opprec context fix assoc prec
    then Format.fprintf ppf "(%a)" f (s,a)
    else f ppf (s,a)
  and pr_infix opassoc opprec context fix assoc prec ppf (s, (a1 :: a2 :: args)) =
    let f ppf (s, (a1,a2)) =
      Format.fprintf ppf "%a@ %a@ %a"
        (pr_term LeftTermContext Lfabsyn.Infix opassoc opprec) a1
        pr_symb s
        (pr_term RightTermContext Lfabsyn.Infix opassoc opprec) a2
    in
    if needsParens Lfabsyn.Infix opassoc opprec context fix assoc prec
    then Format.fprintf ppf "(%a)" f (s, (a1, a2))
    else f ppf (s ,(a1, a2))
  and pr_postfix opassoc opprec context fix assoc prec ppf (s, a) =
    let f ppf (s,a) =
      Format.fprintf ppf "%a@ %a" (pr_term LeftTermContext Lfabsyn.Postfix opassoc opprec) a pr_symb s
    in
    if needsParens Lfabsyn.Postfix opassoc opprec context fix assoc prec
    then Format.fprintf ppf "(%a)" f (s, a)
    else f ppf (s, a)
  and pr_term context fix assoc prec ppf = function
    | Lfabsyn.AbsTerm (_, _, _) as tm ->
      if needsParens bindFixity bindAssoc bindPrec context fix assoc prec
      then Format.fprintf ppf "(%a)" (pr_abs context fix assoc prec) tm
      else pr_abs context fix assoc prec ppf tm
    | Lfabsyn.AppTerm(id, tms) as tm ->
      if needsParens appFixity appAssoc appPrec context fix assoc prec 
      then  Format.fprintf ppf "(%a)" (pr_app context fix assoc prec) tm
      else pr_app context fix assoc prec ppf tm
    | Lfabsyn.IdTerm(id) -> pr_id ppf id
  in
  pr_term context fix assoc prec ppf tm

(* print a type using implicit LF *)
and pr_ityp types objs context fix assoc prec ppf ty=
  (* print a lsit of arguments *)
  let rec pr_args ppf = function
    | (a :: args) ->
      Format.fprintf ppf "@ %a%a"
        (pr_iterm types objs RightTermContext appFixity appAssoc appPrec) a
        pr_args args
    | [] -> ()
  (* print a Pi type *)
  and pr_pi context fix assoc prec ppf = function
    | Lfabsyn.PiType (s, tyl, tyr, true) ->
        Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
          pr_symb s
          (* MKS: is the context set right? *)
          (pr_typ RightTermContext colFixity colAssoc colPrec) tyl
          (pr_typ RightTermContext bindFixity bindAssoc bindPrec) tyr
    | Lfabsyn.PiType (s, tyl, tyr, false) ->
        Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]"
          (pr_typ LeftTermContext arrFixity arrAssoc arrPrec) tyl
          kwd imp
          (pr_typ LeftTermContext arrFixity arrAssoc arrPrec) tyr
  (* print a base type (application) *)
  and pr_app context fix assoc prec ppf = function
    | Lfabsyn.AppType(Lfabsyn.Const(s), args) ->
      let t = Option.get (Symboltable.lookup types s) in
      let printargs = skip (Lfabsyn.get_typefam_implicit t) args in
      (match t with
        Lfabsyn.TypeFam(_,_,Lfabsyn.NoFixity,opAssoc,opPrec,_,implicit) ->
          Format.fprintf ppf "@[<hov 2>%a%a@]" pr_symb s pr_args args
      | Lfabsyn.TypeFam(_,_,Lfabsyn.Prefix,opAssoc,opPrec,_,implicit) ->
        if List.length args = 1
        then pr_prefix opAssoc opPrec context fix assoc prec ppf (s, List.hd args)
        else
          Format.fprintf ppf "@[%a%a@]"
            (pr_prefix opAssoc opPrec LeftTermContext appFixity appAssoc appPrec) (s, List.hd args)
            pr_args (List.tl args)
      | Lfabsyn.TypeFam(_,_,Lfabsyn.Infix,opAssoc,opPrec,_,implicit) ->
        if List.length args = 2
        then pr_infix opAssoc opPrec context fix assoc prec ppf (s, args)
        else
          Format.fprintf ppf "@[%a%a@]"
            (pr_infix opAssoc opPrec LeftTermContext appFixity appAssoc appPrec) (s, args)
            pr_args (List.tl (List.tl args))
      | Lfabsyn.TypeFam(_,_,Lfabsyn.Postfix,opAssoc,opPrec,_,implicit) ->
        if List.length args = 1
        then pr_postfix opAssoc opPrec context fix assoc prec ppf (s, List.hd args)
        else
          Format.fprintf ppf "@[%a%a@]"
            (pr_postfix opAssoc opPrec LeftTermContext appFixity appAssoc appPrec) (s, List.hd args)
            pr_args (List.tl args)     )
    | Lfabsyn.AppType(id, []) ->
      pr_id ppf id 
    | Lfabsyn.AppType(id, args) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id pr_args args
  and pr_prefix opassoc opprec context fix assoc prec ppf (s, a) =
    let f ppf (s,a) =
      Format.fprintf ppf "%a@ %a" pr_symb s (pr_iterm types objs RightTermContext Lfabsyn.Prefix opassoc opprec) a
    in
    if needsParens Lfabsyn.Prefix opassoc opprec context fix assoc prec
    then Format.fprintf ppf "(%a)" f (s,a)
    else f ppf (s,a)
  and pr_infix opassoc opprec context fix assoc prec ppf (s, (a1 :: a2 :: args)) =
    let f ppf (s, (a1,a2)) =
      Format.fprintf ppf "%a@ %a@ %a"
        (pr_iterm types objs LeftTermContext Lfabsyn.Infix opassoc opprec) a1
        pr_symb s
        (pr_iterm types objs RightTermContext Lfabsyn.Infix opassoc opprec) a2
    in
    if needsParens Lfabsyn.Infix opassoc opprec context fix assoc prec
    then Format.fprintf ppf "(%a)" f (s, (a1, a2))
    else f ppf (s, (a1, a2))
  and pr_postfix opassoc opprec context fix assoc prec ppf (s, a) =
    let f ppf (s,a) =
      Format.fprintf ppf "%a@ %a" (pr_iterm types objs LeftTermContext Lfabsyn.Postfix opassoc opprec) a pr_symb s
    in
    if needsParens Lfabsyn.Postfix opassoc opprec context fix assoc prec
    then Format.fprintf ppf "(%a)" f (s, a)
    else f ppf (s, a)
  and pr_typ context fix assoc prec ppf = function
    | Lfabsyn.PiType (_, _, _, true) as ty ->
      if needsParens bindFixity bindAssoc bindPrec context fix assoc prec
      then Format.fprintf ppf "(%a)" (pr_pi context fix assoc prec) ty
      else pr_pi context fix assoc prec ppf ty
    | Lfabsyn.PiType (_,_,_,false) as ty ->
      (* is this correct? *)
      if needsParens arrFixity arrAssoc arrPrec context fix assoc prec
      then Format.fprintf ppf "(%a)" (pr_pi context fix assoc prec) ty
      else pr_pi context fix assoc prec ppf ty
    | Lfabsyn.AppType (_, _) as ty ->
      if needsParens appFixity appAssoc appPrec context fix assoc prec
      then Format.fprintf ppf "(%a)" (pr_app context fix assoc prec) ty
      else pr_app context fix assoc prec ppf ty
   | Lfabsyn.IdType (id) -> pr_id ppf id
  in
  pr_typ context fix assoc prec ppf ty

(* print a kind using implicit LF *)
let pr_ikind types objs context fix assoc prec ppf k =
  (* print a Pi kins *)
  let rec pr_pi context fix assoc prec ppf = function
    | Lfabsyn.PiKind(s,ty,k,dep) ->
      if dep
      then
        Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
          pr_symb s
          (pr_ityp types objs RightTermContext colFixity colAssoc colPrec) ty
          (pr_kind LeftTermContext bindFixity bindAssoc bindPrec) k
      else
        Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]"
          (pr_ityp types objs LeftTermContext arrFixity arrAssoc arrPrec) ty
          kwd imp
          (pr_kind RightTermContext arrFixity arrAssoc arrPrec) k
  and pr_kind context fix assoc prec ppf = function
    | Lfabsyn.PiKind(_,_,_,_) as ty ->
      if needsParens bindFixity bindAssoc bindPrec context fix assoc prec
      then Format.fprintf ppf "(%a)" (pr_pi context fix assoc prec) ty
      else pr_pi context fix assoc prec ppf ty
    | Lfabsyn.Type -> Format.fprintf ppf "%a" kwd typekwd
  in
  pr_kind context fix assoc prec ppf k


(* These are the externally visible general printinf functions *)
let pr_term_implicit types objs =
  pr_iterm types objs WholeTermContext Lfabsyn.NoFixity Lfabsyn.None 0
let pr_typ_implicit types objs =
  pr_ityp types objs WholeTermContext Lfabsyn.NoFixity Lfabsyn.None 0
let pr_kind_implicit types objs =
  pr_ikind types objs WholeTermContext Lfabsyn.NoFixity Lfabsyn.None 0
let pr_typefam_implicit types objs ppf (Lfabsyn.TypeFam (s, k, _, _, _, _, _)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_symb s (pr_kind_implicit types objs) k
let pr_obj_implicit types objs ppf (Lfabsyn.Object (s, ty, _, _, _, _)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_symb s (pr_typ_implicit types objs) ty
let pr_query_implicit types objs ppf (Lfabsyn.Query (_, s, ty)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_symb s (pr_typ_implicit types objs) ty
let pr_query_exp_implicit types objs ppf (Lfabsyn.Query (fvars, s, ty)) =
  (** Print the logic variables along with their types **)
  let rec pr_qvars ppf = function
    | ((s,ty) :: vars) -> Format.fprintf ppf "%a :@ %a.@,%a"
      pr_symb s
      (pr_typ_implicit types objs) ty
      pr_qvars vars
    | _ -> ()
  in
  Format.fprintf ppf "@[<4>@[<2>%a@]@,%a :@ %a@]"
    pr_qvars fvars
    pr_symb s
    (pr_typ_implicit types objs) ty
let pr_solution_implicit types objs ppf (subst, disprs) =
  let pr_subst ppf (s, tm) =
    Format.fprintf ppf "@[<4>%a =@ %a@]"
      pr_symb s
      (pr_term_implicit types objs) tm
  in
  let pr_dispr ppf (tml, tmr) =
    Format.fprintf ppf "@[<4><%a,@ %a>@]"
      (pr_term_implicit types objs) tml
      (pr_term_implicit types objs) tmr
  in
  let rec pr_substs ppf = function
    | (s :: ss) -> Format.fprintf ppf "@.%a%a" pr_subst s pr_substs ss
    | _ -> ()
  in
  let rec pr_disprs ppf = function
    | (d :: dd) -> Format.fprintf ppf "@.%a%a" pr_dispr d pr_disprs dd
    | _ -> ()
  in
  Format.fprintf ppf "@[<v 0>%a@.%a@]" pr_substs subst pr_disprs disprs
    

(* use general formmatting functions to print to stdout *)
let print_term_implicit types objs = pr_term_implicit types objs Format.std_formatter 
let print_typ_implicit types objs = pr_typ_implicit types objs Format.std_formatter
let print_kind_implicit types objs = pr_kind_implicit types objs Format.std_formatter
let print_typefam_implicit types objs = pr_typefam_implicit types objs Format.std_formatter
let print_obj_implicit types objs = pr_obj_implicit types objs Format.std_formatter
let print_query_implicit types objs = pr_query_implicit types objs Format.std_formatter
let print_query_exp_implicit types objs = pr_query_exp_implicit types objs Format.std_formatter
let print_solution_implicit types objs = pr_solution_implicit types objs Format.std_formatter 

(* use general formatting functions to generate a string *)
let string_of_term_implicit types objs tm  =
  pr_term_implicit types objs Format.str_formatter tm;
  Format.flush_str_formatter ()
let string_of_typ_implicit types objs ty =
  pr_typ_implicit types objs Format.str_formatter ty;
  Format.flush_str_formatter ()
let string_of_kind_implicit types objs k  =
  pr_kind_implicit types objs Format.str_formatter k;
  Format.flush_str_formatter ()
let string_of_typefam_implicit types objs t  =
  pr_typefam_implicit types objs Format.str_formatter t;
  Format.flush_str_formatter ()
let string_of_obj_implicit types objs o  =
  pr_obj_implicit types objs Format.str_formatter o;
  Format.flush_str_formatter ()
let string_of_query_implicit types objs q  =
  pr_query_implicit types objs Format.str_formatter q;
  Format.flush_str_formatter ()
let string_of_query_exp_implicit types objs q =
  pr_query_exp_implicit types objs Format.str_formatter q;
  Format.flush_str_formatter ()
let string_of_solution_implicit types objs sol  =
  pr_solution_implicit types objs Format.str_formatter sol;
  Format.flush_str_formatter ()
