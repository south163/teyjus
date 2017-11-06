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


let rec pr_qvars ppf = function
  | ((id,ty) :: vars) -> Format.fprintf ppf "<%a :@ %a>@," pr_id id pr_qvars vars
  | _ -> ()
    
let pr_query_explicit ppf (Lfabsyn.Query (fvars, s, ty)) =
  (** Print the logic variables along with their types **)
  let rec pr_qvars ppf = function
    | ((s,ty) :: vars) -> Format.fprintf ppf "%a :@ %a.@," pr_symb s pr_qvars vars
    | _ -> ()
  in
  Format.fprintf ppf "@[<4>@[<2>%a@]@,%a :@ %a@]" pr_qvars fvars pr_symb s pr_typ ty

let pr_subst ppf (s, tm) =
  Format.fprintf ppf "@[<4>%a =@ %a@]" pr_symb s pr_term tm

let pr_dispr ppf (tml, tmr) =
  Format.fprintf ppf "@[<4><%a,@ %a>@]" pr_term tml pr_term tmr

let rec pr_substs ppf = function
  | (s :: ss) -> Format.fprintf ppf "%a@,%a" pr_subst s pr_substs ss
  | _ -> ()

let rec pr_disprs ppf = function
  | (d :: dd) -> Format.fprintf ppf "%a@,%a" pr_dispr d pr_disprs dd
    
let pr_solution ppf (subst, disprs) =
  Format.fprintf ppf "@[<v 0>%a@,%a@]" pr_substs subst pr_disprs disprs

  

(* use general formmatting functions to print to stdout *)
let print_id = pr_id Format.std_formatter 
let print_term = pr_term Format.std_formatter 
let print_typ = pr_typ Format.std_formatter 

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


(*** This set of functions is for printing implicit LF given  
     particular type and object tables.                      ***)

let appFixity = Lfabsyn.Infix
let appPrec = Lfabsyn.maxPrec + 1
let appAssoc = Lfabsyn.Left
let bindFixity = Lfabsyn.Prefix (* ? *)
let bindPrec = Lfabsyn.minPrec - 3
let bindAssoc = Lfabsyb.None
let arrFixity = Lfabsyn.Infix
let arrPrec = Lfabsyn.minPrec - 1
let arrAssoc = Lfabsyn.Right
let colFixity = Lfabsyn.Infix
let colPrec = Lfabsyn.minPrec - 2
let colAssoc = Lfabsyn.Left
  
let rec skip k l =
  match k, l with
    0, _ -> l
  | n, (x :: l') -> skip (n1) l'
  | _ ->
    prerr_endline "Attempting to skip in empty argument list.";
    l

let rec pr_iterm types objmap fix assoc prec ppf = function
  | Lfabsyn.AbsTerm (s, Lfabsyn.Unknown, tm) ->
    Format.fprintf ppf "@[<2>[@,%a@,]@ %a@]"
                     pr_symb s (pr_iterm types objmap bindFixity bindAssoc bindPrec) tm
  | Lfabsyn.AbsTerm (s, ty, tm) ->
      Format.fprintf ppf "@[<2>[@,%a@,:@,%a@,]@ %a@]"
                     pr_symb s
                     (pr_ityp types objmap colFixity colAssoc colPrec) ty
                     (pr_iterm types objmap bindFixity bindAssoc bindPrec) tm
  | Lfabsyn.AppTerm(Lfabsyn.Const(s), tms) ->
      (match Option.get (Symboltable.lookup objs s) with
          Lfabsyn.Object(_,_,Lfabsyn.NoFixity,_,_,implicit) ->
            Format.fprintf ppf "@[<hov 2>%a%a@]"
              pr_symb s
              (pr_iterms types objmap) (skip implicit tms)
        | Lfabsyn.Object(_,_,Lfabsyn.Prefix,opAssoc,opPrec,implicit) ->
          (match skip implicit tms with
            (a :: []) ->
              Format.fprintf ppf "@[<hov 2>%a@ %a@]"
                pr_symb s
                (pr_iterm types objmap Lfabsyn.Prefix opAssoc opPrec) a
          | args ->
            Format.fprontf ppf "@[<hov 2>%a%a@]"
              pr_symb s
              (pr_iterms types objmap) args )
        | Lfabsyn.Object(_,_,Lfabsyn.Infix,opAssoc,opPrec,implicit) ->
          (match skip implicit tms with
            (a1 :: a2 :: []) ->
              Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]"
                (pr_iterm types objmap Lfabsyn.Infix opAssoc opPrec) a1
                pr_symb s
                (pr_iterm types objmap Lfabsyn.Infix opAssoc opPrec) a2
          | args ->          )
        | Lfabsyn.Object(_,_,Lfabsyn.Postfix,opAssoc,opPrec,implicit) ->
      )
  | Lfabsyn.AppTerm(id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]"
        pr_id id
        (pr_iterms types objmap) tms )
  | Lfabsyn.IdTerm(id) -> pr_id ppf id

and pr_iterm' types objmap fix assoc prec ppf = function
  | Lfabsyn.IdTerm (id) -> pr_id ppf id
  | t ->
     Format.fprintf ppf "@[<2>(%a)@]" (pr_iterm types objmap fix assoc prec) t
                        
and pr_iterms types objmap ppf l =
  let rec f ppf = function
    | (tm :: tms) ->
        Format.fprintf ppf "@ %a%a" (pr_iterm' types objmap appFix appAssoc appPrec) tm f tms
    | _ -> ()
  in
  f ppf l
    
and pr_ityp types objmap fix assoc prec ppf = function
  | Lfabsyn.PiType (s, tyl, tyr,dep) ->
    if dep
    then
      Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
        pr_symb s
        (pr_ityp types objmap colFixity colAssoc colPrec) tyl
        (pr_ityp types objmap bindFixity bindAssoc bindPrec) tyr
    else
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]"
        (pr_ityp' types objmap arrFix arrAssoc arrPrec) tyl
        kwd imp
        (pr_ityp types objmap fix assoc prec) tyr
  | Lfabsyn.AppType (id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id (pr_iterms types objmap) tms
  | Lfabsyn.IdType (id) -> pr_id ppf id

and pr_ityp' types objmap ppf = function
  | Lfabsyn.IdType (id) -> pr_id ppf id
  | t -> Format.fprintf ppf "@[<2>(%a)@]" (pr_ityp types objmap) t
