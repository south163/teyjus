(* use ocaml formatting to print LF terms more nicely *)

let imp = "->"
let typekwd = "type"

let kwd ppf s = Format.fprintf ppf "%s" s

(*** These first functions will print explicit LF terms; they do not 
     lookup any constants to remove implicit arguments               ***)
  
let pr_id ppf = function
  | Lfabsyn.Const (s) 
  | Lfabsyn.Var (s,_) 
  | Lfabsyn.LogicVar (s, _) -> Format.fprintf ppf "%s" s

let rec pr_term ppf = function
  | Lfabsyn.AbsTerm (id, ty, tm) ->
      Format.fprintf ppf "@[<2>[@,%a@,:@,%a@,]@ %a@]"
                     pr_id id pr_typ ty pr_term tm
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
  | Lfabsyn.PiType (id, tyl, tyr) ->
      Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
                     pr_id id pr_typ tyl pr_typ tyr
  | Lfabsyn.ImpType (tyl, tyr) ->
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]" pr_typ' tyl kwd imp pr_typ tyr
  | Lfabsyn.AppType (id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id pr_terms tms
  | Lfabsyn.IdType (id) -> pr_id ppf id

and pr_typ' ppf = function
  | Lfabsyn.IdType (id) -> pr_id ppf id
  | t -> Format.fprintf ppf "@[<2>(%a)@]" pr_typ t

let rec pr_kind ppf = function
  | Lfabsyn.PiKind (id, ty, k) ->
      Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
                     pr_id id pr_typ ty pr_kind k
  | Lfabsyn.ImpKind (ty, k) ->
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]" pr_typ' ty kwd imp pr_kind k
  | Lfabsyn.Type -> Format.fprintf ppf "%a" kwd typekwd

let pr_typefam ppf (Lfabsyn.TypeFam (id, k, _, _, _, _, _)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_id id pr_kind k

let pr_obj ppf (Lfabsyn.Object (id, ty, _, _, _, _)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_id id pr_typ ty

let pr_query ppf (Lfabsyn.Query (_, id, ty)) =
  Format.fprintf ppf "@[<4>%a :@ %a@]" pr_id id pr_typ ty


let rec pr_qvars ppf = function
  | ((id,ty) :: vars) -> Format.fprintf ppf "<%a :@ %a>@," pr_id id pr_qvars vars
  | _ -> ()
    
let pr_query_explicit ppf (Lfabsyn.Query (fvars, id, ty)) =
  (** Print the logic variables along with their types **)
  let rec pr_qvars ppf = function
    | ((id,ty) :: vars) -> Format.fprintf ppf "%a :@ %a.@," pr_id id pr_qvars vars
    | _ -> ()
  in
  Format.fprintf ppf "@[<4>@[<2>%a@]@,%a :@ %a@]" pr_qvars fvars pr_id id pr_typ ty

let pr_subst ppf (id, tm) =
  Format.fprintf ppf "@[<4>%a =@ %a@]" pr_id id pr_term tm

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


let rec pr_iterm types objmap ppf = function
  | Lfabsyn.AbsTerm (id, ty, tm) ->
      Format.fprintf ppf "@[<2>[@,%a@,:@,%a@,]@ %a@]"
                     pr_id id (pr_ityp types objmap) ty (pr_iterm types objmap) tm
  | Lfabsyn.AppTerm(id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id (pr_iterms types objmap) tms
  | Lfabsyn.IdTerm(id) -> pr_id ppf id

and pr_iterm' types objmap ppf = function
  | Lfabsyn.IdTerm (id) -> pr_id ppf id
  | t ->
     Format.fprintf ppf "@[<2>(%a)@]" (pr_iterm types objmap) t
                        
and pr_iterms types objmap ppf l =
  let rec f ppf = function
    | (tm :: tms) ->
        Format.fprintf ppf "@ %a%a" (pr_iterm' types objmap) tm f tms
    | _ -> ()
  in
  f ppf l
    
and pr_ityp types objmap ppf = function
  | Lfabsyn.PiType (id, tyl, tyr) ->
      Format.fprintf ppf "@[<2>{@,%a@ :@ %a@,}@ %a@]"
                     pr_id id (pr_ityp types objmap) tyl (pr_ityp types objmap) tyr
  | Lfabsyn.ImpType (tyl, tyr) ->
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]"
                     (pr_ityp' types objmap) tyl kwd imp (pr_ityp types objmap) tyr
  | Lfabsyn.AppType (id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id (pr_iterms types objmap) tms
  | Lfabsyn.IdType (id) -> pr_id ppf id

and pr_ityp' types objmap ppf = function
  | Lfabsyn.IdType (id) -> pr_id ppf id
  | t -> Format.fprintf ppf "@[<2>(%a)@]" (pr_ityp types objmap) t
