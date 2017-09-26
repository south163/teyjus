(* use ocaml formatting to print LF terms more nicely *)

let imp = "->"

let kwd ppf s = Format.fprintf ppf "%s" s

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
      Format.fprintf ppf "@[<hov 2>{@,%a@ :@ %a@,}@ %a@]"
                     pr_id id pr_typ tyl pr_typ tyr
  | Lfabsyn.ImpType (tyl, tyr) ->
      Format.fprintf ppf "@[<hov 2>%a@ %a@ %a@]" pr_typ' tyl kwd imp pr_typ tyr
  | Lfabsyn.AppType (id, tms) ->
      Format.fprintf ppf "@[<hov 2>%a%a@]" pr_id id pr_terms tms
  | Lfabsyn.IdType (id) -> pr_id ppf id

and pr_typ' ppf = function
  | Lfabsyn.IdType (id) -> pr_id ppf id
  | t -> Format.fprintf ppf "@[<2>(%a)@]" pr_typ t



(*** use general formmatting functions to print to stdout ***)
let print_id = pr_id Format.std_formatter 
let print_term = pr_term Format.std_formatter 
let print_typ = pr_typ Format.std_formatter 

(*** use general formatting functions to generate a string ***)
let string_of_id id =
  pr_id Format.str_formatter id;
  Format.flush_str_formatter ()
let string_of_term tm  =
  pr_term Format.str_formatter tm;
  Format.flush_str_formatter ()
let string_of_typ ty =
  pr_typ Format.str_formatter ty;
  Format.flush_str_formatter ()
