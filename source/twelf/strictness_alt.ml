(** Implements the strictness check for translating LF types. *)
open Set
open List
open Hashtbl
open Printf


(* ---------------------------------------------------------------------------------- *)
(*Definitions of data types*)


(*Definition of symbol set and symbol map*)
let compare_symb sym sym' = Pervasives.compare (Symb.id sym) (Symb.id sym')
module OrderedSymb = struct
  type t = Symb.symbol
  let compare = compare_symb
end
module SymbSet = Set.Make(OrderedSymb)

type symbset = SymbSet.t
type symbMap = (Symb.symbol, symbset) Hashtbl.t

(*Definition of types involved in the strictness functions*)
type dependency = symbMap (* id -> P(id), if x -> {y}, then if x is strict, then y is strict*)
type delta = symbset (* bounded variables in a term *)
type gamma = symbset (* context *)

type aposanntype = 
  | Pos of (Symb.symbol * aneganntype) list * Lfabsyn.id * Lfabsyn.term list
  | PosNone

and aneganntype = 
  | Neg of (Symb.symbol * aposanntype) list * Lfabsyn.id * Lfabsyn.term list * symbset
  | NegNone

(*Helper methods*)

(*Duplicate a set/map, might not be necessary*)
let setcopy set = SymbSet.fold (fun x s -> SymbSet.add x s) set SymbSet.empty;;
let mapcopy map = Hashtbl.fold (fun k v m -> Hashtbl.add m k (setcopy v); m) map (Hashtbl.create 16);;

(*convert a set of id to a string list *)
let tolist = fun s -> SymbSet.fold (fun x l -> (Symb.name x)::l) s [];;


(*print a symbset*)
let rec printset s = printf "["; printlist_rec (tolist s); printf "]"

and printlist l =  printf "["; printlist_rec l; printf "]\n"

and printlist_rec l =
  match l with
  [] -> ()
  | x::l' -> printf "%s; " (x); printlist_rec l';;


(*convert a map to a list of pairs*)
let topairlist = fun h -> Hashtbl.fold (fun k v acc -> (k, (tolist v)) :: acc) h [];;



(* ---------------------------------------------------------------------------------- *)
(*Strictness functions*)

(* annotate tp given a context gamma*)
let rec find_strict_vars_pos tp g =
  let (s, dep, ann_pairs, c, tms, g') = find_strict_vars_pos_rec tp g in
  	let s_final = finalize s dep in
  		(Pos (ann_pairs, c, tms), (SymbSet.inter g' s_final))


and find_strict_vars_neg tp g =
  let (s, dep, ann_pairs, c, tms, g') = find_strict_vars_neg_rec tp g in

  	let s_final = finalize s dep in
  		(Neg (ann_pairs, c, tms, (SymbSet.inter s_final g')), (SymbSet.inter g' s_final))


(* find a (incomplete) set of strict variables, dependency information, and elements to construct pos_ann_type 
        given a positive type tp and context g *)
and find_strict_vars_pos_rec tp g =
  match tp with
    Lfabsyn.PiType (x, tpA, tpB, _) -> let (ann_tpA, sA) = find_strict_vars_neg tpA g in
                            let (s, dep, ann_pairs, tc, tms, g') = find_strict_vars_pos_rec tpB (SymbSet.add x g)
                            in (s, (add_dep dep x (SymbSet.inter sA g)), (x, ann_tpA)::ann_pairs, tc, tms, (SymbSet.add x g'))
  | Lfabsyn.AppType (c, tms) -> (*printf "Tycon and Terms: %s\n" (Lfabsyn.string_of_typ tp);*)
                                ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms, g)
  | Lfabsyn.IdType t -> (SymbSet.empty, Hashtbl.create 16, [], t, [], SymbSet.empty)
  | Lfabsyn.Unknown -> printf "Unknown type"; exit(1)


(* find a (incomplete) set of strict variables, dependency information, and elements to construct neg_ann_type
        given a negative type and context g *)
and find_strict_vars_neg_rec tp g =
  match tp with
  | Lfabsyn.PiType (x, tpA, tpB, _) -> let (ann_tpA, sA) = find_strict_vars_pos tpA g in
                            let (s, dep, ann_pairs, tc, tms, g') = find_strict_vars_neg_rec tpB  (SymbSet.add x g)
                            in (s, (add_dep dep x (SymbSet.inter sA g)), (x, ann_tpA)::ann_pairs, tc, tms, (SymbSet.add x g'))
 (* | Lfabsyn.PiType (x, tpA, tpB, false) -> find_strict_vars_neg_rec tpB g*)
  | Lfabsyn.AppType (c, tms) -> (*printf "Tycon and Terms: %s\n" (Lfabsyn.string_of_typ tp);*)
                               ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms, g)
  | Lfabsyn.IdType t -> (SymbSet.empty, Hashtbl.create 16, [], t, [], SymbSet.empty)
  | Lfabsyn.Unknown -> printf "Unknown type"; exit(1)


(* find all the strict variables in a list of terms *)
and union_fsvo_terms tms g =
  match tms with
  | [] -> SymbSet.empty
  | tm :: tms' -> SymbSet.union (find_strict_vars_term tm g SymbSet.empty) (union_fsvo_terms tms' g)


(* add x to (dep[v] : list) for each v in l *)
and add_dep (dep : dependency) (x : Symb.symbol) (l : symbset) =
  SymbSet.fold (fun v (tbl : dependency) ->
		    try let set = Hashtbl.find tbl x in Hashtbl.replace tbl x (SymbSet.add v set); tbl
		    with Not_found -> Hashtbl.add tbl x (SymbSet.singleton v); tbl) l dep

(*returns the set of strict variables in a term*)
and find_strict_vars_term tm g d =
  match tm with
  | Lfabsyn.AppTerm (id, tms) -> if (all_ids_are_strict tms d SymbSet.empty)
                        then SymbSet.singleton (Lfabsyn.get_id_symb id) (*Init0*)
                        else if not (SymbSet.mem (Lfabsyn.get_id_symb id) g)
                        then union_sv_subterms tms g d (*App0*)
                        else SymbSet.empty
  | Lfabsyn.AbsTerm (x, tp, tm') ->  find_strict_vars_term tm' g (SymbSet.add x d) (*ABS0*)
  | Lfabsyn.IdTerm id -> SymbSet.singleton (Lfabsyn.get_id_symb id)

(*Check if all terms in tms are ids and if they are all bounded (in delta)*)
and all_ids_are_strict tms d checked : bool =
  match tms with
  | [] -> true
  | (Lfabsyn.IdTerm id)::tms' -> SymbSet.mem (Lfabsyn.get_id_symb id) d && not (SymbSet.mem (Lfabsyn.get_id_symb id) checked) 
                            && all_ids_are_strict tms' d (SymbSet.add (Lfabsyn.get_id_symb id) checked)
  | _ -> false

(* the union of all strict variables found in tms *)
and union_sv_subterms tms g d =
  List.fold_left (fun vars tm -> SymbSet.union (find_strict_vars_term tm g d) vars) SymbSet.empty tms

(*finalize the set of strict variables with dependency information*)
and finalize s dep =
  let s' = SymbSet.union (SymbSet.fold (fun v set ->
					    try let s' = (Hashtbl.find dep v) in SymbSet.union s' set
					    with Not_found -> set) s SymbSet.empty) s
  in if (tolist s' = tolist s) then s'
     else finalize s' dep;;
       







