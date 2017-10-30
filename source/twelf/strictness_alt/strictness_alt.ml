(** Implements the strictness check for translating LF types. *)
open Set
open List
open Hashtbl
open Printf

(* ---------------------------------------------------------------------------------- *)
(*Definitions of data types*)


(*Definition of Id set and Id map*)
let compare_id i i' = Pervasives.compare (Lfabsyn.string_of_id i) (Lfabsyn.string_of_id i')
module OrderedId = struct
  type t = Lfabsyn.id
  let compare = compare_id
end
module IdSet = Set.Make(OrderedId)

type idset = IdSet.t
type idMap = (Lfabsyn.id, idset) Hashtbl.t

(*Definition of types involved in the strictness functions*)
type dependency = idMap (* id -> P(id), if x -> {y}, then if x is strict, then y is strict*)
type delta = idset (* bounded variables in a term *)
type gamma = idset (* context *)

type aposanntype = Pos of (Lfabsyn.id * aneganntype) list * Lfabsyn.id * Lfabsyn.term list
and aneganntype = Neg of (Lfabsyn.id * aposanntype) list * Lfabsyn.id * Lfabsyn.term list * idset


(*Helper methods*)

(*Duplicate a set/map, might not be necessary*)
let setcopy set = IdSet.fold (fun x s -> IdSet.add x s) set IdSet.empty;;
let mapcopy map = Hashtbl.fold (fun k v m -> Hashtbl.add m k (setcopy v); m) map (Hashtbl.create 16);; 

(*convert a set of id to a string list *)
let tolist = fun s -> IdSet.fold (fun x l -> (Lfabsyn.string_of_id x)::l) s [];; 


(*print a string list*)
let rec printlist l = printf "["; printlist_rec l; printf "]"
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
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_pos_rec tp g in
  	let s_final = finalize s dep in
  		(Pos (ann_pairs, c, tms), s_final)


and find_strict_vars_neg tp g =
  let (s, dep, ann_pairs, c, tms) = find_strict_vars_neg_rec tp g in
  	let s_final = finalize s dep in
  		(Neg (ann_pairs, c, tms, (IdSet.diff s_final g)), s_final)

              
(* find (incomplete) strict variables, dependency information, and elements to construct pos_ann_type 
        given positive type and context g *)
and find_strict_vars_pos_rec tp g =
  match tp with
    Lfabsyn.PiType (x, tpA, tpB) -> let (ann_tpA, sA) = find_strict_vars_neg tpA g in
                            let (s, dep, ann_pairs, tc, tms) = find_strict_vars_pos_rec tpB  (IdSet.add x g)
                            in (s, (add_dep dep x (IdSet.inter sA g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | Lfabsyn.AppType (c, tms) -> ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms)
  | Lfabsyn.ImpType (tpA, tpB) -> find_strict_vars_pos_rec tpB g                       
  | Lfabsyn.IdType t -> (IdSet.empty, Hashtbl.create 16, [], t, [])


(* find (incomplete) strict variables, dependency information, and elements to construct neg_ann_type 
        given negative type and context g *)
and find_strict_vars_neg_rec tp g =
  match tp with
  | Lfabsyn.PiType (x, tpA, tpB) -> let (ann_tpA, sA) = find_strict_vars_pos tpA g in
                            let (s, dep, ann_pairs, tc, tms) = find_strict_vars_neg_rec tpB  (IdSet.add x g)
                            in (s, (add_dep dep x (IdSet.inter sA g)), (x, ann_tpA)::ann_pairs, tc, tms)
  | Lfabsyn.AppType (c, tms) -> ((union_fsvo_terms tms g), Hashtbl.create 16, [], c, tms)
  | Lfabsyn.ImpType (tpA, tpB) -> find_strict_vars_neg_rec tpB g                         
  | Lfabsyn.IdType t -> (IdSet.empty, Hashtbl.create 16, [], t, [])


(* find all the strict variables in a list of terms *)
and union_fsvo_terms tms g =
  match tms with
  | [] -> IdSet.empty
  | tm :: tms' -> IdSet.union (find_strict_vars_term tm g IdSet.empty) (union_fsvo_terms tms' g)

                
(* add x to (dep[v] : list) for each v in l *)
and add_dep (dep : dependency) (x : Lfabsyn.id) (l : idset) =
  IdSet.fold (fun v (tbl : dependency) ->
      match  Hashtbl.find_opt tbl x with
      | None -> Hashtbl.add tbl x (IdSet.singleton v); tbl
      | Some set -> Hashtbl.replace tbl x (IdSet.add v set);
                    tbl) l dep

(*returns the set of strict variables in a term*)
and find_strict_vars_term tm g d =
  match tm with
  | Lfabsyn.AppTerm (v, tms) -> if (all_ids_are_strict tms d IdSet.empty)
                        then IdSet.singleton v (*Init0*)
                        else if not (IdSet.mem v g)
                        then union_sv_subterms tms g d (*App0*)
                        else IdSet.empty
  | Lfabsyn.AbsTerm (v, tp, tm') ->  find_strict_vars_term tm' g (IdSet.add v d) (*ABS0*)
  | _ -> IdSet.empty

(*Check if all terms in tms are ids and if they are all bounded (in delta)*)
and all_ids_are_strict tms d checked : bool =
  match tms with
  | [] -> true
  | (Lfabsyn.IdTerm v)::tms' -> IdSet.mem v d && not (IdSet.mem v checked) && all_ids_are_strict tms' d (IdSet.add v checked)
  | _ -> false

(* the union of all strict variables found in tms *)
and union_sv_subterms tms g d =
  List.fold_left (fun vars tm -> IdSet.union (find_strict_vars_term tm g d) vars) IdSet.empty tms

(*finalize the set of strict variables with dependency information*)
and finalize s dep =
  let s' = IdSet.union (IdSet.fold (fun v set ->
                             match (Hashtbl.find_opt dep v) with
                             | None -> set
                             | Some s -> IdSet.union s set) s IdSet.empty) s
  in if (tolist s' = tolist s) then s'
     else finalize s' dep;;
       







