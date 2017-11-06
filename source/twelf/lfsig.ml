(** Describes a representation for fully explicit LF signatures. *)

type signature = Signature of (Lfabsyn.typefam Symboltable.table * Lfabsyn.obj Symboltable.table)


(* print the signature by type family. for each type family print out 
   the corresponding objects in order *)
let string_of_sig (Signature(types, objs)) =
  let per_type symb (Lfabsyn.TypeFam(_,_,_,_,_,objlst,_) as ty) str =
    (Lfabsyn.string_of_typefam ty) ^ "\n" ^
    (List.fold_left (fun str sym -> str ^ (Lfabsyn.string_of_obj (Option.get (Symboltable.lookup objs sym))) ^ "\n" ) "" (!objlst)) ^ str
  in Symboltable.fold types per_type ""


let get_typetable (Signature(types, objs)) = types
let get_objtable  (Signature(types, objs)) = objs
