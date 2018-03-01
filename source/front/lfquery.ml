(** datastructures needed for inverting solutions:
      1. fvartypes
         a symbol table mapping free variables to their LF type
      2. freeVarTab
         a table with the type symbol for each translated free variable
 *)
let fvar_types = ref Table.empty

let freeVarTab = ref Intmap.empty
  
let fvartypes_init (Lfabsyn.Query(fvars, ptSymb, ty)) =
  let types = List.fold_left (fun tbl (s,t) -> Table.add (Symbol.symbol (Symb.name s)) t tbl) 
                             Table.empty 
                             ((ptSymb, ty) :: fvars) in
  fvar_types := types

(* initialize free var table with fvars *)
let freeVarTab_init fvars =
  let rec init_aux vars idx table =
    match vars with
        (v :: vars') ->
          init_aux vars' (idx + 1) (Intmap.add idx v table)
      | [] -> table
  in
  freeVarTab := init_aux fvars 0 Intmap.empty;
  true


let time f dscr =
  let start = Sys.time () in
  let x = f () in
  Printf.printf "Execution Time(%s): %fs\n" dscr (Sys.time () -. start);
  x
    
let submit_query query metadata kinds constants =
  let (term, fvars) =
    time 
      (fun () ->
        match Translator.get_translation () with
        | "naive" ->
          Translator.NaiveTranslation.translate_query query metadata kinds constants
        | "optimized" ->
          Translator.OptimizedTranslation.translate_query query metadata kinds constants)
      "translate query"
  in
  time
    (fun () ->
      let (term',_,_) = Parse.fixTerm (Parse.removeNestedAbstractions term) in 
(*  let _ = print_endline ("translated query: "^(Absyn.string_of_term term')) in  *)
      Ccode_stubs.setTypeAndTermLocation (); 
      Readterm.readTermAndType term' (Types.Molecule(Absyn.ApplicationType(Pervasive.kbool,[]),[])) fvars []; 
      fvartypes_init query; 
      freeVarTab_init fvars)
    "set up simulator to solve query"

let show_answers lpmodule ((Lfsig.Signature(types,objmap)) as lfsig) metadata = 
  let lpsol = time (fun () -> Buildterm.build_solution lpmodule (!freeVarTab)) "build term" in 
(*  let _ = print_endline (string_of_lpsol lpsol) in *)
  let lfsol = time (fun () -> Inverse.invert lfsig metadata (!fvar_types) lpsol) "invert term" in
  (print_endline "\nThe answer substitution:";
   print_endline (PrintLF.string_of_solution_implicit types objmap lfsol))
