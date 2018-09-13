(** Use the Teyjus loader to load the bytecode file
    and set up the simulator state. 
    Followed from simulatorfront main.*)
let setup_sim path heapSize name =
  Front.systemInit  !heapSize ;
  Module.setPath    !path;
  Module.moduleLoad !name;
  Front.simulatorInit () ;
  Module.moduleInstall !name ;
  Module.initModuleContext () 

let time f dscr =
  let start = Sys.time () in
  let x = f () in
  Printf.printf "Execution Time(%s): %fs\n" dscr (Sys.time () -. start);
  x


(** Read the mapping between LF and LP constants
    from the metadata file. *)
let load_metadata name =
  let metadata = ref Metadata.empty in
  let in_chann = open_in (name ^ ".md") in
  let _ =
    try
      while true do
        let line = input_line in_chann in
        match Str.split (Str.regexp " ") line  with
            [fst; snd] ->
              metadata := Metadata.set_mapping (!metadata) (Symb.symbol fst) (Symbol.symbol snd)
          | _ -> ()
      done
    with End_of_file -> close_in in_chann
  in !metadata



  (* translate LF sig and generate files. *)
let translate_sig sign = 
    match Translator.get_translation () with
        "optimized" -> Translator.OptimizedTranslation.translate sign 
      | "original" -> Translator.OriginalTranslation.translate sign


(* Compile and link the module before loading *)
let compile_and_link name =
  let executable_dir = Filename.dirname Sys.executable_name in
  (Sys.command ((Filename.concat executable_dir "tjcc.opt") ^ " " ^ !name );
   Sys.command ((Filename.concat executable_dir "tjlink.opt") ^ " " ^ !name);
  )
    
let output_files (metadata, kinds, constants, clauses) name =
  let per_kind kind =
    let rec getKindType n =
      match n with
          0 -> "type"
        | _ -> "type -> " ^ getKindType (n-1) 
    in
    "kind " ^ (Absyn.string_of_kind kind) ^ " " ^ (getKindType (Absyn.getKindArity kind)) ^"."
  in
  let per_const constant =
    (if Absyn.getConstantExportDef constant then "exportdef " else "type ") ^ (Absyn.getConstantPrintName constant) ^ " " ^(Absyn.string_of_skeleton (Absyn.getConstantSkeletonValue constant)) ^ "."
  in
  let per_clause c =
    (Absyn.string_of_term c) ^ "."
  in
  let sigstr = Table.fold (fun s c b -> b ^ (per_const c) ^ "\n") 
                          constants 
                          ((Table.fold (fun s k b -> b ^ (per_kind k) ^ "\n") 
                                       kinds 
                                       ("sig "^ !name ^".\n%%%kind decls\n")) ^ "%%%const decls\n")
  in
  let modstr = List.fold_left (fun s t -> s ^ (per_clause t) ^ "\n")
                              ("module " ^ !name ^ ".\n")
                              clauses
  in
  let mdstr = Metadata.string_of_metadata metadata in
  let (out_md, out_sig, out_mod) = (open_out (!name^".md"), open_out (!name^".sig"), open_out (!name^".mod")) in
  Printf.fprintf out_md "%s\n" mdstr; 
  Printf.fprintf out_sig "%s\n" sigstr;
  Printf.fprintf out_mod "%s\n" modstr;
  close_out out_md;
  close_out out_sig;
  close_out out_mod
                                                         
let setup_system path heapSize sign name =
  let (md,_,_,_) as res = time (fun () -> translate_sig sign) "translate signature" in
  time (fun () -> output_files res name; compile_and_link name; setup_sim path heapSize name)
       "compile, link, and start simulator";
  ( Module.getCurrentModule (), md)
