(** Frontend for the tjtwelf tool *)

(* the signature file to parse. Should become a parameter. *)
let inputName = ref ""
let outputName = ref "top"
                     
let minSolutions = ref 0
let maxSolutions = ref max_int
let batch = ref false
let queryStrings = ref []
let heapSize = ref 0
let path = ref "./"
                       
let addQuery str = queryStrings := !queryStrings @ [str]

let setPath p =
  path := p
                                                     
let setInputName ?(filter=(fun x -> x)) name =
  if !inputName = "" 
  then
    inputName := filter name
  else
    (prerr_endline "Error: More than one input file specified.";
     exit 1)

let checkInput () =
  if !inputName = ""
  then
    (prerr_endline "Error: No input file specified.";
     exit 1)
  else 
    if Sys.file_exists !inputName
    then
      ()
    else
      (prerr_endline ("Error: Invalid input file: `"^ !inputName ^"'.");
       exit 1)

(* NOTE: should figure out valid module names in Teyjus and check here *)        
let setOutputName name =
  outputName := name    

let specList = Parseargs.dualArgs
  [("-s", "--sig", Arg.String setInputName,
      " Specifies name of the input signature.") ;
   ("-o", "--output", Arg.String setOutputName,
      " Specifies name of the generated Lambda Prolog module. Default is `top'.") ;
   ("-t", "--translation", Arg.String(Translator.set_translation),
      " Specifies the desired translation. Options: naive, optimized. Default is `optimized'.") ;
   ("-index", "--opt-index", Arg.Bool(Optimization.Swap.set),
      " Set whether the indexing optimization is run. Default is true.") ;
   ("-specialize", "--opt-specialize", Arg.Bool(Optimization.Specialize.set),
      " Set whether the specialized predicates optimization is run. Default is true.") ;
   ("-b", "--batch", Arg.Set batch,
    " Suppress system interaction; send all output without stopping") ;
   ("-q", "--query", Arg.String addQuery,
    " Solve the given query on startup. Several queries may be specified.") ;
   ("-e", "--expect", Arg.Set_int minSolutions,
        " Expect at least this many solutions from each query;\n" ^
      "\t\terror if fewer. Valid only in batch mode") ;
   ("-m", "--maximum", Arg.Set_int maxSolutions,
        " Halt after this many solutions to the query have been found.\n" ^
      "\t\tValid only in batch mode") ;
   ("-p", "--path", Arg.String setPath,
    " Add PATH to the search path.") ;
   ("-k", "--heap", Arg.Set_int heapSize,
    " Allocate a heap of the given size (K)") ]

let usageMsg = 
  "Usage: tjtwelf [options] <signature-name>\n" ^
    "options are: "

let parse_args () =
  Arg.parse specList setInputName usageMsg 



            


(** main *)
let _ =
  let _ = parse_args () in
  let _ = checkInput () in
  (* parse LF signature *)
  let res = ParseTwelf.parse_sig (!inputName) in
  let sign =
    (match res with
         Some(s) -> s
       | None -> prerr_endline "\nError: Failed to parse signature.\n";
                 exit 1) in
  let (currmod, md) = LfSetup.setup_system path heapSize sign outputName in

  (* Query solving in batch mode. interaction is suppressed *)
  let solveQueryBatch () =
    let rec solveQueryBatchAux numResults =
      if Query.solveQuery () && numResults < !maxSolutions then
        (Lfquery.show_answers currmod sign md;
         solveQueryBatchAux (numResults + 1))
      else
         numResults
    in
    if Query.queryHasVars () then
      let numResults = solveQueryBatchAux 0 in
      if numResults < !minSolutions then
        Parseargs.error "fewer answers than expected"
      else ()
    else (* query does not have free variables *)
      if Query.solveQuery () then 
        if !minSolutions > 1 then 
          Parseargs.error "fewer answers than expected"
        else ()
      else 
        if !minSolutions > 0 then
          Parseargs.error "fewer answers than expected"
        else ()
  in
  (* Query solving interaction loop
   finds a solution for query and prints out. *)
  let rec solveQueryInteract () =
    let rec moreAnswers () =
      print_string "\nMore solutions (y/n)? ";
      match read_line () with
        | "y" -> true
        | "n" -> false
        | _ ->
            print_endline ("\nSorry, only options are `y' or `n'.\n" ^ 
                           "Let's try it again:");
            moreAnswers ()
    in
    if (Query.solveQuery ()) then
      if (Query.queryHasVars ()) then
        (Lfquery.show_answers currmod sign md; 
         if (moreAnswers ()) then
           solveQueryInteract ()
         else
           print_endline "\nyes\n")
      else
        print_endline "\nyes\n"
    else
      print_endline "\nno (more) solutions\n"
  in
  let solveQuery query =
    (match query with
         Some(Lfabsyn.Query(_,_,Lfabsyn.PiType(_,_,_,_))) ->
           prerr_endline "Error: Only queries which are base types are supported at this time."
       | Some(lfquery) -> 
(*           print_endline ("LF query: " ^ PrintLF.string_of_query' lfquery); *)
           if Lfquery.submit_query lfquery md (Absyn.getModuleKindTable currmod) (Absyn.getModuleConstantTable currmod) then 
             (if !batch
              then solveQueryBatch ()
              else solveQueryInteract () )
           else
             prerr_endline ""
       | None -> prerr_endline "\nError: Failed to parse query.\n");
    Module.cleanModule (); 
    Front.simulatorReInit false ;
    Module.initModuleContext ()  
  in
  (** solveQueries: solve the command line queries and
    enter interactive mode *)
  let solveQueries () =
    if !batch then
      List.iter (fun s -> solveQuery (ParseTwelf.parse_queryStr s)) !queryStrings
    else
      (List.iter (fun s -> solveQuery (ParseTwelf.parse_queryStr s)) !queryStrings;
       (* enter interactive mode *)
       while true do 
    (*    let _ = print_string ("[" ^ "top" ^"] ?- ") in *)
         solveQuery (ParseTwelf.parse_queryT ()) 
       done )
  in
  solveQueries ()
