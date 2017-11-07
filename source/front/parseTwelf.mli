(* Parse: 
     contains functions for parsing implicit LF signatures (and queries) 
     and generating the explicit LF signature (and queries) 
*)

val parse_sig : string -> Lfsig.signature option
val parse_queryT : unit -> Lfabsyn.query option
val parse_queryStr : string -> Lfabsyn.query option
