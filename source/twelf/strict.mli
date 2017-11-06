  exception Error of string
  
  val check : (IntSyn.exp * IntSyn.exp) * Paths.occConDec option -> unit
    
  val checkType : (int * IntSyn.exp) * Paths.occConDec option -> unit
