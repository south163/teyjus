(* This is taken from the Twelf implementation *)

  exception Error of string

  val condecToConDec : ExtConDec.condec * Paths.location * bool -> IntSyn.conDec option * Paths.occConDec option
                     (* optional ConDec is absent for anonymous definitions *)
                     (* bool = true means that condec is an abbreviation *)
