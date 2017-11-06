(* External Syntax for signature entries *)
  (* Author: Frank Pfenning *)

  type name = string

  type condec =				(* constant declaration *)
      Condec of name * ExtSyn.term
    | Condef of name option * ExtSyn.term * ExtSyn.term option
    | Blockdef of string *  (string list * string) list
    | Blockdec of name * ExtSyn.dec list * ExtSyn.dec list

  
  val condec : string * ExtSyn.term -> condec	(* id : tm *)
  val blockdec : string * ExtSyn.dec list * ExtSyn.dec list -> condec
  val blockdef : string *  (string list * string) list -> condec
  val condef : string option * ExtSyn.term * ExtSyn.term option -> condec
					(* id : tm = tm | _ : tm = tm *)

