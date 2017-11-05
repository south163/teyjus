type name = string

type condec =
      Condec of name * ExtSyn.term
    | Condef of name option * ExtSyn.term * ExtSyn.term option
    | Blockdef of string *  (string list * string) list
    | Blockdec of name * ExtSyn.dec list * ExtSyn.dec list

let condec (a,b) = Condec(a,b)
  
let blockdec (a,b,c) = Blockdec(a,b,c)

let blockdef (a,b) = Blockdef(a,b)

let condef (a,b,c) = Condef(a,b,c)
