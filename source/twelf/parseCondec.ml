(* This is taken from the Twelf implementation *)

let parseConDec3 (optName, optTm, s) =
  let (tm', f') = ParseTerm.parseTerm' (Tparsing.Parsing.Lexer'.Stream'.expose s) in
  (ExtConDec.condef (optName, tm', optTm), f')

  (* parseConDec2  "= U" | "" *)
let parseConDec2 args =
  match args with
      (optName, (tm, Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.EQUAL, r), s'))) ->
        parseConDec3 (optName, Some(tm), s')
    | (Some(name), (tm, f)) ->
        ((ExtConDec.condec(name, tm)), f)
    | (None, (tm, Tparsing.Parsing.Lexer'.Stream'.Cons((t,r),s'))) ->
        Tparsing.Parsing.error (r, "Illegal anonymous declared constant")

    (* parseConDec1  ": V = U" | "= U" *)
let parseConDec1 args =
  match args with
      (optName, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COLON, r), s')) ->
        parseConDec2 (optName, ParseTerm.parseTerm' (Tparsing.Parsing.Lexer'.Stream'.expose s'))
    | (optName, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EQUAL, r), s')) ->
        parseConDec3 (optName, None, s')
    | (optName, Tparsing.Parsing.Lexer'.Stream'.Cons ((t,r), s')) ->
      Tparsing.Parsing.error (r, "Expected `:', found " ^ Tparsing.Parsing.Lexer'.toString t)

  (* parseConDec' : lexResult front -> ExtConDec.ConDec * lexResult front
     Invariant: first token in exposed input stream is an identifier or underscore
  *)
let parseConDec' args =
  match args with
      (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (idCase,name), r), s')) ->
        parseConDec1 (Some(name), Tparsing.Parsing.Lexer'.Stream'.expose s')
    | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.UNDERSCORE, r), s')) ->
        parseConDec1 (None, Tparsing.Parsing.Lexer'.Stream'.expose s')
    | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
        Tparsing.Parsing.error (r, "Constant or block declaration expected, found token " ^ Tparsing.Parsing.Lexer'.toString t)

let parseConDec (s) = parseConDec' (Tparsing.Parsing.Lexer'.Stream'.expose s)
let parseAbbrev' (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ABBREV, r), s)) = parseConDec (s)
