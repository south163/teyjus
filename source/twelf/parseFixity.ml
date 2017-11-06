let fixToString (Fixity.Strength(p)) = string_of_int p
	    
    (* idToPrec (region, (idCase, name)) = n
       where n is the precedence indicated by name, which should consists
       of all digits.  Raises error otherwise, or if precedence it too large
    *)
let idToPrec (r, (_, name)) =
  let prec =
    try
      Fixity.Strength (Tparsing.Parsing.Lexer'.stringToNat (name)) 
    with Tparsing.Parsing.Lexer'.NotDigit _ -> Tparsing.Parsing.error (r, "Precedence not a natural number")
(* what is overflow exception in ocaml?
        Overflow -> Parsing.error (r, "Precedence too large")
       | Tparsing.Parsing.Lexer'.NotDigit _ -> Parsing.error (r, "Precedence not a natural number")
*)
   in
   if Fixity.less(prec, Fixity.minPrec) || Fixity.less (Fixity.maxPrec, prec)
   then Tparsing.Parsing.error (r, "Precedence out of range ["
			      ^ fixToString Fixity.minPrec ^ ","
			      ^ fixToString Fixity.maxPrec ^ "]")
   else prec

    (*-----------------------------*)
    (* Parsing fixity declarations *)
    (*-----------------------------*)

    (* parseFixCon "id" *)
let parseFixCon args =
  match args with
    (fixity, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, name), r), s')) ->
      (((Names.Qid ([],name),r), fixity), Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (fixity, Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
    Tparsing.Parsing.error (r, "Expected identifier to assign fixity, found " ^ Tparsing.Parsing.Lexer'.toString t)

    (* parseFixPrec "n id" where n is precedence *)
let parseFixPrec args =
  match args with
    (fixity, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID(a,b), r), s')) ->
      parseFixCon (fixity (idToPrec (r, (a,b))), Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (fixity, Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
      Tparsing.Parsing.error (r, "Expected precedence, found " ^ Tparsing.Parsing.Lexer'.toString t)

    (* parseInfix "none|left|right n id" where n is precedence *)
let parseInfix args =
  match args with
    (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (Tparsing.Parsing.Lexer'.Lower, "none"), r), s')) ->
      parseFixPrec ((fun p -> Fixity.Infix(p, Fixity.None)), Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (Tparsing.Parsing.Lexer'.Lower, "left"), r), s')) ->
      parseFixPrec ((fun p -> Fixity.Infix(p, Fixity.Left)), Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (Tparsing.Parsing.Lexer'.Lower, "right"), r), s')) ->
      parseFixPrec ((fun p -> Fixity.Infix(p, Fixity.Right)), Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) ->
      Tparsing.Parsing.error (r, "Expected associatitivy `left', `right', or `none', found "
			    ^ Tparsing.Parsing.Lexer'.toString t)

    (* parsePrefix "n id" where n is precedence *)
let parsePrefix (f) = parseFixPrec ((fun x -> Fixity.Prefix x), f)

    (* parsePostfix "n id" where n is precedence *)
let parsePostfix (f) = parseFixPrec ((fun x -> Fixity.Postfix x), f)

    (* parseFixity' : lexResult stream -> (name,fixity) * lexResult stream
       Invariant: token stream starts with %infix, %prefix or %postfix
    *)
let parseFixity' args =
  match args with
    (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.INFIX, r), s')) -> parseInfix (Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.PREFIX, r), s')) -> parsePrefix (Tparsing.Parsing.Lexer'.Stream'.expose s')
  | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.POSTFIX, r), s')) -> parsePostfix (Tparsing.Parsing.Lexer'.Stream'.expose s')
      (* anything else should be impossible *)

let parseFixity (s) = parseFixity' (Tparsing.Parsing.Lexer'.Stream'.expose (s))

(*
        
    (*------------------------------------*)
    (* Parsing name preferences %name ... *)
    (*------------------------------------*)

    (* parseName5 "string ... )" or ")" *)
    fun parseName5 (name, r0, prefENames, prefUNames, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, prefUName), r), s')) =
        (* prefUName should be lower case---not enforced *)
        parseName5 (name, r0, prefENames, prefUNames @ [prefUName] , Tparsing.Parsing.Lexer'.Stream'.expose s')	
      | parseName5 (name, r0, prefENames, prefUNames, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RPAREN, r), s')) = 
	(((Names.Qid ([], name), r0), (prefENames, prefUNames)), Tparsing.Parsing.Lexer'.Stream'.expose s')
      | parseName5 (name, r0, prefENames, prefUNames, Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) =
	  Tparsing.Parsing.error (r, "Expected name preference or ')', found " ^ Tparsing.Parsing.Lexer'.toString t)

    (* parseName3 "string" or "" *)
    fun parseName3 (name, r0, prefEName, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, prefUName), r), s')) =
        (* prefUName should be lower case---not enforced *)
        (((Names.Qid ([], name), r0), (prefEName, [prefUName])), Tparsing.Parsing.Lexer'.Stream'.expose s')
      | parseName3 (name, r0, prefEName, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.LPAREN, r), s')) = 
	parseName5 (name, r0, prefEName, [], Tparsing.Parsing.Lexer'.Stream'.expose s')
      | parseName3 (name, r0, prefEName, f) =
	(((Names.Qid ([], name), r0), (prefEName, [])), f)

    (* parseName4 "string ... )" or ")" *)
    fun parseName4 (name, r0, prefENames, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, prefEName), r), s')) =
        if Tparsing.Parsing.Lexer'.isUpper prefEName
	  then parseName4 (name, r0,  prefENames @ [prefEName] , Tparsing.Parsing.Lexer'.Stream'.expose s')
	else Tparsing.Parsing.error (r, "Expected uppercase identifer, found " ^ prefEName)
      | parseName4 (name, r0, prefENames, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RPAREN, r), s')) = 
          parseName3 (name, r0, prefENames, Tparsing.Parsing.Lexer'.Stream'.expose s')
      | parseName4 (name, r0, prefENames, Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) =
	  Tparsing.Parsing.error (r, "Expected name preference or ')', found " ^ Tparsing.Parsing.Lexer'.toString t)

    (* parseName2 "string" or "string string" 
              or "(string ... ) string"  or " string (string ...)" 
              or "(string ... ) (string ...)" *)
    fun parseName2 (name, r0, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, prefEName), r), s')) =
        if Tparsing.Parsing.Lexer'.isUpper prefEName
	  then parseName3 (name, r0, [prefEName], Tparsing.Parsing.Lexer'.Stream'.expose s')
	else Tparsing.Parsing.error (r, "Expected uppercase identifer, found " ^ prefEName)
      | parseName2 (name, r0, Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.LPAREN, r), s')) = 
	parseName4 (name, r0, [], Tparsing.Parsing.Lexer'.Stream'.expose s')
      | parseName2 (name, r0, Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) =
	  Tparsing.Parsing.error (r, "Expected name preference, found " ^ Tparsing.Parsing.Lexer'.toString t)

    (* parseName1 "id string" or "id string string" *)
    fun parseName1 (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (_, name), r), s')) =
          parseName2 (name, r, Tparsing.Parsing.Lexer'.Stream'.expose s')
      | parseName1 (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s')) =
	  Tparsing.Parsing.error (r, "Expected identifer to assign name preference, found " ^ Tparsing.Parsing.Lexer'.toString t)

    (* parseNamePref' "%name id string" or "%name id string string"
       Invariant: token stream starts with %name
    *)
let parseNamePref' (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.NAME, r), s')) = parseName1 (Tparsing.Parsing.Lexer'.Stream'.expose s')

let parseNamePref (s) = parseNamePref' (Tparsing.Parsing.Lexer'.Stream'.expose s)
  
*)
