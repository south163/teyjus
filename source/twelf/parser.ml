(* This is taken from the Twelf implementation *)

  type fileParseResult =
      ConDec of ExtConDec.condec
    | AbbrevDec of ExtConDec.condec
    | FixDec of (Names.qid * Paths.region) * Fixity.fixity
    | FQuery of ExtSyn.query (* A *)
    (* Further declarations to be added here *)
 
  let stripDot args =
    match args with
        (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DOT, r), s)) -> s
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RPAREN, r), s)) ->
          Tparsing.Parsing.error (r, "Unexpected right parenthesis")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACE, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected right brace")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.RBRACKET, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected right bracket")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected end of file")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EQUAL, r), s)) -> 
          Tparsing.Parsing.error (r, "Unexpected `='")
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t, r), s)) -> 
          Tparsing.Parsing.error (r, "Expected `.', found " ^ Tparsing.Parsing.Lexer'.toString t)
      (* Everything else should be impossible *)

  let rec parseTLStream instream =
    let finish args =
      match args with
          (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, r), s)) -> Tparsing.Parsing.Lexer'.Stream'.Empty
        | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACE, r), s)) ->
            Tparsing.Parsing.error (r, "Unmatched `}'")
    in
    Tparsing.Parsing.Lexer'.Stream'.delay (fun () -> parseStream'' (Tparsing.Parsing.Lexer'.Stream'.expose(Tparsing.Parsing.Lexer'.lexStream instream), finish))

  and parseStream' (s, sc)  =
    Tparsing.Parsing.Lexer'.Stream'.delay (fun () -> parseStream'' (Tparsing.Parsing.Lexer'.Stream'.expose s, sc))

  (* parseStream'' : lexResult front -> fileParseResult front *)
  (* parseStream'' switches between various specialized parsers *)
  and parseStream'' (f, sc)  =
    match (f, sc) with
      (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ID (idCase,name), r0), s'), sc) ->
          parseConDec' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ABBREV, r), s'), sc) -> 
          parseAbbrev' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.UNDERSCORE, r), s'), sc) -> parseConDec' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.INFIX, r), s'), sc) -> 
          parseFixity' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.PREFIX, r), s'), sc) -> 
          parseFixity' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.POSTFIX, r), s'), sc) -> 
          parseFixity' (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.NAME, r1), s'), sc) ->
          (*let (namePref, ((Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r2), _)) as f')) = ParseFixity.parseNamePref' f in
          let  r = Paths.join (r1, r2) in
          Tparsing.Parsing.Lexer'.Stream'.Cons ((NamePref namePref, r), parseStream' (stripDot f', sc))*)
          Tparsing.Parsing.warning (r1, " : %name  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DEFINE, r), s'), sc) ->
          (*parseSolve' (f, sc)*)
          Tparsing.Parsing.warning (r, " : %define declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.SOLVE, r), s'), sc) ->
          (*parseSolve' (f, sc)*)
          Tparsing.Parsing.warning (r, (" : %solve  declarations are not treated."));
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.QUERY, r0), s'), sc) ->
          (*let (expected, s1) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s') in
          let (tri, s2) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s1) in
          let (query, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Tparsing.Parsing.Lexer'.Stream'.expose s2) in
          let r = Paths.join (r0, r') in 
          Tparsing.Parsing.Lexer'.Stream'.Cons ((Query (expected, tri, query), r), parseStream' (stripDot f3, sc))*)
          Tparsing.Parsing.warning (r0," : %query  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.FQUERY, r0), s'), sc) ->
          let (query, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Tparsing.Parsing.Lexer'.Stream'.expose s') in
          let r = Paths.join (r0, r') in
          Tparsing.Parsing.Lexer'.Stream'.Cons ((FQuery query, r), parseStream' (stripDot f3, sc))
      | (Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.QUERYTABLED, r0), s'), sc) ->
          (*let (numSol, s1) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s') in
          let (try, s2) = parseBound' (Tparsing.Parsing.Lexer'.Stream'.expose s1) in
          let (query, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f3)) = ParseQuery.parseQuery' (Tparsing.Parsing.Lexer'.Stream'.expose s2) in
          let r = Paths.join (r0, r') in
          Tparsing.Parsing.Lexer'.Stream'.Cons ((Querytabled (numSol, try, query), r), parseStream' (stripDot f3, sc))*)
          Tparsing.Parsing.warning (r0," : %querytabled  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.MODE, r), s'), sc) -> (*parseMode' (f, sc)*)
          Tparsing.Parsing.warning (r, " : %mode  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.UNIQUE, r), s'), sc) -> (*parseUnique' (f, sc)*)
          Tparsing.Parsing.warning (r, " : %unique  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COVERS, r), s'), sc) -> (*parseCovers' (f, sc)*)
          Tparsing.Parsing.warning (r, " : %covers  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TOTAL, r), s'), sc) -> (*parseTotal' (f, sc) (* -fp *)*)
          Tparsing.Parsing.warning (r," : %total  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TERMINATES, r), s'), sc) -> (*parseTerminates' (f, sc)*)
          Tparsing.Parsing.warning (r," : %terminates  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.BLOCK, r), s'), sc) -> (*parseConDec' (f, sc) (* -cs *)*)
          Tparsing.Parsing.warning (r," : %block  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.WORLDS, r), s'), sc) -> (*parseWorlds' (f, sc)*)
          Tparsing.Parsing.warning (r," : %worlds  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.REDUCES, r), s'), sc) -> (*parseReduces' (f, sc) (* -bp *)*)
          Tparsing.Parsing.warning (r," : %reduces  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TABLED, r), s'), sc) -> (*parseTabled' (f, sc) (* -bp *)*)
          Tparsing.Parsing.warning (r," : %tabled  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.KEEPTABLE, r), s'), sc) -> (*parseKeepTable' (f, sc) (* -bp *)*)
          Tparsing.Parsing.warning (r," : %keepTable  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.THEOREM, r), s'), sc) -> (*parseTheorem' (f, sc)*)
          Tparsing.Parsing.warning (r," : %theorem  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.PROVE, r), s'), sc) -> (*parseProve' (f, sc)*)
          Tparsing.Parsing.warning (r, " : %prove  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ESTABLISH, r), s'), sc) -> (*parseEstablish' (f, sc)*)
          Tparsing.Parsing.warning (r," : %establish  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.ASSERT, r), s'), sc) -> (*parseAssert' (f, sc)*)
          Tparsing.Parsing.warning (r," : %assert  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.TRUSTME, r), s'), sc) -> (*parseTrustMe' (f, sc)*)
          Tparsing.Parsing.warning (r," : %trustme  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.FREEZE, r), s'), sc) -> (*parseFreeze' (f, sc)*)
          Tparsing.Parsing.warning (r," : %freeze  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.SUBORD, r), s'), sc) -> (*parseSubord' (f, sc)*)
          Tparsing.Parsing.warning (r," : %subord  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.THAW, r), s'), sc) -> (*parseThaw' (f, sc)*)
          Tparsing.Parsing.warning (r," : %thaw  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.DETERMINISTIC, r), s'), sc) -> (*parseDeterministic' (f, sc) (* -rv *)*)
          Tparsing.Parsing.warning (r," : %deterministic  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.COMPILE, r), s'), sc) -> (*parseCompile' (f, sc) (* -ABP 4/4/03 *)*)
          Tparsing.Parsing.warning (r," : %compile  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.CLAUSE, r), s'), sc) -> (*parseClause' (f, sc) (* -fp *)*)
          Tparsing.Parsing.warning (r," : %clause  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.SIG, r), s'), sc) -> (*parseSigDef' (f, sc)*)
          Tparsing.Parsing.warning (r," : %sig  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.STRUCT, r), s'), sc) -> (*parseStructDec' (f, sc)*)
          Tparsing.Parsing.warning (r," : %struct  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.INCLUDE, r), s'), sc) -> (*parseInclude' (f, sc)*)
          Tparsing.Parsing.warning (r," : %include  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.OPEN, r), s'), sc) -> (*parseOpen' (f, sc)*)
          Tparsing.Parsing.warning (r," : %open  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.USE, r), s'), sc) -> (*parseUse' (Tparsing.Parsing.Lexer'.Stream'.expose s', sc)*)
          Tparsing.Parsing.warning (r," : %use  declarations are not treated.");
          parseSkip (f, sc)
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.EOF, _), _), sc) -> sc f
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((Tparsing.Parsing.Lexer'.RBRACE, _), _), sc) -> sc f
      | (Tparsing.Parsing.Lexer'.Stream'.Cons ((t,r), s'), sc) ->
          Tparsing.Parsing.error (r, "Expected constant name or pragma keyword, found "
                            ^ Tparsing.Parsing.Lexer'.toString t)

  and parseConDec' (Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r0), _) as f, sc) =
    let (conDec, ((Tparsing.Parsing.Lexer'.Stream'.Cons((_,r'),_)) as f')) = ParseCondec.parseConDec' (f) in
    let r = Paths.join (r0, r') in
    match conDec with
      ExtConDec.Condef(_,_,_) ->
        Tparsing.Parsing.warning (r', " : Definitions will be treated as abbreviations and are unfolded.");
        Tparsing.Parsing.Lexer'.Stream'.Cons ((AbbrevDec conDec, r), parseStream' (stripDot f', sc))
    | _ ->
      Tparsing.Parsing.Lexer'.Stream'.Cons ((ConDec conDec, r), parseStream' (stripDot f', sc))

  and parseAbbrev' (Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r0), _) as f, sc) =
    let (conDec, (Tparsing.Parsing.Lexer'.Stream'.Cons ((_,r') ,_) as f')) = ParseCondec.parseAbbrev' (f) in
    let r = Paths.join (r0, r')	in
    Tparsing.Parsing.Lexer'.Stream'.Cons ((AbbrevDec conDec, r), parseStream' (stripDot f', sc))

  and parseFixity' (Tparsing.Parsing.Lexer'.Stream'.Cons ((_, r0), _) as f, sc) =
    let ((fdec1, fdec2), (Tparsing.Parsing.Lexer'.Stream'.Cons ((_,r'),_) as f')) = ParseFixity.parseFixity' (f) in
    let r = Paths.join (r0, r')	in
    Tparsing.Parsing.Lexer'.Stream'.Cons ((FixDec (fdec1, fdec2), r), parseStream' (stripDot f', sc))
      
  and parseSkip (f, sc)  =
    match f with
        Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.DOT,_),s') -> 
          parseStream'' ((Tparsing.Parsing.Lexer'.Stream'.expose s'), sc)
      | Tparsing.Parsing.Lexer'.Stream'.Cons((Tparsing.Parsing.Lexer'.EOF,_),_) -> 
          sc f 
      | Tparsing.Parsing.Lexer'.Stream'.Cons((t,r),s') -> 
          parseSkip (Tparsing.Parsing.Lexer'.Stream'.expose s', sc)

  let rec parseQ (s) = Tparsing.Parsing.Lexer'.Stream'.delay (fun () -> parseQ' (Tparsing.Parsing.Lexer'.Stream'.expose s))
  and parseQ' (f) =
    let (query, f') = ParseQuery.parseQuery' (f) in
    Tparsing.Parsing.Lexer'.Stream'.Cons (query, parseQ (stripDot (f'))) 
  
 
  
  let parseStream instream = parseTLStream instream

  let parseTerminalQ prompts =  parseQ (Tparsing.Parsing.Lexer'.lexTerminal prompts)
