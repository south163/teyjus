(* This is taken from the Twelf implementation *)

  type fileParseResult =
      ConDec of ExtConDec.condec
    | AbbrevDec of ExtConDec.condec
    | FixDec of (Names.qid * Paths.region) * Fixity.fixity
    | FQuery of ExtSyn.query (* A *)
    (* Further declarations to be added here *)

  val parseStream: in_channel -> (fileParseResult * Paths.region) Tparsing.Parsing.Lexer'.Stream'.stream
  val parseTerminalQ : string * string -> ExtSyn.query Tparsing.Parsing.Lexer'.Stream'.stream (* reads from std input *)
  val parseStringQ : string -> ExtSyn.query Tparsing.Parsing.Lexer'.Stream'.stream
