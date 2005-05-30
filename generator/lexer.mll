(* $Id: lexer.mll,v 1.2 2001/01/10 00:30:43 gerd Exp $
 *)

{
  open Parser;;
  exception Error;;
}


rule token = parse
    '['  { LBRACKET }
  | ']'  { RBRACKET }
  | '<'  { LANGLE }
  | '>'  { RANGLE }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | '*'  { STAR }
  | ','  { COMMA }
  | ';'  { SEMICOLON }
  | ':'  { COLON }
  | "=>" { MAPSTO }
  | '='  { EQUAL }
  | [ 'a'-'z' 'A'-'Z' ] [ 'a'-'z' 'A'-'Z' '_' '0'-'9']*
    { 
      let s = Lexing.lexeme lexbuf in
      match s with
	  "opaque"     -> K_opaque
	| "string"     -> K_string
	| "void"       -> K_void
	| "unsigned"   -> K_unsigned
	| "int"        -> K_int
	| "hyper"      -> K_hyper
	| "float"      -> K_float
	| "double"     -> K_double
	| "quadruple"  -> K_quadruple
	| "bool"       -> K_bool
	| "enum"       -> K_enum
	| "struct"     -> K_struct
	| "union"      -> K_union
	| "switch"     -> K_switch
	| "case"       -> K_case
	| "default"    -> K_default
	| "const"      -> K_const
	| "typedef"    -> K_typedef
	| "program"    -> K_program
	| "version"    -> K_version
	| _            -> IDENT s
    }
  | "_abstract" { K_abstract }
  | "_unboxed"  { K_unboxed }
  | "_int32"    { K_int32 }
  | "_int64"    { K_int64 }
  | '-'? ['1'-'9'] ['0'-'9']*                      (* C-style decimal number *)
    { INTLITERAL (Lexing.lexeme lexbuf) }
  | '-'? '0' ['0'-'7']*                            (* C-style octal number *)
    { INTLITERAL (Lexing.lexeme lexbuf) }
  | '-'? '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+  (* C-style hex number *)
    { INTLITERAL (Lexing.lexeme lexbuf) }
  | '#' [' ' '\t']* ['0'-'9']+ [' ' '\t']+ '"' [^'"']* '"' [' ' '\t' '0'-'9']* '\n'
      (* Preprocessor line number directive *)
      { let s = Lexing.lexeme lexbuf in
	let k = ref 0 in
	let n = ref (-1) in
	let j = ref 0 in
	let f = ref "" in
	for i = 0 to String.length s - 1 do
	  if !k = 0 && s.[i] >= '0' && s.[i] <= '9' then
	    k := i
	  else
	    if !n = (-1) && !k > 0 && (s.[i] = ' ' || s.[i] = '\t') then
	      n := int_of_string (String.sub s !k (i - !k))
	    else
	      if !j = 0 && s.[i] = '"' then
		j := i+1
	      else
		if !j <> 0 && s.[i] = '"' then
		  f := String.sub s !j (i - !j)
	done;
	SETFILE(!n, !f)
      }
  | '%'                 { PERCENT }
  | '\n'                { LINEFEED(1,0) }
  | [ ' ' '\t' '\r' ]+  { IGNORE }
  | "/*" ([^ '*'] | ('*'+ [^ '/']))* "*/" 
      { let s = Lexing.lexeme lexbuf in
	let n = ref 0 in
	let m = ref 0 in
	for i = 0 to String.length s - 1 do
	  if s.[i] = '\n' then (incr n; m := i)
	done;
	if !n = 0 then
	  IGNORE
	else
	  LINEFEED(!n, String.length s - !m)
      }
  | eof                 { EOF }
  | _                   { raise Error }

and ignore_line = parse
  | [^ '\n']* '\n' { LINEFEED(1,0) }
  | [^ '\n']*      { IGNORE }

(* ======================================================================
 * 
 * $Log: lexer.mll,v $
 * Revision 1.2  2001/01/10 00:30:43  gerd
 * 	Continued: int mapping, C preprocessor, % lines
 *
 * Revision 1.1  2001/01/08 01:47:58  gerd
 * 	Initial revision.
 *
 *)

