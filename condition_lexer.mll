{

  open Condition_parser

  exception Lex_error of string;;

  let string_buf = Buffer.create 79

}

rule token = parse
    [' ' '\t' '\n'] +     { token lexbuf }
  | "and"                 { AND }
  | "or"                  { OR }
  | "not"                 { NOT }
  | ':'                   { COLON }
  | '('                   { LPAR }
  | ')'                   { RPAR }
  | '$'                   { AT }
  | (">" | "<" | ">=" | "<=" | "=" | "<>") 
                          { COMP(Lexing.lexeme lexbuf) }
  | ['0'-'9']+            { INT(Lexing.lexeme lexbuf) }
  | ['A'-'Z' 'a'-'z'] +   { IDENT(Lexing.lexeme lexbuf) }
  | '"'                   { Buffer.clear string_buf; STRING(string lexbuf) }
  | eof                   { EOF }
  | _                     { raise 
			      (Lex_error 
				 ("Invalid character " ^ 
				  (Lexing.lexeme lexbuf))) }

and string = parse
    '"'                   { Buffer.contents string_buf }
  | eof                   { raise (Lex_error ("Unterminated string")) }
  | [^ '"'] +             { Buffer.add_string 
			      string_buf (Lexing.lexeme lexbuf);
			    string lexbuf }
