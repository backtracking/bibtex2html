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
  | "exists"              { EXISTS }
  | "&"                   { AND }
  | "|"                   { OR }
  | "!"                   { NOT }
  | "?"                   { EXISTS }
  | ':'                   { COLON }
  | '('                   { LPAR }
  | ')'                   { RPAR }
  | "$key"                { DOLLAR_KEY }
  | "$type"               { DOLLAR_TYPE }
  | (">" | "<" | ">=" | "<=" | "=" | "<>") 
                          { COMP(Lexing.lexeme lexbuf) }
  | ['0'-'9']+            { INT(Lexing.lexeme lexbuf) }
  | ['A'-'Z' 'a'-'z'] +   { IDENT(Lexing.lexeme lexbuf) }
  | '"'                   { Buffer.clear string_buf; STRING(string lexbuf) }
  | '\''                  { Buffer.clear string_buf; STRING(string2 lexbuf) }
  | eof                   { EOF }
  | _                     { raise 
			      (Lex_error 
				 ("Invalid character " ^ 
				  (Lexing.lexeme lexbuf))) }

and string = parse
    '"'                   { Buffer.contents string_buf }
  | eof                   { raise (Lex_error ("Unterminated string")) }
  | '\\' '"'              { Buffer.add_string
			      string_buf (Lexing.lexeme lexbuf);
			    string lexbuf }
  | _                     { Buffer.add_char
			      string_buf (Lexing.lexeme_char lexbuf 0);
			    string lexbuf }
 
and string2 = parse
    '\''                  { Buffer.contents string_buf }
  | eof                   { raise (Lex_error ("Unterminated string")) }
  | '\\' '\''             { Buffer.add_string
			      string_buf (Lexing.lexeme lexbuf);
			    string2 lexbuf }
  | _                     { Buffer.add_char 
			      string_buf (Lexing.lexeme_char lexbuf 0);
			    string2 lexbuf }

