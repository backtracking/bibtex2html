(**************************************************************************)
(*  bibtex2html - A BibTeX to HTML translator                             *)
(*  Copyright (C) 1997-2010 Jean-Christophe Filliâtre and Claude Marché   *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(**************************************************************************)

{

  open Condition_parser

  exception Lex_error of string;;

  let string_buf = Buffer.create 79

  let char_of_string s =
    try
      char_of_int (int_of_string s)
    with
	_ -> raise (Lex_error ("invalid character code"))

}

let digit = ['0' - '9']

let special_char = ['$' '^' '.' '*' '+' '?' '[' ']' 'b' '|' '(' ')' '\\']

let letter = [ 'A'-'Z' 'a'-'z' ]

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
  | (">" | "<" | ">=" | "<=" | "=" | "<>" | "==" | "!=") 
                          { COMP(Lexing.lexeme lexbuf) }
  | digit +               { INT(Lexing.lexeme lexbuf) }
  | (letter | '_') (letter | digit | '_' | '-') *   
                          { IDENT(Lexing.lexeme lexbuf) }
  | '"'                   { Buffer.clear string_buf; STRING(string lexbuf) }
  | '\''                  { Buffer.clear string_buf; STRING(string2 lexbuf) }
  | eof                   { EOF }
  | _                     { raise 
			      (Lex_error 
				 ("Invalid character " ^ 
				  (Lexing.lexeme lexbuf))) }

and string = parse
    '"'                   
      { Buffer.contents string_buf }
  | eof                   
      { raise (Lex_error ("Unterminated string")) }
  | '\\' '"'              
      { Buffer.add_char string_buf '"';
	string lexbuf }
  | '\\' 'r'              
      { Buffer.add_char string_buf '\r';
	string lexbuf }
  | '\\' 'n'              
      { Buffer.add_char string_buf '\n';
	string lexbuf }
  | '\\' 't'              
      { Buffer.add_char string_buf '\t';
	string lexbuf }
  | '\\' special_char    
      { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
	string lexbuf }
  | '\\' (digit digit digit as s) 
      { Buffer.add_char string_buf (char_of_string s);
	string lexbuf }
  | '\\' _ 
      { raise (Lex_error ("Invalid escape character " ^ 
			  (Lexing.lexeme lexbuf))) }
  | _                     
      { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0);
	string lexbuf }
 
and string2 = parse
  |  '\''                 
      { Buffer.contents string_buf }
  | eof                  
      { raise (Lex_error ("Unterminated string")) }
  | '\\' '\''   
      { Buffer.add_char string_buf '\'';
	string2 lexbuf }
  | '\\' 'r'   
      { Buffer.add_char string_buf '\r';
	string2 lexbuf }
  | '\\' 'n'             
      { Buffer.add_char string_buf '\n';
	string2 lexbuf }
  | '\\' 't' 
      { Buffer.add_char string_buf '\t';
	string2 lexbuf }
  | '\\' special_char  
      { Buffer.add_string string_buf (Lexing.lexeme lexbuf);
	string2 lexbuf }
  | '\\' (digit digit digit as s)
      { Buffer.add_char string_buf (char_of_string s);
	string2 lexbuf }
  | '\\' _
      { raise (Lex_error ("Invalid escape character " ^ 
			  (Lexing.lexeme lexbuf))) }
  | _                     
      { Buffer.add_char string_buf (Lexing.lexeme_char lexbuf 0);
	string2 lexbuf }

