(*
 * bibtex2html - A BibTeX to HTML translator
 * Copyright (C) 1997-2000 Jean-Christophe Filliâtre and Claude Marché
 * 
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU General Public License version 2 for more details
 * (enclosed in the file GPL).
 *)

(*i $Id: bibtex_lexer.mll,v 1.9 2001-02-21 09:51:52 filliatr Exp $ i*)

(*s Lexer for BibTeX files. *)

{
open Bibtex_parser

let serious = ref false    (* if we are inside a command or not *) 

let brace_depth = ref 0

let line = ref 0

let reset () = line := 0

(*s To buffer string literals *)

let buffer = Buffer.create 8192

let reset_string_buffer () = 
  Buffer.reset buffer

let store_string_char c =
  if c = '\n' then incr line; 
  Buffer.add_char buffer c
 
let get_stored_string () =
  let s = Buffer.contents buffer in
  Buffer.reset buffer;
  s

}

rule token = parse
  | [' ' '\t'] +
      { token lexbuf }
  | '\n' { incr line; token lexbuf }
  | '@' { serious := true ; token lexbuf }
  | '=' { if !serious then Tequal else token lexbuf }
  | '#' { if !serious then Tsharp else token lexbuf }
  | ',' { if !serious then Tcomma else token lexbuf }
  | '{' | '('
        { if !serious then begin
	    incr brace_depth ; 
            if !brace_depth = 1 then
	      Tlbrace 
	    else begin
	      reset_string_buffer();
	      brace lexbuf;
	      Tstring (get_stored_string())
	    end 
          end else
	    token lexbuf }
  | '}' | ')'
        { if !serious then begin
	    if !brace_depth > 0 then decr brace_depth ;
	    if !brace_depth = 0 then serious := false ;
	    Trbrace
	  end else
	    token lexbuf }
  | (['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ':' '-' '+' '?' '.' '*' '&' '/'
      '\192'-'\214' '\216'-'\246' '\248'-'\255']) +
      { if !serious then
	  let s = Lexing.lexeme lexbuf in 
          match String.uppercase s with
              "STRING" -> Tabbrev
	    | "COMMENT" -> Tcomment
	    | "PREAMBLE" -> Tpreamble
	    | _ -> Tident s 
      	else
          token lexbuf }
  | "\""
      { if !serious then begin
	  reset_string_buffer();
          string lexbuf;
          Tstring (get_stored_string())
	end else
	  token lexbuf }
  | eof { EOF }
  | _   { token lexbuf }

and string = parse
  | '"'
      { () }
  | "\\\""
      { store_string_char '\\';
        store_string_char '"';
	string lexbuf}
  | eof
      { failwith "unterminated string" }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and brace = parse
  | '{'
      { incr brace_depth;
        store_string_char '{';
      	brace lexbuf
      }
  | '}'
      {  decr brace_depth;
	 if !brace_depth > 1 then begin
	   store_string_char '}';
	   brace lexbuf
	 end
      }
  | eof
      { failwith "unterminated string" }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        brace lexbuf }
