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

(*i $Id: bibtex_lexer.mll,v 1.19 2010-02-22 07:38:19 filliatr Exp $ i*)

(*s Lexer for BibTeX files. *)

{

open Lexing
open Bibtex_parser

let serious = ref false    (* if we are inside a command or not *) 

let brace_depth = ref 0

(*s To buffer string literals *)

let buffer = Buffer.create 8192

let reset_string_buffer () = 
  Buffer.reset buffer

let store_string_char c = 
  Buffer.add_char buffer c
 
let get_stored_string () =
  let s = Buffer.contents buffer in
  Buffer.reset buffer;
  s

let start_delim = ref ' '

let check_delim d = match !start_delim, d with
  | '{', '}' | '(', ')' -> ()
  | _ -> failwith "closing character does not match opening"

}

let space = [' ' '\t' '\r' '\n']

rule token = parse
  | space +
      { token lexbuf }
  | '@' space* 
    ([^ ' ' '\t' '\n' '\r' '{' '(']+ as entry_type) space* 
    (('{' | '(') as delim) space*
       { serious := true; 
	 start_delim := delim; 
	 match String.lowercase entry_type with 
	   | "string" -> 
	       Tabbrev
	   | "comment" -> 
	       reset_string_buffer ();
               comment lexbuf;
               serious := false;
               Tcomment (get_stored_string ())
	   | "preamble" -> 
	       Tpreamble
	   |  et -> 
		Tentry (entry_type, key lexbuf) 
       }
  | '=' { if !serious then Tequal else token lexbuf }
  | '#' { if !serious then Tsharp else token lexbuf }
  | ',' { if !serious then Tcomma else token lexbuf }
  | '{' { if !serious then begin
	    reset_string_buffer ();
	    brace lexbuf;
	    Tstring (get_stored_string ())
          end else
	    token lexbuf }
  | ('}' | ')') as d
        { if !serious then begin
	    check_delim d;
	    serious := false;
	    Trbrace
	  end else
	    token lexbuf }
  | [^ ' ' '\t' '\n' '\r' '{' '}' '(' ')' '=' '#' ',' '"' '@']+
      { if !serious then
	  Tident (Lexing.lexeme lexbuf)
      	else
          token lexbuf }
  | "\""
      { if !serious then begin
	  reset_string_buffer ();
          string lexbuf;
          Tstring (get_stored_string ())
	end else
	  token lexbuf }
  | eof { EOF }
  | _   { token lexbuf }

and string = parse
  | '{'
      { store_string_char '{';
      	brace lexbuf;
	store_string_char '}';
	string lexbuf
      }  
  | '"'
      { () }
  | "\\\""
      { store_string_char '\\';
        store_string_char '"';
	string lexbuf}
  | eof
      { failwith "unterminated string" }
  | _
      { let c = Lexing.lexeme_char lexbuf 0 in
	store_string_char c;
        string lexbuf }

and brace = parse
  | '{'
      { store_string_char '{';
      	brace lexbuf;
	store_string_char '}';
	brace lexbuf
      }
  | '}'
      { () }
  | eof
      { failwith "unterminated string" }
  | _
      { let c = Lexing.lexeme_char lexbuf 0 in
	store_string_char c;
        brace lexbuf }

and key = parse
  | [^ ' ' '\t' '\n' '\r' ',']+ 
    { lexeme lexbuf }
  | eof 
  | _ 
    { raise Parsing.Parse_error }

and comment = parse
  | '{'
      { comment lexbuf; comment lexbuf }
  | [^ '}' '@'] as c
      { store_string_char c;
        comment lexbuf }
  | eof 
      { () }
  | _ 
      { () }
