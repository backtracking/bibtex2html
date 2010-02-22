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

(*i $Id: bbl_lexer.mll,v 1.9 2010-02-22 07:38:19 filliatr Exp $ i*)

(*s Lexer to analyze \verb!.bbl! files. *)

{

open Lexing

exception End_of_biblio

let opt_ref = ref None

let key = ref ""

let brace_depth = ref 0

let buf = Buffer.create 1024

}

rule biblio_header = parse
  | "\\begin{thebibliography}" '{' [^ '}']* '}'
      { biblio_name lexbuf }
  | eof
      { raise End_of_file }
  | _ 
      { biblio_header lexbuf }

and biblio_name = parse
  | '[' [^ ']']* ']'
      { let l = lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
        Some s }
  | _
      { None } 

and bibitem = parse
  | "\\end{thebibliography}"
      { raise End_of_biblio }
  | '\\' ['a'-'z']* "bibitem"
      { brace_depth := 0;
	begin try bibitem1 lexbuf 
	      with Failure "lexing: empty token" -> opt_ref := None end;
        bibitem2 lexbuf }
  | _ { bibitem lexbuf }

and bibitem1 = parse
  | '[' { Buffer.reset buf; opt_ref := Some (bibitem1_body lexbuf) }

and bibitem1_body = parse
  | ']'   { Buffer.contents buf }
  | "%\n" { bibitem1_body lexbuf }
  | _     { Buffer.add_char buf (lexeme_char lexbuf 0); bibitem1_body lexbuf }
  | eof   { raise End_of_file }

and bibitem2 = parse
  | '{' { Buffer.reset buf; 
	  key := bibitem2_body lexbuf;
	  skip_end_of_line lexbuf;
	  Buffer.reset buf;
	  bibitem_body lexbuf }

and bibitem2_body = parse
  | '}'   { Buffer.contents buf }
  | "%\n" { bibitem2_body lexbuf }
  | _     { Buffer.add_char buf (lexeme_char lexbuf 0); bibitem2_body lexbuf }
  | eof   { raise End_of_file }

and bibitem_body = parse
  | "\n\n"
      { let s = Buffer.contents buf in (!opt_ref, !key, s) }
  | eof
      { raise End_of_file }
  | "\\%" { Buffer.add_string buf "\\%"; bibitem_body lexbuf }
  | "%\n" { bibitem_body lexbuf }
  | _     { Buffer.add_char buf (lexeme_char lexbuf 0); bibitem_body lexbuf }

and skip_end_of_line = parse
  | [' ' '\n' '\010' '\013' '\009' '\012'] +
      { () }
  | _ { skip_end_of_line lexbuf }
