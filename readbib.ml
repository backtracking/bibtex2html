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

(* $Id: readbib.ml,v 1.7 2000-06-02 19:37:37 filliatr Exp $ *)

(* [(read_entries_from_file f)] returns the BibTeX entries of the
   BibTeX file [f] (from standard input if [f=""]).  *)

let read_entries_from_file f =
  if f = "" then 
    Printf.eprintf "Reading from standard input...\n"
  else
    Printf.eprintf "Reading %s..." f; 
  flush stderr;
  Bibtex_lexer.reset();
  let chan = if f = "" then stdin else open_in f in
  try
    let el =
      Bibtex_parser.command_list Bibtex_lexer.token (Lexing.from_channel chan)
    in
    if f <> "" then close_in chan;
    Printf.eprintf "ok (%d entries).\n" (Bibtex.size el); flush stderr;
    el
  with
      Parsing.Parse_error | Failure "unterminated string" ->
	if f<>"" then close_in chan;
	Printf.eprintf "Parse error line %d.\n" !Bibtex_lexer.line;
	flush stderr;
	exit 1 
