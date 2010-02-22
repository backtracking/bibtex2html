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

(*s [(read_entries_from_file f)] returns the BibTeX entries of the
    BibTeX file [f]. *)

open Printf

let read_entries_from_file f =
  if not !Options.quiet then begin
    if f = "" then 
      eprintf "Reading from standard input...\n"
    else
      eprintf "Reading %s..." f; 
    flush stderr
  end;
  let chan = if f = "" then stdin else open_in f in
  let lb = Lexing.from_channel chan in
  try
    let el = Bibtex_parser.command_list Bibtex_lexer.token lb in
    if f <> "" then close_in chan;
    if not !Options.quiet then begin
      eprintf "ok (%d entries).\n" (Bibtex.size el); flush stderr
    end;
    el
  with Parsing.Parse_error | Failure "unterminated string" ->
    if f <> "" then close_in chan;
    eprintf "Parse error character %d, in or after entry '%s'.\n" 
      (Lexing.lexeme_start lb) !Bibtex.current_key;
    flush stderr;
    exit 1 
