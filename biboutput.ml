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

(*i $Id: biboutput.ml,v 1.9 2001-02-21 09:51:52 filliatr Exp $ i*)

(*s Output a BibTeX bibliography. *)

open Printf
open Bibtex

let needs_output k = function
  | None -> true
  | Some s -> KeySet.mem k s

let print_atom html ch keys = function
  | Id s -> 
      if html & not (abbrev_is_implicit s) then 
	begin
	  Html.open_href ch ("#" ^ s);
	  output_string ch s;
	  Html.close_href ch
	end
      else
	output_string ch s
  | String s -> 
      output_string ch ("{"^s^"}")

let print_atom_list html ch keys = function 
  | [] -> ()
  | a::l ->
      print_atom html ch keys a;
      List.iter
	(fun a -> output_string ch " # "; print_atom html ch keys a)
	l

let print_crossref html ch keys = function
  | [String s] -> 
      output_string ch "{";
      if html then Html.open_href ch ("#" ^ s);
      output_string ch s;
      if html then Html.close_href ch;
      output_string ch "}"
  | l -> 
      if not !Options.quiet then
	eprintf "Warning: cross-references must be constant strings\n";
      print_atom_list html ch keys l

let print_command html ch keys = function 
  | Comment s -> 
(*i
      if html then 
	begin
	  Html.open_balise ch "i";
	  output_string ch s;
	  Html.close_balise ch "i"
	end
      else
i*)
	output_string ch ("@COMMENT{{" ^ s ^ "}}\n\n")
  | Preamble s ->
      output_string ch ("@PREAMBLE{{" ^ s ^ "}}\n\n")
  | Abbrev(s,l) ->
      if needs_output s keys then 
	begin
	  if html then Html.open_anchor ch s;
	  output_string ch ("@STRING{" ^ s ^ " = ");
	  print_atom_list html ch keys l;
	  output_string ch "}\n\n"
	end
  | Entry (entry_type,key,fields) ->
      if needs_output key keys then 
	begin
	  if html then Html.open_anchor ch key;
	  output_string ch ("@" ^ entry_type ^ "{" ^ key);
	  List.iter
	    (fun (field,l) ->
	       output_string ch (",\n  " ^ field ^ " = ");
	       if html & field = "CROSSREF" then print_crossref html ch keys l
	       else print_atom_list html ch keys l)
	    fields;
	  output_string ch "\n}\n\n"
	end

let output_bib html ch bib keys =
  Bibtex.fold
    (fun entry () -> print_command html ch keys entry)    
    bib
    ()
