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

(*i $Id: biboutput.ml,v 1.13 2003-10-01 15:23:24 filliatr Exp $ i*)

(*s Output a BibTeX bibliography. *)

open Printf
open Bibtex

let link_fields = ref []
let add_link_field f = link_fields := f :: !link_fields
let is_link_field f = List.mem f !link_fields

let needs_output k = function
  | None -> true
  | Some s -> KeySet.mem k s

let url_re = Str.regexp "\\(ftp\\|http\\)://.*"
let is_url s = Str.string_match url_re s 0

let print_atom html ch = function
  | Id s -> 
      if html & not (abbrev_is_implicit s) then 
	begin
	  Html.open_href ch ("#" ^ s);
	  output_string ch s;
	  Html.close_href ch
	end
      else
	output_string ch s
  | String s when html & is_url s -> 
      output_string ch "{";
      Html.open_href ch s;
      output_string ch s;
      Html.close_href ch;
      output_string ch "}"
  | String s -> 
      output_string ch ("{"^s^"}")

let print_atom_list html ch = function 
  | [] -> ()
  | a::l ->
      print_atom html ch a;
      List.iter
	(fun a -> output_string ch " # "; print_atom html ch a)
	l

let print_crossref html ch = function
  | [String s] -> 
      output_string ch "{";
      if html then Html.open_href ch ("#" ^ s);
      output_string ch s;
      if html then Html.close_href ch;
      output_string ch "}"
  | l -> 
      if not !Options.quiet then
	eprintf "Warning: cross-references must be constant strings\n";
      print_atom_list html ch l

let print_link_field ch = function
  | [String s] ->
      output_string ch "{";
      Html.open_href ch s;
      output_string ch s;
      Html.close_href ch;
      output_string ch "}"
  | l ->
      if not !Options.quiet then
	eprintf "Warning: web links must be constant strings\n";
      print_atom_list true ch l

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
  | Preamble l ->
      output_string ch "@PREAMBLE{{";
      print_atom_list html ch l;
      output_string ch "}}\n\n"
  | Abbrev(s,l) ->
      if needs_output s keys then 
	begin
	  if html then begin Html.open_anchor ch s; Html.close_anchor ch end;
	  output_string ch ("@STRING{" ^ s ^ " = ");
	  print_atom_list html ch l;
	  output_string ch "}\n\n"
	end
  | Entry (entry_type,key,fields) ->
      if needs_output key keys then 
	begin
	  if html then begin Html.open_anchor ch key; Html.close_anchor ch end;
	  output_string ch ("@" ^ entry_type ^ "{" ^ key);
	  List.iter
	    (fun (field,l) ->
	       output_string ch (",\n  " ^ field ^ " = ");
	       if html & field = "CROSSREF" then 
		 print_crossref html ch l
	       else if html & is_link_field field then
		 print_link_field ch l
	       else 
		 print_atom_list html ch l)
	    fields;
	  output_string ch "\n}\n\n"
	end

let output_bib html ch bib keys =
  Bibtex.fold
    (fun entry () -> print_command html ch keys entry)    
    bib
    ()
