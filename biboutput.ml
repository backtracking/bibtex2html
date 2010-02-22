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
      if !Options.warn_error then exit 2;
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
      if !Options.warn_error then exit 2;
      print_atom_list true ch l

let print_command remove rename html html_file ch keys = function 
  | Comment s -> 
      if html then output_string ch "<pre>\n";
      output_string ch ("@comment{{" ^ s ^ "}}\n");
      if html then output_string ch "</pre>\n";
      output_string ch "\n"
  | Preamble l ->
      if html then output_string ch "<pre>\n";
      output_string ch "@preamble{";
      print_atom_list html ch l;
      output_string ch "}\n";
      if html then output_string ch "</pre>\n";
      output_string ch "\n"
  | Abbrev(s,l) ->
      if needs_output s keys then 
	begin
	  if html then begin Html.open_anchor ch s; Html.close_anchor ch end;
	  if html then output_string ch "<pre>\n";
	  output_string ch ("@string{" ^ s ^ " = ");
	  print_atom_list html ch l;
	  output_string ch "}\n";
	  if html then output_string ch "</pre>\n";
	  output_string ch "\n"
	end
  | Entry (entry_type,key,fields) ->
      if needs_output key keys then 
	begin
	  (*if html then Html.open_balise ch "p";*)
	  if html then begin Html.open_anchor ch key; Html.close_anchor ch end;
	  if html then output_string ch "<pre>\n";
	  output_string ch ("@" ^ entry_type ^ "{");
	  begin match html_file with
	    | Some f -> 
		Html.open_href ch (f ^ "#" ^ key); 
		output_string ch key; 
		Html.close_href ch
	    | None -> 
		output_string ch key
	  end;
	  List.iter
	    (fun (field,l) ->
               if not (List.mem field remove) then
                 begin
                   let ofield =
                     try List.assoc field rename
                     with Not_found -> field
                   in
	           output_string ch (",\n  " ^ ofield ^ " = ");
	           if html & field = "crossref" then 
		     print_crossref html ch l
	           else if html & is_link_field field then
		     print_link_field ch l
	           else 
		     print_atom_list html ch l
                 end)
	    fields;
	  output_string ch "\n}\n";
	  if html then output_string ch "</pre>\n";
	  (*if html then Html.close_balise ch "p";*)
	  output_string ch "\n"

	end

(*s PHP output *)

open Printf

exception Bad_input_for_php of string

let php_print_atom ch = function
  | Id s -> fprintf ch "\"%s\"" s
  | String s -> 
      let s = String.copy s in
      for i=0 to String.length s - 1 do
	s.[i] <- 
	  match s.[i] with
	    | '\n' | '\t' -> ' '
	    | c -> c
      done;
      fprintf ch "\"%s\"" (String.escaped s)

let php_print_atom_list ch = function 
  | [] -> ()
  | [a] -> php_print_atom ch a
  | a::l -> 
      php_print_atom ch a;
      List.iter
	(fun a -> 
	   fprintf ch ".";
	   php_print_atom ch a)
	l

let php_print_command index remove rename ch keys = 
  function 
  | Comment s -> 
      raise 
	(Bad_input_for_php "comments not supported, use option --no-comment")
(*
      output_string ch "<pre>\n";
      output_string ch ("@comment{{" ^ s ^ "}}\n");
      output_string ch "</pre>\n";
      output_string ch "\n"
*)
  | Preamble l ->
      raise (Bad_input_for_php "preamble not supported")
(*
      output_string ch "<pre>\n";
      output_string ch "@preamble{";
      php_print_atom_list ch l;
      output_string ch "}\n";
      output_string ch "</pre>\n";
      output_string ch "\n"
*)
  | Abbrev(s,l) ->
      raise (Bad_input_for_php "string not supported, use option --expand")
(*
      if needs_output s keys then 
	begin
	  Html.open_anchor ch s; 
	  Html.close_anchor ch;
	  output_string ch "<pre>\n";
	  output_string ch ("@string{" ^ s ^ " = ");
	  php_print_atom_list ch l;
	  output_string ch "}\n";
	  output_string ch "</pre>\n";
	  output_string ch "\n"
	end
*)
  | Entry (entry_type,key,fields) ->
      if needs_output key keys then 
	begin
	  if index > 0 then fprintf ch ",\n\n";
	  fprintf ch "%-5d => Array (\n" index;
	  fprintf ch "  \"entrytype\" => \"%s\",\n" entry_type;
	  fprintf ch "  \"cite\" => \"%s\"" key;
	  List.iter
	    (fun (field,l) ->
               if not (List.mem field remove) then
		 begin
		   let ofield =
                     try List.assoc field rename
                     with Not_found -> field
		   in
		   fprintf ch ",\n  \"%s\" => " ofield;
		   php_print_atom_list ch l
             end)
	    fields;
	  fprintf ch ")";
	  succ index
	end
      else index

let output_bib ?(remove=[]) ?(rename=[]) ?(php=false) ~html ?html_file 
    ch bib keys =
  let _ =
    Bibtex.fold
      (fun entry i -> 
	 if php then
	   php_print_command i remove rename ch keys entry
	 else
	   (print_command remove rename html html_file ch keys entry;
	    succ i))    
      bib
    0
  in ()
