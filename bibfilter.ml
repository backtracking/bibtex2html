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

(*s Filtering and saturating BibTeX files. *)

open Printf
open Bibtex

let debug = false

(*s [filter bib f] returns the list of keys of [bib] whose fields
    satisfy the filter criterion [f] *)

let filter biblio criterion =
  Bibtex.fold
    (fun entry keys ->
       match entry with
	   Entry(entry_type,key,fields) 
	     when criterion entry_type key fields ->
	       KeySet.add key keys
	 | _ -> keys)
    biblio
    KeySet.empty


(*s [needed_keys biblio field value keys] returns the set of keys
    [keys] augmented with the needed keys for [value] *)

let rec needed_keys_for_field biblio field value keys abbrevs =
  if field = "crossref"
  then 
    match value with
	[String(s)] -> 
	  if not (KeySet.mem s keys) then
	    begin
	      try
		let e = find_entry s biblio
		in
		if debug then begin
		  eprintf "We need additional crossref %s\n" s
		end;
		needed_keys_for_entry biblio (KeySet.add s keys) abbrevs e
	      with Not_found ->
		if not !Options.quiet then
		  eprintf "Warning: cross-reference \"%s\" not found.\n" s;
		if !Options.warn_error then exit 2;
		(keys,abbrevs)
	    end
	  else (keys,abbrevs)
      | _ -> 
	  if not !Options.quiet then
	    eprintf "Warning: cross-references must be constant strings\n";
	  if !Options.warn_error then exit 2;
	  (keys,abbrevs)
  else
    List.fold_right
      (fun a (keys,abbrevs) ->
	 match a with
	     Id(id) -> 
	       let id = String.lowercase id in		 
	       if not (KeySet.mem id abbrevs) 
	       then
		 try
		   let e = find_abbrev id biblio in
		   if debug then begin
		     eprintf "We need additional abbrev %s\n" id
		   end;
		   needed_keys_for_entry biblio keys (KeySet.add id abbrevs) e
		 with Not_found ->
		   if abbrev_is_implicit id then (keys,abbrevs)
		   else 
		     begin
		       if not !Options.quiet then
			 eprintf "Warning: string \"%s\" not found.\n" id;
		       if !Options.warn_error then exit 2;
		       (keys,abbrevs)
		     end
	       else (keys,abbrevs)
	   | _ -> (keys,abbrevs))
      value
      (keys,abbrevs)

and needed_keys_for_entry biblio keys abbrevs = function
    Entry(entry_type,key,fields) ->
      List.fold_right
	(fun (field,value) (keys,abbrevs) ->
           (*i eprintf "Field : %s\n" field; i*)
	   needed_keys_for_field biblio field value keys abbrevs)
	fields
	(keys,abbrevs)
  | Abbrev(field,value) -> 
      needed_keys_for_field biblio field value keys abbrevs
  | _ -> (keys,abbrevs)


(*s [saturate bib l] returns the smallest part of the bibliography
    [bib] containing all the keys in l together with all the necessary
    abbreviation strings and cross-references *)

let saturate biblio s =
  let (keys,abbrevs) =
    Bibtex.fold
      (fun entry (keys,abbrevs) ->
	 match entry with
	   | Entry(_,key,_) as e when KeySet.mem key s ->
	       needed_keys_for_entry biblio keys abbrevs e
	   | _ -> (keys,abbrevs))
      biblio
      (s,KeySet.empty)
  in
  KeySet.union keys abbrevs
