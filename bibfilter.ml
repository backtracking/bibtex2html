(*
 * bibtex2html - A BibTeX to HTML translator
 * Copyright (C) 1997 Jean-Christophe FILLIATRE
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

(* $Id: bibfilter.ml,v 1.2 1999-06-30 16:44:39 marche Exp $ *)

open Bibtex;;

(* [filter bib f] returns the list of keys of [bib] whose fields
   satisfy the filter criterion [f] *)

let filter biblio criterion =
  List.fold_right
    (fun entry keys ->
       match entry with
	   Entry(entry_type,key,fields) when criterion key fields ->
	     KeySet.add key keys
	 | _ -> keys)
    biblio
    KeySet.empty
;;


(* [needed_keys biblio field value keys] returns the set of keys
[keys] augmented with the needed keys for [value] *)

let rec needed_keys_for_field biblio field value keys =
  if field = "CROSSREF"
  then 
    match value with
	[String(s)] -> 
	  if not (KeySet.mem s keys) then
	    begin
	      try
		let e = find_entry s biblio
		in
(*
		  Printf.printf "We need additional crossref %s\n" s;
*)
		  needed_keys_for_entry biblio (KeySet.add s keys) e
	      with Not_found ->
		Printf.printf "Warning: cross-reference \"%s\" not found.\n" s;
		keys
	    end
	  else keys
      | _ -> 
	  Printf.printf "Warning: cross-references must be constant strings\n";
	  keys
  else
    List.fold_right
      (fun a keys ->
	 match a with
	     Id(id) -> 
	       let id = String.uppercase id in		 
		 if not (KeySet.mem id keys) then
		   if abbrev_is_implicit id then keys
		   else 
		     if abbrev_exists id biblio 
		     then		       
		       (*
			 Printf.printf "We need additional abbrev %s\n" id;
		       *)
		       KeySet.add id keys
		     else
		       begin
			 Printf.printf "Warning: string \"%s\" not found.\n" id;
			 keys
		       end 
		 else keys
	   | _ -> keys)
      value
      keys

and needed_keys_for_entry biblio keys = function
    Entry(entry_type,key,fields) ->
	     List.fold_right
	       (fun (field,value) keys ->
(*
		  Printf.printf "Field : %s\n" field;
*)
		  needed_keys_for_field biblio field value keys)
	       fields
	       keys
  | _ -> keys
;;


(* [saturate bib l] returns the smallest part of the bibliography
   [bib] containing all the keys in l together with all the necessary
   abbreviation strings and cross-references *)


let saturate biblio s =
  List.fold_right
    (fun entry keys ->
       match entry with
	   Entry(_,key,_) as e when KeySet.mem key s ->
	     needed_keys_for_entry biblio keys e
	 | _ -> keys)
    biblio
    s
;;





  

