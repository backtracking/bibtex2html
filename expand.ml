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

(*i $Id: expand.ml,v 1.9 2001-02-21 09:51:53 filliatr Exp $ i*)

(*s Expansion of abbreviations in BibTeX databases. *)

open Format
open Bibtex

type fields = (string * string) list

type entry = entry_type * key * fields
		

let abbrev_table = Hashtbl.create 97

let add_abbrev a s = Hashtbl.add abbrev_table a s

let find_abbrev s = Hashtbl.find abbrev_table s

let assoc_months = 
  [ "JAN", "January" ;
    "FEB", "February" ;
    "MAR", "March" ;
    "APR", "April" ;
    "MAY", "May" ;
    "JUN", "June" ;
    "JUL", "July" ;
    "AUG", "August" ;
    "SEP", "September" ;
    "OCT", "October" ;
    "NOV", "November" ;
    "DEC", "December" ]

let rec expand_list = function
  | [] -> 
      ""
  | (Id s) :: rem ->
      (try find_abbrev s with Not_found -> s) ^ (expand_list rem)

  | (String s) :: rem ->
      s ^ (expand_list rem)

let rec expand_fields = function
  | [] -> 
      []
  | ("MONTH" as n,l) :: rem ->
      let s = expand_list l in
      	(n,try List.assoc (String.uppercase s) assoc_months 
	   with Not_found -> s) 
      	:: (expand_fields rem)
  | (n,l) :: rem -> 
      (n,expand_list l) :: (expand_fields rem)

let macros_in_preamble s =
  try
    let lb = Lexing.from_string s in Latexscan.read_macros lb
  with _ -> ()

let rec expand biblio = 
  Bibtex.fold 
    (fun command accu ->
       match command with
	 | Abbrev (a,l) ->
	     let s = expand_list l in
	     add_abbrev a s; 
	     accu
	 | Entry (t,k,f) ->
	     (t,k,expand_fields f) :: accu
	 | Preamble s ->
	     macros_in_preamble s;
	     accu
	 | Comment _ -> accu)	
    biblio
    []

(*s Sort BibTeX entries by decreasing dates. *)

let int_of_month = function
  | "January" -> 0
  | "February" -> 1
  | "March" -> 2
  | "April" -> 3
  | "May" -> 4
  | "June" -> 5
  | "July" -> 6
  | "August" -> 7
  | "September" -> 8
  | "October" -> 9
  | "November" -> 10 
  | "December" -> 11
  | _ -> 0 (* TODO *)

type date = { year : int; month : int }

let dummy_date = { year = 0; month = 0 }

let extract_year k f =
  try
    int_of_string (List.assoc "YEAR" f)
  with Failure "int_of_string" ->
    if not !Options.quiet then
      eprintf "Warning: incorrect year in entry %s\n" k;
    0

let extract_month k f =
  try
    int_of_month (List.assoc "MONTH" f)
  with 
    | Failure "int_of_string" ->
	if not !Options.quiet then
	  eprintf "Warning: incorrect month in entry %s\n" k; 
	0
    | Not_found -> 0

let rec find_entry k = function
  | [] -> raise Not_found
  | (_,k',_) as e :: r -> if k = k' then e else find_entry k r

let rec extract_date el (_,k,f) =
  try
    let y = extract_year k f in
    let m = extract_month k f in
    { year = y; month = m }
  with Not_found ->
    try extract_date el (find_entry (List.assoc "CROSSREF" f) el)
    with Not_found -> dummy_date

let date_order el e1 e2 =
  let d1 = extract_date el e1 in
  let d2 = extract_date el e2 in
  (d1.year < d2.year) or (d1.year == d2.year & d1.month < d2.month)

(*s Access to the fields. *)

let get_field (_,_,f) s = List.assoc (String.uppercase s) f
let get_uppercase_field (_,_,f) s = List.assoc s f

let get_title e = get_uppercase_field e "TITLE"

let get_year e = get_uppercase_field e "YEAR"

let get_month e = get_uppercase_field e "MONTH"

let get_author e = get_uppercase_field e "AUTHOR"

