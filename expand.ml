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

(*s Expansion of abbreviations in BibTeX databases. *)

open Format
open Bibtex

type fields = (string * string) list

type entry = entry_type * key * fields
		

let abbrev_table = Hashtbl.create 97

let add_abbrev a s = Hashtbl.add abbrev_table a s

let find_abbrev s = Hashtbl.find abbrev_table s

(* months are predefined abbreviations *)
let () = 
  List.iter (fun (id,m) -> add_abbrev id m)
  [ "jan", "January" ;
    "feb", "February" ;
    "mar", "March" ;
    "apr", "April" ;
    "may", "May" ;
    "jun", "June" ;
    "jul", "July" ;
    "aug", "August" ;
    "sep", "September" ;
    "oct", "October" ;
    "nov", "November" ;
    "dec", "December" ]

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
	 | Preamble l ->
	     let s = expand_list l in
	     macros_in_preamble s;
	     accu
	 | Comment _ -> accu)	
    biblio
    []

(*s Sort BibTeX entries by decreasing dates. *)

let int_of_month = function
  | "Janvier" | "January" -> 0
  | "Février" | "February" -> 1
  | "Mars" | "March" -> 2
  | "Avril" | "April" -> 3
  | "Mai" | "May" -> 4
  | "Juin" | "June" -> 5
  | "Juillet" | "July" -> 6
  | "Août" | "August" -> 7
  | "Septembre" | "September" -> 8
  | "Octobre" | "October" -> 9
  | "Novembre" | "November" -> 10 
  | "Décembre" | "December" -> 11
  | _ -> invalid_arg "int_of_month"

let month_day_re1 = Str.regexp "\\([a-zA-Z]+\\)\\( \\|~\\)\\([0-9]+\\)"
let month_day_re2 = Str.regexp "\\([0-9]+\\)\\( \\|~\\)\\([a-zA-Z]+\\)"
let month_anything = Str.regexp "\\([a-zA-Z]+\\)"

let parse_month m =
  if Str.string_match month_day_re1 m 0 then
    int_of_month (Str.matched_group 1 m), int_of_string (Str.matched_group 3 m)
  else if Str.string_match month_day_re2 m 0 then
    int_of_month (Str.matched_group 3 m), int_of_string (Str.matched_group 1 m)
  else if Str.string_match month_anything m 0 then
    int_of_month (Str.matched_group 1 m), 1
  else
    int_of_month m, 1

type date = { year : int; month : int; day : int }

let dummy_date = { year = 0; month = 0; day = 0 }

let extract_year k f =
  try
    int_of_string (List.assoc "year" f)
  with Failure "int_of_string" ->
    if not !Options.quiet then
      eprintf "Warning: incorrect year in entry %s@." k;
    if !Options.warn_error then exit 2;
    0

let extract_month k f =
  try
    parse_month (List.assoc "month" f)
  with 
    | Not_found ->
	0,1
    | _ ->
	if not !Options.quiet then
	  eprintf "Warning: incorrect month in entry %s\n" k; 
	if !Options.warn_error then exit 2;
	0,1

let rec find_entry k = function
  | [] -> raise Not_found
  | (_,k',_) as e :: r -> if k = k' then e else find_entry k r

let rec extract_date el (_,k,f) =
  try
    let y = extract_year k f in
    let m,d = extract_month k f in
    (* eprintf "extract_date: year = %d month = %d day = %d@." y m d; *)
    { year = y; month = m; day = d }
  with Not_found ->
    try extract_date el (find_entry (List.assoc "crossref" f) el)
    with Not_found -> dummy_date

let date_order el e1 e2 =
  let d1 = extract_date el e1 in
  let d2 = extract_date el e2 in
  (d1.year < d2.year) ||
  (d1.year == d2.year && d1.month < d2.month) ||
  (d1.year == d2.year && d1.month == d2.month && d1.day < d2.day)

let combine_comp c d =
  if c=0 then d else c

let date_compare el e1 e2 =
  let d1 = extract_date el e1 in
  let d2 = extract_date el e2 in
  combine_comp 
    (d1.year - d2.year)
    (combine_comp 
       (d1.month - d2.month)
       (d1.day - d2.day))

(*s Access to the fields. *)

let get_field (_,_,f) s = List.assoc (String.lowercase s) f
let get_lowercase_field (_,_,f) s = List.assoc s f

let get_title e = get_lowercase_field e "title"

let get_year e = get_lowercase_field e "year"

let get_month e = get_lowercase_field e "month"

let get_author e = get_lowercase_field e "author"

