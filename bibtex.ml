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

(*i $Id: bibtex.ml,v 1.15 2001-02-21 09:51:52 filliatr Exp $ i*)

(*s Datatype for BibTeX bibliographies. *)

type entry_type = string
		    
type key = string

module KeySet = Set.Make(struct type t = key let compare = compare end)
	     
type atom =
  | Id of string
  | String of string

type command = 
  | Comment of string
  | Preamble of string
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list

(*s biblio is stored as a list. Beware, this in reverse order: the
   first entry is at the end of the list. This is intentional! *)

type biblio = command list

let empty_biblio = []

let size b = List.length b

(*s the natural iterator on biblio must start at the first entry, so it
   is the [fold_right] function on lists, NOT the [fold_left]! *)

let fold = List.fold_right

let rec find_entry key biblio =
  match biblio with
    | [] -> raise Not_found
    | (Entry (_,s,_) as e) :: b ->
	if s = key then e else find_entry key b
    | _ :: b -> find_entry key b

let add_new_entry command biblio = command :: biblio

let rec remove_entry key biblio =
  match biblio with
    | [] -> raise Not_found
    | (Entry(_,s,_) as e) :: b ->
	if s = key then b else e :: (remove_entry key b)
    | e :: b -> e :: (remove_entry key b)

(*s [add_entry k c b] adds an entry of key [k] and command [c] in
   biblio [b] and returns the new biblio. If an entry of key [k]
   already exists in [b], it is replaced by the new one. *)

let add_entry command biblio =
  match command with
    | Entry(_,key,_) ->
	begin
	  try
	    let new_bib = remove_entry key biblio in
	    command :: new_bib
	  with Not_found -> 
	    command :: biblio
	end
    | _ -> command::biblio

let merge_biblios b1 b2 =
  let b2keys =
    fold
      (fun entry accu -> match entry with
	 | Entry (_,key,_) -> KeySet.add key accu
	 | _ -> accu)
      b2
      KeySet.empty
  and b1abbrevs =
    fold
      (fun entry accu -> match entry with
	 | Abbrev (key,_) -> KeySet.add key accu
	 | _ -> accu)
      b1
      KeySet.empty
  in
  let new_b1 = 
    fold
      (fun entry accu -> match entry with
	 | Entry (_,key,_) -> 
	     if KeySet.mem key b2keys then accu else entry :: accu
	 | _ -> entry :: accu)
      b1
      empty_biblio
  in
  let new_bib =
    fold
      (fun entry accu -> match entry with
	 | Abbrev (key,_) -> 
	     if KeySet.mem key b1abbrevs then accu else entry :: accu
	 | _ -> entry :: accu)
      b2
      new_b1
  in
  new_bib

let month_env =
  List.map
    (fun s -> (s,[Id s]))
    [ "JAN" ; "FEB" ; "MAR" ; "APR" ; "MAY" ; "JUN" ;
      "JUL" ; "AUG" ; "SEP" ; "OCT" ; "NOV" ; "DEC" ]

let abbrev_is_implicit key =
  try
    let _ = int_of_string key in true
  with Failure "int_of_string" ->
    try
      let _ = List.assoc key month_env in true
    with Not_found -> false

(*i
let rec abbrev_exists key biblio =
  match biblio with
    | [] -> false
    | (Abbrev (s,_)) :: b -> s = key || abbrev_exists key b
    | _ :: b -> abbrev_exists key b
i*)

let rec find_abbrev key biblio =
  match biblio with
    | [] -> raise Not_found
    | (Abbrev (s,_) as e) :: b -> 
	if s = key then e
	else find_abbrev key b
    | _ :: b -> find_abbrev key b

let concat_atom_lists a1 a2 = 
  match (a1,a2) with
    | ([String s1], [String s2]) -> [String (s1 ^ s2)]
    | _ -> a1 @ a2

let abbrev_table = Hashtbl.create 97

let add_abbrev a l = Hashtbl.add abbrev_table a l

let _ = List.iter (fun (a,l) -> add_abbrev a l) month_env

let find_abbrev_in_table a = Hashtbl.find abbrev_table a

let rec expand_list = function
  | [] -> []
  | ((Id s) as a) :: rem ->
      begin
	try 
	  let v = find_abbrev_in_table s in
	  concat_atom_lists v (expand_list rem)
	with Not_found -> 
	  concat_atom_lists [a] (expand_list rem)
      end
  | ((String _) as a) :: rem ->
      concat_atom_lists [a] (expand_list rem)

let rec expand_fields = function
  | [] ->  []
  | (n,l) :: rem -> (n, expand_list l) :: (expand_fields rem)

let rec expand_abbrevs biblio = 
  fold 
    (fun command accu ->
       match command with
	 | Abbrev (a,l) ->
	     let s = expand_list l in
	     add_abbrev a s; 
	     accu
	 | Entry (t,k,f) ->
	     Entry (t,k,expand_fields f) :: accu
	 | e ->
	     e :: accu)
    biblio
    []
