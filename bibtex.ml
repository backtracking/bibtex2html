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

(*s Datatype for BibTeX bibliographies. *)

type entry_type = string
		    
type key = string

module KeySet = Set.Make(struct type t = key let compare = compare end)
	     
type atom =
  | Id of string
  | String of string

type command = 
  | Comment of string
  | Preamble of atom list
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

let find_entry key biblio =
  let rec find key b =
    match b with
      | [] -> raise Not_found
      | (Entry (_,s,_) as e) :: b ->
	  if String.lowercase s = key then e else find key b
      | _ :: b -> find key b
  in find (String.lowercase key) biblio

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
	     if KeySet.mem key b2keys then 
	       begin
		 Format.eprintf "Warning, key '%s' duplicated@." key;
		 if !Options.warn_error then exit 2;
		 accu 
	       end 
	     else entry :: accu
	 | _ -> entry :: accu)
      b1
      empty_biblio
  in
  let new_bib =
    fold
      (fun entry accu -> match entry with
	 | Abbrev (key,_) -> 
	     if KeySet.mem key b1abbrevs then 
	       begin
		 Format.eprintf "Warning, key '%s' duplicated@." key;
		 if !Options.warn_error then exit 2;
		 accu 
	       end 
	     else entry :: accu
	 | _ -> entry :: accu)
      b2
      new_b1
  in
  new_bib

let month_env =
  List.map
    (fun s -> (s,[Id s]))
    [ "jan" ; "feb" ; "mar" ; "apr" ; "may" ; "jun" ;
      "jul" ; "aug" ; "sep" ; "oct" ; "nov" ; "dec" ]

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

let add_crossref_fields =
  List.fold_left
    (fun acc ((x,_) as d) -> 
       if List.mem_assoc x acc then acc else d::acc)


let rec expand_crossrefs biblio = 
  let crossref_table = Hashtbl.create 97 in
  let add_crossref a l = Hashtbl.add crossref_table (String.lowercase a) l in
  let find_crossref a = Hashtbl.find crossref_table (String.lowercase a) in
  let replace_crossref a l = 
    Hashtbl.replace crossref_table (String.lowercase a) l 
  in
  (* first phase: record needed crossrefs in table *)
  List.iter 
    (fun command ->
       match command with
	 | Entry (t,k,f) ->
	     begin
	       try
		 match List.assoc "crossref" f with
		   | [String(s)] -> 
		       add_crossref s []
		   | _ -> 
		       begin
			 Format.eprintf 
			   "Warning: invalid cross-reference in entry '%s'.@." k;
			 if !Options.warn_error then exit 2;
		   end
	       with Not_found -> ();
	     end
	 | _ -> ())
    biblio;
  (* second phase: record crossrefs data in table *)
  List.iter 
    (fun command ->
       match command with
	 | Entry (t,k,f) ->
	     begin
	       try 
		 let _ = find_crossref k in
		 if !Options.debug then
		   Format.eprintf "recording cross-reference '%s'.@." k;
		 replace_crossref k f
	       with Not_found -> ()
	     end
	 | _ -> ())
    biblio;
  (* third phase: expand crossrefs *)
  fold 
    (fun command accu ->
       match command with
	 | Entry (t,k,f) ->
	     begin
	       try
		 match List.assoc "crossref" f with
		   | [String(s)] -> 
		       begin
			 try 
			   let f = List.remove_assoc "crossref" f in
			   let f' = find_crossref s in
			   if f' = [] then
			     begin
			       Format.eprintf 
				 "Warning: cross-reference '%s' not found.@." s;
			       if !Options.warn_error then exit 2;
			     end;
			   Entry (t,k,add_crossref_fields f f') :: accu
			 with Not_found ->
			   assert false
		       end
		   | _ ->  command :: accu
	       with Not_found -> command :: accu
	     end
	 | e ->
	     e :: accu)
    biblio
    []



let sort comp bib = 
  let comments,preambles,abbrevs,entries =
    List.fold_left
      (fun (c,p,a,e) command ->
	 match command with
	   | Comment _ -> (command::c,p,a,e)
	   | Preamble _ -> (c,command::p,a,e)
	   | Abbrev _ -> (c,p,command::a,e)
	   | Entry _ -> (c,p,a,command::e))
      ([],[],[],[])
      bib
  in
  let sort_abbrevs = List.sort comp abbrevs
  and sort_entries = List.sort comp entries
  in
  List.rev_append sort_entries
    (List.rev_append sort_abbrevs
       (List.rev_append preambles (List.rev comments)))




let current_key = ref ""

