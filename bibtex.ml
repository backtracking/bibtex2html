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

(* $Id: bibtex.ml,v 1.8 1999-06-29 15:48:54 marche Exp $ *)

type entry_type = string
		    
type key = string

module KeySet = Set.Make(struct type t = key let compare = compare end);;
	     
type atom =
    Id of string
  | String of string

type command = 
    Comment of string
  | Preamble of string
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list


type biblio = command list


let rec find_entry key biblio =
  match biblio with
      [] -> raise Not_found
    | (Entry(_,s,_) as e)::b ->
	if s=key then e else find_entry key b
    | _::b -> find_entry key b
;;


let month_env =
  List.map
    (fun s -> s,[Id(s)])
    [ "JAN" ; "FEB" ; "MAR" ; "APR" ; "MAY" ; "JUN" ;
      "JUL" ; "AUG" ; "SEP" ; "OCT" ; "NOV" ; "DEC" ]
;;

let abbrev_is_implicit key =
  try
    let _ = int_of_string key
    in true
  with
      Failure "int_of_string" ->
	try
	  let _ = List.assoc key month_env
	  in true
	with Not_found -> false
;;

let rec abbrev_exists key biblio =
  match biblio with
      [] -> false
    | (Abbrev(s,_))::b ->
	if s=key then true else abbrev_exists key b
    | _::b -> abbrev_exists key b
;;

  





let concat_atom_lists a1 a2 = 
  match (a1,a2) with
      ([String(s1)],[String(s2)]) -> [String(s1 ^ s2)]
    | _ -> a1 @ a2
;;


let find_abbrev id env =
  try
    let _ = int_of_string id
    in [String(id)]
  with
      Failure "int_of_string" ->
	List.assoc id env
;;

let rec expand_list env = function
    [] -> []
  | ((Id s) as a)::rem ->
      begin
	try 
	  let v = List.assoc s env in
	    concat_atom_lists v (expand_list env rem)
	with 
	    Not_found -> 
	      concat_atom_lists [a] (expand_list env rem)
      end
  | ((String _) as a)::rem ->
      concat_atom_lists [a] (expand_list env rem)

let rec expand_fields env = function
    [] ->  []
  | (n,l) :: rem -> 
      (n,expand_list env l) :: (expand_fields env rem)

let rec expand env = function
    [] ->
      []
  | (Abbrev (a,l)) :: rem ->
      let s = expand_list env l in
	expand ((a,s)::env) rem
  | (Entry (t,k,f)) :: rem ->
      Entry(t,k,expand_fields env f) :: (expand env rem)
  | e :: rem ->
      e :: (expand env rem)
;;


let expand_abbrevs bib = expand month_env bib;;
