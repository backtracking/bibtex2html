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

(* $Id: bibtex.mli,v 1.8 1999-06-30 16:44:40 marche Exp $ *)

type entry_type = string
		    
type key = string

module KeySet : Set.S with type elt = key;;

type atom =
    Id     of string
  | String of string

type command = 
    Comment of string
  | Preamble of string
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list

type biblio = command list

(* access functions *)

(* [find_entry k b] returns the first entry of key [k] in biblio
   [b]. Raises [Not_found] if no entry of this key exist. *)

val find_entry : key -> biblio -> command;;

(* [abbrev_is_implicit k] is true when [k] is an integer or a month
   name.  [abbrev_exists k b] is true when [k] appears in biblio [b].
   *)

val abbrev_is_implicit : key -> bool;;
val abbrev_exists : key -> biblio -> bool;;


(* expansion of abbreviations. [expand_abbrevs bib] returns a new
   bibliography where all strings have been expanded *)

val expand_abbrevs : biblio -> biblio;;


