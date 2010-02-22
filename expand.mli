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

type fields = (string * string) list

type entry = Bibtex.entry_type * Bibtex.key * fields
		
val expand : Bibtex.biblio -> entry list

val expand_fields : (string * Bibtex.atom list) list -> (string * string) list

(*s Compare the dates of two entries. *)

val date_order : entry list -> entry -> entry -> bool

val date_compare : entry list -> entry -> entry -> int

(*s Access to the fields of a given entry. *)

val get_field : entry -> string -> string
val get_lowercase_field : entry -> string -> string

val get_title : entry -> string
val get_year  : entry -> string
val get_month : entry -> string
val get_author : entry -> string

