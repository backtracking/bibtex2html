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

(* $Id: bibtex.mli,v 1.5 1998-05-28 07:21:01 filliatr Exp $ *)

type entry_type = string
		    
type key = string

type atom =
    Id     of string
  | String of string

type fields = (string * string) list

type entry = entry_type * key * fields
		
type command = 
    Comment
  | Preamble of string
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list

val expand : command list -> entry list (* expand the abbreviations *)

val date_order : entry -> entry -> bool


(* access to the fields *)

val get_field : entry -> string -> string

val get_title : entry -> string
val get_year  : entry -> string
val get_month : entry -> string
val get_author : entry -> string

