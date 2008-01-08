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

(*i $Id: latexmacros.mli,v 1.10 2008-01-08 14:17:45 filliatr Exp $ i*)

(*s This code is Copyright (C) 1997 Xavier Leroy. It provides a table to
    store the translations of LaTeX macros. A translation is a list
    of actions of the following type [action]. *)

type action =
  | Print of string
  | Print_arg
  | Skip_arg
  | Raw_arg of (string -> unit)
  | Parameterized of (string -> action list)
  | Recursive of string

val def : string -> action list -> unit

val find_macro: string -> action list

val init_style_macros : string -> unit

(*s HTML entities *)

val html_entities : unit -> unit

val unicode_entities : unit -> unit

(*s Utility functions used in the definition of translations. *)

val out_channel : out_channel ref
val print_s : string -> unit
val print_c : char -> unit

