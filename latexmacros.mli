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

(*s This module provides a table to store the translations of LaTeX
  macros. A translation is a list of actions of the following type
  [action]. *)

(* This code is an adaptation of a code written by Xavier Leroy in
   1995-1997, in its own home-made latex2html translator. See


@inproceedings{Leroy-latex2html,
               author =        "Xavier Leroy",
               title =         "Lessons learned from the translation of
                         documentation from \LaTeX\ to {HTML}",
               booktitle =     "ERCIM/W4G Int. Workshop on WWW
                         Authoring and Integration Tools",
               year =          1995,
               month =         feb}

*)

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

