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

(* $Id: latexmacros.mli,v 1.4 1998-05-28 07:21:03 filliatr Exp $ *)

(*This code is Copyright (C) 1997 Xavier Leroy. *)

type action =
    Print of string
  | Print_arg
  | Skip_arg
  | Raw_arg of (string -> unit)
  | Recursive of string

val def : string -> action list -> unit

val find_macro: string -> action list

val out_channel : out_channel ref
val print_s : string -> unit
val print_c : char -> unit
