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

(* $Id: condition.mli,v 1.3 2000-06-02 19:37:32 filliatr Exp $ *)

type constante =
  | Key
  | Field of string
  | Cte of string

type condition =
  | True 
  | False 
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | Comp of constante * string * constante
  | Match of constante * Str.regexp

val evaluate_cond : 
  string -> (string * Bibtex.atom list) list -> condition -> bool

val print : condition -> unit

