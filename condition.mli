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

type constante =
  | Key
  | Entrytype
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
  | Exists of string

(*

  [(evaluate_cond e k fields cond)] returns the boolean value of
  [cond] with respect to the entry of type [e], of key [k], and fields
  [fields].

*)

val evaluate_cond : 
  string -> string -> (string * Bibtex.atom list) list -> condition -> bool

val print : condition -> unit

