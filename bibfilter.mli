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

(* $Id: bibfilter.mli,v 1.2 1999-06-30 16:44:39 marche Exp $ *)

open Bibtex;;

(* [filter bib f] returns the set of keys of [bib] whose fields
   satisfy the filter criterion [f] *)

val filter : 
  command list -> (key -> ((string * atom list) list) -> bool) -> KeySet.t;;

(* [saturate bib s] returns the smallest part of the bibliography
   [bib] containing all the keys in s together with all the necessary
   abbreviation strings and cross-references *)

val saturate : command list -> KeySet.t -> KeySet.t;;


		    
