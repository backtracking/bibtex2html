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

(* $Id: bibtex.mli,v 1.11 2000-06-02 19:37:31 filliatr Exp $ *)

type entry_type = string
		    
type key = string

module KeySet : Set.S with type elt = key

type atom =
    Id     of string
  | String of string

type command = 
    Comment of string
  | Preamble of string
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list

type biblio

(* [empty_biblio] is an empty bibliography *)

val empty_biblio : biblio

(* [add_new_entry k c b] adds an entry of key [k] and command [c] in
   biblio [b] and returns the new biblio. The entry [k]
   is supposed not to exists yet in [b]. *)

val add_new_entry : command -> biblio -> biblio

(* [merge_biblios b1 b2] merges biblios [b1] and [b2]. Commands in the
   resulting biblio are the commands of b1, then the commands of b2,
   except for dupliacates: any abbrev in [b2] that already exists in
   [b1] is ignored, and conversely every regular entries of [b1] which
   key exists also in [b2] is ignored. This behaviour is because
   abbrevs are supposed to be used by entries AFTER the definition of
   abbrevs, whereas regular entries are supposed to be used as
   crossrefs by entries BEFORE the definition of this entry. *)

val merge_biblios : biblio -> biblio -> biblio

(* access functions *)

(* [find_entry k b] returns the first entry of key [k] in biblio
   [b]. Raises [Not_found] if no entry of this key exist. *)

val find_entry : key -> biblio -> command

(* [size b] is the number of commands in [b] *)

val size : biblio -> int

(* [fold f b accu] iterates [f] on the commands of [b], starting from
   [a]. If the commands of [b] are $c_1,\ldots,c_n$ in this order,
   then it computes [f $c_n$ (f $c_{n-1}$ $\cdots$ (f $c_1$
   a)$\cdots$)] *)

val fold : (command -> 'a -> 'a) -> biblio -> 'a -> 'a

(* [abbrev_is_implicit k] is true when [k] is an integer or a month
   name.  [abbrev_exists k b] is true when [k] appears in biblio [b]. *)

val abbrev_is_implicit : key -> bool
val abbrev_exists : key -> biblio -> bool

(* expansion of abbreviations. [expand_abbrevs bib] returns a new
   bibliography where all strings have been expanded *)

val expand_abbrevs : biblio -> biblio
