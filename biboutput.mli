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

(*s [output_bib html ch bib keys] outputs to the channel [ch] the
   fields of the bibliography [bib] whose key belong to [keys]. [html]
   is a flag that tells whether html anchors must be added: if [html]
   is false, the output is a regular bibtex file, if [html] is true,
   anchors are added on crossrefs, abbreviations, and URLs in
   fields. Notice that to guarantee that the generated part of the
   bibliography is coherent, that is all needed abbreviations and
   cross-references are included, one as to call Bibfilter.saturate
   before. Notice finally that the channel [ch] is NOT closed by this
   function *)

open Bibtex

exception Bad_input_for_php of string

val output_bib : 
  ?remove:string list -> ?rename:(string * string) list ->
  ?php:bool -> html:bool -> ?html_file:string -> 
  out_channel -> biblio -> KeySet.t option -> unit

(*s [add_link_field f] declares a new field [f] to be displayed as a web link
    (when HTML option of [output_bib] is set) *)

val add_link_field : string -> unit
