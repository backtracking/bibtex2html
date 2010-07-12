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

(*s Production of the HTML documents from the BibTeX bibliographies. *)

open Bibtex

(*s Translation options. *)

val nodoc : bool ref
val nokeys : bool ref
val use_keys : bool ref
val file_suffix : string ref
val link_suffix : string ref
val raw_url : bool ref
val title : string ref
val title_spec : bool ref
val print_abstract : bool ref
val print_keywords : bool ref
val print_links : bool ref
val print_header : bool ref
val print_footer : bool ref
val multiple : bool ref
val single : bool ref
val both : bool ref
val user_header : string ref
val user_footer : string ref
val bib_entries : bool ref
val input_file : string ref
val output_file : string ref
val use_label_name : bool ref
val linebreak : bool ref
val doi : bool ref
val doi_prefix : string ref
val eprint : bool ref
val eprint_prefix : string ref
val links_in_bib_file : bool ref
val revkeys : bool ref

type table_kind = Table | DL | NoTable
val table : table_kind ref

(*s Inserting links for some BibTeX fields. *)

val add_field : string -> unit
val add_named_field : string -> string -> unit
val add_note_field : string -> unit
val add_note_html_field : string -> unit

(*s Production of the HTML output. *)

val format_list :
  biblio ->
  (string option * (string option * string * Expand.entry) list) list ->
  KeySet.t option -> unit
