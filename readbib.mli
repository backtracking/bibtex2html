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

(*i $Id: readbib.mli,v 1.3 2001-02-21 09:51:54 filliatr Exp $ i*)

(*s [(read_entries_from_file f)] returns the BibTeX entries of the
    BibTeX file [f] (from standard input if [f=""]). *)

val read_entries_from_file : string -> Bibtex.biblio

	
