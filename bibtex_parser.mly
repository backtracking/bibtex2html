/*
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
 */

/* $Id: bibtex_parser.mly,v 1.6 2000-04-03 14:14:46 marche Exp $ */

%{

  open Bibtex

%}

%token <string> Tident
%token <string> Tstring
%token Tabbrev Tcomment Tpreamble Tlbrace Trbrace Tcomma Tequal EOF Tsharp

%start command_list
%type <Bibtex.biblio> command_list
%start command
%type <Bibtex.command> command

%%

command_list:
  commands EOF { $1 }
;

commands:
   commands command
     { add_new_entry $2 $1 }
 | /* epsilon */
     { empty_biblio }
;
command:
   Tcomment Tlbrace anything_until_rbrace
     { Comment $3 }
 | Tpreamble Tlbrace Tstring Trbrace
     { Preamble $3 }
 | Tabbrev Tlbrace Tident Tequal sharp_string_list Trbrace
     { Abbrev (String.uppercase $3,$5) }
 | Tident Tlbrace Tident Tcomma comma_field_list Trbrace
     { Entry (String.uppercase $1,$3,$5) }
;
comma_field_list:
   field Tcomma comma_field_list
     { $1::$3 }
 | field 
     { [$1] }
 | field Tcomma
     { [$1] }
;
field:
   field_name Tequal sharp_string_list
     { ($1,$3) }
 | field_name  Tequal
     { ($1,[String ""]) }
;
field_name:
   Tident   { String.uppercase $1 }
 | Tcomment { "COMMENT" }
;
sharp_string_list:
   atom Tsharp sharp_string_list
     { $1::$3 }
 | atom
     { [$1] }
;
atom:
   Tident
     { Id (String.uppercase $1) }
 | Tstring
     { String $1 }
;
anything_until_rbrace:
   Trbrace
     { "" }
 | any_but_rbrace anything_until_rbrace
     { $1 ^ $2 }
;
any_but_rbrace:
   Tident  { $1 }
 | Tstring { $1 }
 | Tequal  { "=" }
%%
