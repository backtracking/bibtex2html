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

/* $Id: condition_parser.mly,v 1.1 1999-06-30 16:44:42 marche Exp $ */

%{

  open Condition

%}

%token <string> IDENT STRING COMP
%token <string> INT 
%token COLON AND OR NOT LPAR RPAR EOF

%start condition_start
%type <Condition.condition> condition_start

%left OR
%left AND
%left NOT

%%

condition_start:
  condition EOF              { $1 }
;

condition:
  condition OR condition     { Or($1,$3) }
| condition AND condition    { And($1,$3) }
| NOT condition              { Not($2) }
| LPAR condition RPAR        { $2 }
| atom                       { $1 }
;

atom:
  cte COLON STRING           { (*
				Printf.printf 
				   "field = %s regexp = %s\n" $1 $3;
			       *)
                               Match($1,
				     Str.regexp_case_fold $3) }
| cte COMP cte               { Comp($1,$2,$3) }
;

cte:
  IDENT                      { Field(String.uppercase $1) }
| INT                        { Cte($1) }
| STRING                     { Cte($1) }
;

