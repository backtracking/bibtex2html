/* 
 * bibtex_parser.mly
 */

%{

  open Bibtex

%}

%token <string> Tident
%token <string> Tstring
%token Tabbrev Tcomment Tpreamble Tlbrace Trbrace Tcomma Tequal EOF Tsharp

%start entry_list
%type <(Bibtex.entry list)> entry_list
%start command_list
%type <(Bibtex.command list)> command_list
%start command
%type <Bibtex.command> command

%%

entry_list:
   command_list
     { Bibtex.expand $1 }
;
command_list:
   command command_list
     { $1::$2 }
 | EOF
     { [] }
;
command:
   Tcomment Tlbrace anything_until_rbrace
     { Comment }
 | Tpreamble Tlbrace Tstring Trbrace
     { Preamble $3 }
 | Tabbrev Tlbrace Tident Tequal sharp_string_list Trbrace
     { Abbrev ($3,$5) }
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
     { (Id $1) }
 | Tstring
     { (String $1) }
;
anything_until_rbrace:
   Trbrace
     { () }
 | any_but_rbrace anything_until_rbrace
     { () }
;
any_but_rbrace:
   Tident  { () }
 | Tstring { () }
 | Tequal  { () }
%%
