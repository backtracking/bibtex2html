/* 
 * bibtex_parser.mly
 */

%{

  open Bibtex

%}

%token <string> Tident
%token <string> Tstring
%token Tabbrev Tat Tlbrace Trbrace Tcomma Tequal EOF Tsharp

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
   Tabbrev Tlbrace Tident Tequal Tstring Trbrace
     { Abbrev ($3,$5) }
 | Tat Tident Tlbrace Tident Tcomma comma_field_list Trbrace
     { Entry (String.uppercase $2,$4,$6) }
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
   Tident Tequal sharp_string_list
     { (String.uppercase $1,$3) }
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
%%
