#use "bbl_lexer.ml";;

let lb = Lexing.from_channel (open_in "/tmp/bib2html3.bbl");;

skip_header lb;;

#trace bibitem;;

bibitem lb;;



#load "bibtex.cmo";;
#load "bibtex_parser.cmo";;
#load "bibtex_lexer.cmo";;
open Bibtex;;
open Bibtex_parser;;
open Bibtex_lexer;;

let lb = 
  Lexing.from_channel (open_in "/users/demons/demons/biblio/demons.bib");;
let c = ref (Entry ("","",[]));;
Bibtex_lexer.reset();;

while true do c := command token lb done;;
!Bibtex_lexer.line;;


command token lb;;
