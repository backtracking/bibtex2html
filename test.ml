#use "bbl_lexer.ml";;

let lb = Lexing.from_channel (open_in "/tmp/bib2html0.bbl");;

skip_header lb;;

#trace bibitem;;

bibitem lb;;



#load "bibtex.cmo";;
#load "bibtex_parser.cmo";;
#load "bibtex_lexer.cmo";;
open Bibtex;;
open Bibtex_parser;;
open Bibtex_lexer;;

let lb = Lexing.from_channel (open_in "/home/jcfillia/tmp/toto.bib");;

command token lb;;
