(**************************************************************************)
(*  bibtex2html - A BibTeX to HTML translator                             *)
(*  Copyright (C) 1997-2014 Jean-Christophe Filliâtre and Claude Marché   *)
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

(* This code is an adaptation of a code written by Xavier Leroy in
   1995-1997, in his own made latex2html translator. See

@inproceedings{Leroy-latex2html,
               author =        "Xavier Leroy",
               title =         "Lessons learned from the translation of
                         documentation from \LaTeX\ to {HTML}",
               booktitle =     "ERCIM/W4G Int. Workshop on WWW
                         Authoring and Integration Tools",
               year =          1995,
               month =         feb}

*)

open Printf
open Options

(*s Output functions. *)

let out_channel = ref stdout
let print_s s = output_string !out_channel s
let print_c c = output_char !out_channel c

(*s Actions and translations table. *)

type action =
  | Print of string
  | Print_arg
  | Skip_arg
  | Raw_arg of (string -> unit)
  | Parameterized of (string -> action list)
  | Recursive of string  (*r piece of LaTeX to analyze recursively *)

let cmdtable = (Hashtbl.create 19 : (string, action list) Hashtbl.t)

let def name action =
  Hashtbl.add cmdtable name action

let find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
    if not !quiet then eprintf "Unknown macro: %s\n" name;
    []
;;

(*s Translations of general LaTeX macros. *)

(* Sectioning *)
def "\\part"
    [Print "<H0>"; Print_arg; Print "</H0>\n"];
def "\\chapter"
    [Print "<H1>"; Print_arg; Print "</H1>\n"];
def "\\chapter*"
    [Print "<H1>"; Print_arg; Print "</H1>\n"];
def "\\section"
    [Print "<H2>"; Print_arg; Print "</H2>\n"];
def "\\section*"
    [Print "<H2>"; Print_arg; Print "</H2>\n"];
def "\\subsection"
    [Print "<H3>"; Print_arg; Print "</H3>\n"];
def "\\subsection*"
    [Print "<H3>"; Print_arg; Print "</H3>\n"];
def "\\subsubsection"
    [Print "<H4>"; Print_arg; Print "</H4>\n"];
def "\\subsubsection*"
    [Print "<H4>"; Print_arg; Print "</H4>\n"];
def "\\paragraph"
    [Print "<H5>"; Print_arg; Print "</H5>\n"];

(* Text formatting *)
def "\\begin{alltt}" [Print "<pre>"];
def "\\end{alltt}" [Print "</pre>"];
def "\\textbf" [Print "<b>" ; Print_arg ; Print "</b>"];
def "\\mathbf" [Print "<b>" ; Print_arg ; Print "</b>"];
def "\\texttt" [Print "<tt>" ; Print_arg ; Print "</tt>"];
def "\\mathtt" [Print "<tt>" ; Print_arg ; Print "</tt>"];
def "\\textit" [Print "<i>" ; Print_arg ; Print "</i>"];
def "\\mathit" [Print "<i>" ; Print_arg ; Print "</i>"];
def "\\textsl" [Print "<i>" ; Print_arg ; Print "</i>"];
def "\\textem" [Print "<em>" ; Print_arg ; Print "</em>"];
def "\\textrm" [Print_arg];
def "\\mathrm" [Print_arg];
def "\\textmd" [Print_arg];
def "\\textup" [Print_arg];
def "\\textnormal" [Print_arg];
def "\\mathnormal" [Print "<i>" ; Print_arg ; Print "</i>"];
def "\\mathcal" [Print_arg];
def "\\mathbb" [Print_arg];
def "\\mathfrak" [Print_arg];

def "\\textin" [Print "<sub>"; Print_arg; Print "</sub>"];
def "\\textsu" [Print "<sup>"; Print_arg; Print "</sup>"];
def "\\textsuperscript" [Print "<sup>"; Print_arg; Print "</sup>"];
def "\\textsi" [Print "<i>" ; Print_arg ; Print "</i>"];

(* Basic color support. *)

def "\\textcolor" [ Parameterized (function name ->
  match String.lowercase_ascii name with
  (* At the moment, we support only the 16 named colors defined in HTML 4.01. *)
  | "black" | "silver" | "gray" | "white" | "maroon" | "red" | "purple" | "fuchsia"
  | "green" | "lime" | "olive" | "yellow" | "navy" | "blue" | "teal" | "aqua" ->
    [ Print (Printf.sprintf "<font color=%s>" name);
      Print_arg ;
      Print "</font>"
    ]
  (* Other, unknown colors have no effect. *)
  | _ ->
    [ Print_arg ]
)];

(* Fonts without HTML equivalent *)
def "\\textsf" [Print "<b>" ; Print_arg ; Print "</b>"];
def "\\mathsf" [Print "<b>" ; Print_arg ; Print "</b>"];
def "\\textsc" [Print_arg];

def "\\textln" [Print_arg];
def "\\textos" [Print_arg];
def "\\textdf" [Print_arg];
def "\\textsw" [Print_arg];

def "\\rm" [];
def "\\cal" [];
def "\\emph" [Print "<em>" ; Print_arg ; Print "</em>"];
def "\\mbox" [Print_arg];
def "\\footnotesize" [];
def "\\etalchar" [ Print "<sup>" ; Raw_arg print_s ; Print "</sup>" ];
def "\\newblock" [Print " "];

(* Environments *)
def "\\begin{itemize}" [Print "<p><ul>"];
def "\\end{itemize}" [Print "</ul>"];
def "\\begin{enumerate}" [Print "<p><ol>"];
def "\\end{enumerate}" [Print "</ol>"];
def "\\begin{description}" [Print "<p><dl>"];
def "\\end{description}" [Print "</dl>"];
def "\\begin{center}" [Print "<blockquote>"];
def "\\end{center}" [Print "</blockquote>"];
def "\\begin{htmlonly}" [];
def "\\end{htmlonly}" [];
def "\\begin{flushleft}" [Print "<blockquote>"];
def "\\end{flushleft}" [Print "</blockquote>"];

(* Special characters *)
def "\\ " [Print " "];
def "\\\n" [Print " "];
def "\\{" [Print "{"];
def "\\}" [Print "}"];
def "\\l" [Print "l"];
def "\\L" [Print "L"];
def "\\oe" [Print "&oelig;"];
def "\\OE" [Print "&OElig;"];
def "\\o" [Print "&oslash;"];
def "\\O" [Print "&Oslash;"];
def "\\ae" [Print "&aelig;"];
def "\\AE" [Print "&AElig;"];
def "\\aa" [Print "&aring;"];
def "\\AA" [Print "&Aring;"];
def "\\i" [Print "i"];
def "\\j" [Print "j"];
def "\\&" [Print "&amp;"];
def "\\$" [Print "$"];
def "\\%" [Print "%"];
def "\\_" [Print "_"];
def "\\slash" [Print "/"];
def "\\copyright" [Print "(c)"];
def "\\th" [Print "&thorn;"];
def "\\TH" [Print "&THORN;"];
def "\\dh" [Print "&eth;"];
def "\\DH" [Print "&ETH;"];
def "\\dj" [Print "&#273;"];
def "\\DJ" [Print "&#272;"];
def "\\ss" [Print "&szlig;"];
def "\\rq" [Print "&rsquo;"];
def "\\spadesuit" [Print "&#x2660;"];
def "\\heartsuit" [Print "&#x2661;"];
def "\\diamondsuit" [Print "&#x2662;"];
def "\\clubsuit" [Print "&#x2663;"];
def "\\flat" [Print "&#x266d;"];
def "\\natural" [Print "&#x266e;"];
def "\\sharp" [Print "&#x266f;"];
def "\\'" [Raw_arg(function "e" -> print_s "&eacute;"
                          | "E" -> print_s "&Eacute;"
			  | "a" -> print_s "&aacute;"
			  | "A" -> print_s "&Aacute;"
			  | "o" -> print_s "&oacute;"
			  | "O" -> print_s "&Oacute;"
			  | "i" -> print_s "&iacute;"
                          | "\\i" -> print_s "&iacute;"
			  | "I" -> print_s "&Iacute;"
			  | "u" -> print_s "&uacute;"
			  | "U" -> print_s "&Uacute;"
			  | "'"  -> print_s "&rdquo;"
			  | "c" -> print_s "&#x107;"
			  | "C" -> print_s "&#x106;"
			  | "g" -> print_s "&#x1f5;"
			  | "G" -> print_s "G"
			  | "l" -> print_s "&#x13A;"
			  | "L" -> print_s "&#x139;"
			  | "n" -> print_s "&#x144;"
			  | "N" -> print_s "&#x143;"
			  | "r" -> print_s "&#x155;"
			  | "R" -> print_s "&#x154;"
			  | "s" -> print_s "&#x15b;"
			  | "S" -> print_s "&#x15a;"
			  | "y" -> print_s "&yacute;"
			  | "Y" -> print_s "&Yacute;"
			  | "z" -> print_s "&#x179;"
			  | "Z" -> print_s "&#x17a;"
			  | ""  -> print_c '\''
                          | s   -> print_s s ; print_s "&#x0301;")];
def "\\`" [Raw_arg(function "e" -> print_s "&egrave;"
                          | "E" -> print_s "&Egrave;"
                          | "a" -> print_s "&agrave;"
                          | "A" -> print_s "&Agrave;"
			  | "o" -> print_s "&ograve;"
			  | "O" -> print_s "&Ograve;"
			  | "i" -> print_s "&igrave;"
                          | "\\i" -> print_s "&igrave;"
			  | "I" -> print_s "&Igrave;"
                          | "u" -> print_s "&ugrave;"
                          | "U" -> print_s "&Ugrave;"
			  | "`"  -> print_s "&ldquo;"
			  | ""  -> print_s "&lsquo;"
			  | s -> print_s s ; print_s "&#x0300;")];
def "\\~" [Raw_arg(function "n" -> print_s "&ntilde;"
		          | "N" -> print_s "&Ntilde;"
		          | "o" -> print_s "&otilde;"
		          | "O" -> print_s "&Otilde;"
			  | "i" -> print_s "&#x129;"
			  | "\\i" -> print_s "&#x129;"
			  | "I" -> print_s "&#x128;"
		          | "a" -> print_s "&atilde;"
		          | "A" -> print_s "&Atilde;"
			  | "u" -> print_s "&#x169;"
			  | "U" -> print_s "&#x168;"
		          | ""  -> print_s "&tilde;"
                          | s -> print_s s ; print_s "&#x0303;")];
def "\\=" [Raw_arg(function s -> print_s s ; print_s "&#x0304;")];
def "\\k" [Raw_arg(function "A" -> print_s "&#260;"
                          | "a" -> print_s "&#261;"
                          | "i" -> print_s "&#302;"
                          | "I" -> print_s "&#303;"
                          | s   -> print_s s)];
def "\\c" [Raw_arg(function "c" -> print_s "&ccedil;"
                          | "C" -> print_s "&Ccedil;"
                          | s   -> print_s s)];
def "\\^" [Raw_arg(function "a" -> print_s "&acirc;"
                          | "A" -> print_s "&Acirc;"
                          | "e" -> print_s "&ecirc;"
                          | "E" -> print_s "&Ecirc;"
                          | "i" -> print_s "&icirc;"
                          | "\\i" -> print_s "&icirc;"
                          | "I" -> print_s "&Icirc;"
                          | "o" -> print_s "&ocirc;"
                          | "O" -> print_s "&Ocirc;"
                          | "u" -> print_s "&ucirc;"
                          | "U" -> print_s "&Ucirc;"
                          | "w" -> print_s "&#x175;"
                          | "W" -> print_s "&#x174;"
                          | "y" -> print_s "&#x177;"
                          | "Y" -> print_s "&#x176;"
			  | ""  -> print_c '^'
                          | "j" -> print_s "&#x0237;&#x0302;"
                          | s   -> print_s s ; print_s "&#x0302;")];
def "\\c" [Raw_arg(function "C" -> print_s "&#x00c7;" (* cedilla accent *)
                          | "C" -> print_s "&#x00C7;"
                          | "c" -> print_s "&#x00E7;"
                          | "D" -> print_s "&#x1E10;"
                          | "d" -> print_s "&#x1E11;"
                          | "E" -> print_s "&#x0228;"
                          | "e" -> print_s "&#x0229;"
                          | "G" -> print_s "&#x0122;"
                          | "g" -> print_s "&#x0123;"
                          | "H" -> print_s "&#x1E28;"
                          | "h" -> print_s "&#x1E29;"
                          | "K" -> print_s "&#x0136;"
                          | "k" -> print_s "&#x0137;"
                          | "L" -> print_s "&#x013B;"
                          | "l" -> print_s "&#x013C;"
                          | "N" -> print_s "&#x0145;"
                          | "n" -> print_s "&#x0146;"
                          | "R" -> print_s "&#x0156;"
                          | "r" -> print_s "&#x0157;"
                          | "S" -> print_s "&#x015E;"
                          | "s" -> print_s "&#x015F;"
                          | "T" -> print_s "&#x0162;"
                          | "t" -> print_s "&#x0163;"
			  | s -> print_s s ; print_s "&#x0327;")];
def "\\d" [Raw_arg(function "A" -> print_s "&#x1EA0;" (* dot-under accent *)
                         | "a" -> print_s "&#x1EA1;"
                         | "B" -> print_s "&#x1E04;"
                         | "b" -> print_s "&#x1E05;"
                         | "D" -> print_s "&#x1E0C;"
                         | "d" -> print_s "&#x1E0D;"
                         | "E" -> print_s "&#x1EB8;"
                         | "e" -> print_s "&#x1EB9;"
                         | "H" -> print_s "&#x1E24;"
                         | "h" -> print_s "&#x1E25;"
                         | "I" -> print_s "&#x1ECA;"
                         | "i" -> print_s "&#x1ECB;"
                         | "K" -> print_s "&#x1E32;"
                         | "k" -> print_s "&#x1E33;"
                         | "L" -> print_s "&#x1E36;"
                         | "l" -> print_s "&#x1E37;"
                         | "M" -> print_s "&#x1E42;"
                         | "m" -> print_s "&#x1E43;"
                         | "N" -> print_s "&#x1E46;"
                         | "n" -> print_s "&#x1E47;"
                         | "O" -> print_s "&#x1ECC;"
                         | "o" -> print_s "&#x1ECD;"
                         | "R" -> print_s "&#x1E5A;"
                         | "r" -> print_s "&#x1E5B;"
                         | "S" -> print_s "&#x1E62;"
                         | "s" -> print_s "&#x1E63;"
                         | "T" -> print_s "&#x1E6C;"
                         | "t" -> print_s "&#x1E6D;"
                         | "U" -> print_s "&#x1EE4;"
                         | "u" -> print_s "&#x1EE5;"
                         | "V" -> print_s "&#x1E7E;"
                         | "v" -> print_s "&#x1E7F;"
                         | "W" -> print_s "&#x1E88;"
                         | "w" -> print_s "&#x1E89;"
                         | "Y" -> print_s "&#x1EF4;"
                         | "y" -> print_s "&#x1EF5;"
                         | "Z" -> print_s "&#x1E92;"
                         | "z" -> print_s "&#x1E93;"
                         |  s -> print_s s ; print_s "&#x0323;")];
def "\\b" [Raw_arg(function "B" -> print_s "&#x1E06;"  (* bar-under accent *)
                         | "b" -> print_s "&#x1E07;"
                         | "D" -> print_s "&#x1E0E;"
                         | "d" -> print_s "&#x1E0F;"
                         | "h" -> print_s "&#x1E96;"
                         | "K" -> print_s "&#x1E34;"
                         | "k" -> print_s "&#x1E35;"
                         | "L" -> print_s "&#x1E3A;"
                         | "l" -> print_s "&#x1E3B;"
                         | "N" -> print_s "&#x1E48;"
                         | "n" -> print_s "&#x1E49;"
                         | "R" -> print_s "&#x1E5E;"
                         | "r" -> print_s "&#x1E5F;"
                         | "T" -> print_s "&#x1E6E;"
                         | "t" -> print_s "&#x1E6F;"
                         | "Z" -> print_s "&#x1E94;"
                         | "z" -> print_s "&#x1E95;"
                         | s -> print_s s ; print_s "&#x0331;")];
def "\\t" [Raw_arg(function s   -> print_s s ; print_s "&#x0361;")]; (* tie-after accent *)
  def "\\tilde"          [Raw_arg (function "i" -> print_s "<em>&#x129;</em>"
                                          | "j" -> print_s "<em>&#x237;&#x0303;</em>"
                                          | s -> print_s "<em>" ; print_s s ; print_s "&#x0303;</em>")];
  def "\\bar"		 [Raw_arg (function "i" -> print_s "<em>&#x131;&#x0304;</em>"
                                          | "j" -> print_s "<em>&#x237;&#x0304;</em>"
                                          | s -> print_s "<em>" ; print_s s ; print_s "&#x0304;</em>")];
  def "\\overbar"	 [Raw_arg (function "i" -> print_s "<em>&#x131;&#x0305;</em>"
                                          | "j" -> print_s "<em>&#x237;&#x0305;</em>"
                                          | s -> print_s "<em>" ; print_s s ; print_s "&#x0305;</em>")];
  def "\\vec"		 [Raw_arg (function "i" -> print_s "<em>&#x131;&#x20d7;</em>"
                                          | "j" -> print_s "<em>&#x237;&#x20d7;</em>"
                                          | s -> print_s "<em>" ; print_s s ; print_s "&#x20d7;</em>")];
  def "\\overrightarrow" [Raw_arg (function "i" -> print_s "<em>&#x131;&#x20d7;</em>"
                                          | "j" -> print_s "<em>&#x237;&#x20d7;</em>"
                                          | s -> print_s "<em>" ; print_s s ; print_s "&#x20d7;</em>")];
  def "\\overleftarrow"	 [Raw_arg (function "i" -> print_s "<em>&#x131;&#x20d6;</em>"             
                                         | "j" -> print_s "<em>&#x237;&#x20d6;</em>"			  
                                         | s -> print_s "<em>" ; print_s s ; print_s "&#x20d6;</em>")];
def "\\grave" [Raw_arg(function s -> print_s "<em>" ; print_s s ; print_s "&#x0300;</em>")];
def "\\acute" [Raw_arg(function s -> print_s "<em>" ; print_s s ; print_s "&#x0301;</em>")];
def "\\hat" [Raw_arg(function "a" -> print_s "<em>&acirc;</em>"
                          | "A" -> print_s "<em>&Acirc;</em>"
                          | "e" -> print_s "<em>&ecirc;</em>"
                          | "E" -> print_s "<em>&Ecirc;</em>"
                          | "i" -> print_s "<em>&icirc;</em>"
                          | "\\i" -> print_s "<em>&icirc;</em>"
                          | "I" -> print_s "<em>&Icirc;</em>"
                          | "o" -> print_s "<em>&ocirc;</em>"
                          | "O" -> print_s "<em>&Ocirc;</em>"
                          | "u" -> print_s "<em>&ucirc;</em>"
                          | "U" -> print_s "<em>&Ucirc;</em>"
			  | ""  -> print_c '^'
                          | s   -> print_s "<em>" ; print_s s ; print_s "&#x0302;</em>")];
def "\\breve" [Raw_arg(function "a" -> print_s "<em>&abreve;</em>"
                          | "A" -> print_s "<em>&Abreve;</em>"
                          | "u" -> print_s "<em>&ubreve;</em>"
                          | "U" -> print_s "<em>&Ubreve;</em>"
			  | ""  -> print_c '^'
                          | s   -> print_s "<em>" ; print_s s ; print_s "&#x0306;</em>")];
def "\\dot" [Raw_arg(function "C" -> print_s "<em>&Cdot;</em>"
                          | "c" -> print_s "<em>&cdot;</em>"
                          | "e" -> print_s "<em>&edot;</em>"
                          | "E" -> print_s "<em>&Edot;</em>"
                          | "g" -> print_s "<em>&gdot;</em>"
                          | "G" -> print_s "<em>&Gdot;</em>"
                          | "I" -> print_s "<em>&Idot;</em>"
                          | "Z" -> print_s "<em>&Zdot;</em>"
                          | "z" -> print_s "<em>&zdot;</em>"
                          | ""  -> print_s "<em>&#x02d9;</em>"
                          | s   -> print_s "<em>" ; print_s s ; print_s "&#x0307;</em>")];
def "\\ddot" [Raw_arg(function "i" -> print_s "<em>&#x0131;&#x0308;</em>"
                            | "j" -> print_s "<em>&#x0237;&#x0308;</em>"
                            | s   ->  print_s "<em>" ; print_s s ; print_s "&#x0308;</em>")];
def "\\check" [Raw_arg(function "C" -> print_s "<em>&Ccaron;</em>"
                          | "c" -> print_s "<em>&ccaron;</em>"
                          | "D" -> print_s "<em>&Dcaron;</em>"
                          | "d" -> print_s "<em>&dcaron;</em>"
                          | "E" -> print_s "<em>&Ecaron;</em>"
                          | "e" -> print_s "<em>&ecaron;</em>"
                          | "L" -> print_s "<em>&Lcaron;</em>"
                          | "l" -> print_s "<em>&lcaron;</em>"
                          | "N" -> print_s "<em>&Ncaron;</em>"
                          | "n" -> print_s "<em>&ncaron;</em>"
                          | "R" -> print_s "<em>&Rcaron;</em>"
                          | "r" -> print_s "<em>&rcaron;</em>"
                          | "S" -> print_s "<em>&Scaron;</em>"
                          | "s" -> print_s "<em>&scaron;</em>"
                          | "T" -> print_s "<em>&Tcaron;</em>"
                          | "t" -> print_s "<em>&tcaron;</em>"
                          | "Z" -> print_s "<em>&Zcaron;</em>"
                          | "z" -> print_s "<em>&zcaron;</em>"
                          | ""  -> print_s "<em>&#x02c7;</em>"
                          | s   -> print_s "<em>" ; print_s s ; print_s "&#x030c;</em>")];
def "\\\"" [Raw_arg(function "e" -> print_s "&euml;"
                          | "E" -> print_s "&Euml;"
                          | "a" -> print_s "&auml;"
                          | "A" -> print_s "&Auml;"
                          | "\\i" -> print_s "&iuml;"
                          | "i" -> print_s "&iuml;"
                          | "I" -> print_s "&Iuml;"
                          | "o" -> print_s "&ouml;"
                          | "O" -> print_s "&Ouml;"
                          | "u" -> print_s "&uuml;"
                          | "U" -> print_s "&Uuml;"
                          | "y" -> print_s "&yuml;"
                          | "Y" -> print_s "&Yuml;"
                          | s   -> print_s s ; print_s "&#x0308;")];
def "\\d" [Raw_arg print_s ];
def "\\." [Raw_arg (function "a" -> print_s "&#x227;"
			   | "A" -> print_s "&#x226;"
			   | "c" -> print_s "&#x10b;"
			   | "C" -> print_s "&#x10a;"
			   | "e" -> print_s "&#279;"
			   | "E" -> print_s "&#278;"
			   | "g" -> print_s "&#289;"
			   | "G" -> print_s "&#288;"
			   | "i" -> print_s "i"
			   | "\\i" -> print_s "i"
			   | "I" -> print_s "&#304;"
			   | "o" -> print_s "&#559;"
			   | "O" -> print_s "&#558;"
			   | "z" -> print_s "&#380;"
			   | "Z" -> print_s "&#379;"
			   | s   -> print_s s ; print_s "&#x0307;")];
def "\\u" [Raw_arg(function "a" -> print_s "&#x103;"
			   | "A" -> print_s "&#x102;"
			   | "e" -> print_s "&#x115;"
			   | "E" -> print_s "&#x114;"
			   | "i" -> print_s "&#x12d;"
			   | "\\i" -> print_s "&#x12d;"
			   | "I" -> print_s "&#x12c;"
			   | "g" -> print_s "&#x11F;"
			   | "G" -> print_s "&#x11E;"
			   | "o" -> print_s "&#x14F;"
			   | "O" -> print_s "&#x14E;"
			   | "u" -> print_s "&#x16D;"
			   | "U" -> print_s "&#x16C;"
			   | s   -> print_s s ; print_s "&#x0306;")];
def "\\v" [Raw_arg(function
                     | "C" -> print_s "&#x010C;"
		     | "c" -> print_s "&#x010D;"
                     | "D" -> print_s "&#270;"
		     | "d" -> print_s "&#271;"
                     | "E" -> print_s "&#282;"
		     | "e" -> print_s "&#283;"
                     | "N" -> print_s "&#327;"
		     | "n" -> print_s "&#328;"
		     | "r" -> print_s "&#X0159;"
                     | "R" -> print_s "&#X0158;"
                     | "s" -> print_s "&scaron;" (*"&#X0161;"*)
                     | "S" -> print_s "&Scaron;" (*"&#X0160;"*)
                     | "T" -> print_s "&#356;"
		     | "t" -> print_s "&#357;"
                     | "\\i" -> print_s "&#X012D;"
                     | "i" -> print_s "&#X012D;"
                     | "I" -> print_s "&#X012C;"
                     | "Z" -> print_s "&#381;"
		     | "z" -> print_s "&#382;"
		     | s   -> print_s s ; print_s "&#x030c;")];
def "\\H" [Raw_arg (function
		      | "O" -> print_s "&#336;"
		      | "o" -> print_s "&#337;"
		      | "U" -> print_s "&#368;"
		      | "u" -> print_s "&#369;"
		      | s -> print_s s ; print_s "&#x030b;")];
def "\\r" [Raw_arg (function
		      | "U" -> print_s "&#366;"
		      | "u" -> print_s "&#367;"
		      | s -> print_s s)];

(* Math macros *)
def "\\[" [Print "<blockquote>"];
def "\\]" [Print "\n</blockquote>"];
def "\\le" [Print "&lt;="];
def "\\leq" [Print "&lt;="];
def "\\log" [Print "log "];
def "\\ge" [Print "&gt;="];
def "\\geq" [Print "&gt;="];
def "\\neq" [Print "&lt;&gt;"];
def "\\circ" [Print "o"];
def "\\bigcirc" [Print "O"];
def "\\sim" [Print "~"];
def "\\(" [Print "<I>"];
def "\\)" [Print "</I>"];
def "\\mapsto" [Print "<tt>|-&gt;</tt>"];
def "\\times" [Print "&#215;"];
def "\\neg" [Print "&#172;"];
def "\\frac" [Print "("; Print_arg; Print ")/("; Print_arg; Print ")"];
def "\\not" [Print "not "];
def "\\arccos" [Print "arccos "];
def "\\arcsin" [Print "arcsin "];
def "\\arctan" [Print "arctan "];
def "\\arg" [Print "arg "];
def "\\cos" [Print "cos "];
def "\\cosh" [Print "cosh "];
def "\\coth" [Print "coth "];
def "\\cot" [Print "cot "];
def "\\csc" [Print "csc "];
def "\\deg" [Print "deg "];
def "\\det" [Print "det "];
def "\\dim" [Print "dim "];
def "\\exp" [Print "exp "];
def "\\gcd" [Print "gcd "];
def "\\hom" [Print "hom "];
def "\\inf" [Print "inf "];
def "\\ker" [Print "ker "];
def "\\lg" [Print "lg "];
def "\\lim" [Print "lim "];
def "\\liminf" [Print "liminf "];
def "\\limsup" [Print "limsup "];
def "\\ln" [Print "ln "];
def "\\max" [Print "max "];
def "\\min" [Print "min "];
def "\\Pr" [Print "Pr "];
def "\\sec" [Print "sec "];
def "\\sin" [Print "sin "];
def "\\sinh" [Print "sinh "];
def "\\sup" [Print "sup "];
def "\\tanh" [Print "tanh "];
def "\\tan" [Print "tan "];
def "\\over" [Print "/"];
def "\\lbrace" [Print "{"];
def "\\rbrace" [Print "}"];
def "\\cap" [Print "&#x2229;"];
def "\\wr" [Print "&#x2240;"];
def "\\uplus" [Print "&#x228e;"];
def "\\sqcap" [Print "&#x2293;"];
def "\\sqcup" [Print "&#x2294;"];
def "\\ominus" [Print "&#x2296;"];
def "\\oslash" [Print "&#x2298;"];
def "\\odot" [Print "&#x2299;"];
def "\\star" [Print "&#x22c6;"];
def "\\bigtriangleup" [Print "&#x25b3;"];
def "\\bigtriangleright" [Print "&#x25b7;"];
def "\\bigtriangledown" [Print "&#x25bd;"];
def "\\bigtriangleleft" [Print "&#x25c1;"];
def "\\setminus" [Print "&#x29f5;"];
def "\\amalg" [Print "&#x2a3f;"];
def "\\prime" [Print "&#x2032;"];
def "\\surd" [Print "&#x221a;"];
def "\\top" [Print "&#x22a4;"];
def "\\bot" [Print "&#x22a5;"];
def "\\hookleftarrow" [Print "&#x21a9;"];
def "\\hookrightarrow" [Print "&#x21aa;"];
def "\\leftharpoonup" [Print "&#x21bc;"];
def "\\leftharpoondown" [Print "&#x21bd;"];
def "\\rightharpoonup" [Print "&#x21c0;"];
def "\\rightharpoondown" [Print "&#x21c1;"];
def "\\imath" [Print "&#x1d6a4;"];
def "\\jmath" [Print "&#x1d6a5;"];

(* Math symbols printed as texts (could we do better?) *)
def "\\ne" [Print "=/="];
def "\\in" [Print "in"];
def "\\forall" [Print "for all"];
def "\\exists" [Print "there exists"];
def "\\vdash" [Print "|-"];
def "\\ln" [Print "ln"];
def "\\gcd" [Print "gcd"];
def "\\min" [Print "min"];
def "\\max" [Print "max"];
def "\\exp" [Print "exp"];
def "\\rightarrow" [Print "-&gt;"];
def "\\to" [Print "-&gt;"];
def "\\longrightarrow" [Print "--&gt;"];
def "\\Rightarrow" [Print "=&gt;"];
def "\\leftarrow" [Print "&lt;-"];
def "\\longleftarrow" [Print "&lt;--"];
def "\\Leftarrow" [Print "&lt;="];
def "\\leftrightarrow" [Print "&lt;-&gt;"];
def "\\sqrt" [Print "sqrt("; Print_arg; Print ")"];
def "\\vee" [Print "V"];
def "\\lor" [Print "V"];
def "\\wedge" [Print "/\\"];
def "\\land" [Print "/\\"];
def "\\Vert" [Print "||"];
def "\\parallel" [Print "||"];
def "\\mid" [Print "|"];
def "\\cup" [Print "U"];
def "\\inf" [Print "inf"];
def "\\div" [Print "/"];
def "\\quad" [Print "&nbsp;"];
def "\\qquad" [Print "&nbsp;&nbsp;"];


(* Misc. macros. *)
def "\\TeX" [Print "T<sub>E</sub>X"];
def "\\LaTeX" [Print "L<sup>A</sup>T<sub>E</sub>X"];
def "\\LaTeXe"
  [Print "L<sup>A</sup>T<sub>E</sub>X&nbsp;2<FONT FACE=symbol>e</FONT>"];
def "\\tm" [Print "<sup><font size=-1>TM</font></sup>"];
def "\\par" [Print "<p>"];
def "\\@" [Print " "];
def "\\#" [Print "#"];
def "\\/" [];
def "\\-" [];
def "\\left" [];
def "\\right" [];
def "\\smallskip" [];
def "\\medskip" [];
def "\\bigskip" [];
def "\\relax" [];
def "\\markboth" [Skip_arg; Skip_arg];
def "\\dots" [Print "..."];
def "\\simeq" [Print "&tilde;="];
def "\\approx" [Print "&tilde;"];
def "\\^circ" [Print "&deg;"];
def "\\ldots" [Print "..."];
def "\\cdot" [Print "&#183;"];
def "\\cdots" [Print "..."];
def "\\newpage" [];
def "\\hbox" [Print_arg];
def "\\noindent" [];
def "\\label" [Print "<A name=\""; Print_arg; Print "\"></A>"];
def "\\ref" [Print "<A href=\"#"; Print_arg; Print "\">(ref)</A>"];
def "\\index" [Skip_arg];
def "\\\\" [Print "<br>"];
def "\\," [];
def "\\;" [];
def "\\!" [];
def "\\hspace" [Skip_arg; Print " "];
def "\\symbol"
  [Raw_arg (function s ->
	      try let n = int_of_string s in print_c (Char.chr n)
	      with _ -> ())];
def "\\html" [Raw_arg print_s];
def "\\textcopyright" [Print "&copy;"];
def "\\textordfeminine" [Print "&ordf;"];
def "\\textordmasculine" [Print "&ordm;"];
def "\\backslash" [Print "&#92;"];


(* hyperref *)
def "\\href"
  [Print "<a href=\""; Raw_arg print_s;
   Print "\">"; Print_arg; Print "</a>"];

(* Bibliography *)
def "\\begin{thebibliography}" [Print "<H2>References</H2>\n<dl>\n"; Skip_arg];
def "\\end{thebibliography}" [Print "</dl>"];
def "\\bibitem" [Raw_arg (function r ->
  print_s "<dt><A name=\""; print_s r; print_s "\">[";
  print_s r; print_s "]</A>\n";
  print_s "<dd>")];

(* Greek letters *)
(***
List.iter
  (fun symbol -> def ("\\" ^ symbol) [Print ("<EM>" ^ symbol ^ "</EM>")])
  ["alpha";"beta";"gamma";"delta";"epsilon";"varepsilon";"zeta";"eta";
   "theta";"vartheta";"iota";"kappa";"lambda";"mu";"nu";"xi";"pi";"varpi";
   "rho";"varrho";"sigma";"varsigma";"tau";"upsilon";"phi";"varphi";
   "chi";"psi";"omega";"Gamma";"Delta";"Theta";"Lambda";"Xi";"Pi";
   "Sigma";"Upsilon";"Phi";"Psi";"Omega"];
***)
def "\\alpha" [Print "&alpha;"];
def "\\beta" [Print "&beta;"];
def "\\gamma" [Print "&gamma;"];
def "\\delta" [Print "&delta;"];
def "\\epsilon" [Print "&epsilon;"];
def "\\varepsilon" [Print "&epsilon;"];
def "\\zeta" [Print "&zeta;"];
def "\\eta" [Print "&eta;"];
def "\\theta" [Print "&theta;"];
def "\\vartheta" [Print "&theta;"];
def "\\iota" [Print "&iota;"];
def "\\kappa" [Print "&kappa;"];
def "\\lambda" [Print "&lambda;"];
def "\\mu" [Print "&mu;"];
def "\\nu" [Print "&nu;"];
def "\\xi" [Print "&xi;"];
def "\\pi" [Print "&pi;"];
def "\\varpi" [Print "&piv;"];
def "\\rho" [Print "&rho;"];
def "\\varrho" [Print "&rho;"];
def "\\sigma" [Print "&sigma;"];
def "\\varsigma" [Print "&sigmaf;"];
def "\\tau" [Print "&tau;"];
def "\\upsilon" [Print "&upsilon;"];
def "\\phi" [Print "&phi;"];
def "\\varphi" [Print "&phi;"];
def "\\chi" [Print "&chi;"];
def "\\psi" [Print "&psi;"];
def "\\omega" [Print "&omega;"];
def "\\Gamma" [Print "&Gamma;"];
def "\\Delta" [Print "&Delta;"];
def "\\Theta" [Print "&Theta;"];
def "\\Lambda" [Print "&Lambda;"];
def "\\Xi" [Print "&Xi;"];
def "\\Pi" [Print "&Pi;"];
def "\\Sigma" [Print "&Sigma;"];
def "\\Upsilon" [Print "&Upsilon;"];
def "\\Phi" [Print "&Phi;"];
def "\\Psi" [Print "&Psi;"];
def "\\Omega" [Print "&Omega;"];


(* macros for the AMS styles *)

def "\\bysame" [Print "<u>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;</u>"];
def "\\MR"
  [Raw_arg (fun s ->
	      let mr =
		try
		  let i = String.index s ' ' in
		  if i=0 then raise Not_found;
		  String.sub s 0 i
		with Not_found -> s
	      in
	      print_s "<a href=\"http://www.ams.org/mathscinet-getitem?mr=";
	      print_s mr; print_s "\">MR "; print_s s; print_s "</a>")];
def "\\MRhref"
  [Print "<a href=\"http://www.ams.org/mathscinet-getitem?mr=";
   Print_arg; Print "\">"; Print_arg; Print "</a>"];

(* macros for the aaai-named style *)

def "\\em" [];
def "\\protect" [];
def "\\bgroup" []; (* should go into latexscan? *)
def "\\egroup" []; (* should go into latexscan? *)
def "\\citename" [];

(* dashes *)
def "--" [Print "--"];
def "---" [Print "---"];
()

(* Unicode entities *)

let unicode_entities () =
  def "\\models" [Print "&#X22A7;"];
  def "\\curlyvee" [Print "&#X22CE;"];
  def "\\curlywedge" [Print "&#X22CF"];
  def "\\bigcirc" [Print "&#9711;"];
  def "\\varepsilon" [Print "&#603;"];
  def "--" [Print "&#x2013;"];
  def "---" [Print "&#x2014;"];
  def "\\ddagger" [Print "&#x2021;"];
  def "\\leftarrow" [Print "&#x2190;"];
  def "\\uparrow" [Print "&#x2191;"];
  def "\\rightarrow" [Print "&#x2192;"];
  def "\\downarrow" [Print "&#x2193;"];
  def "\\leftrightarrow" [Print "&#x2194;"];
  def "\\updownarrow" [Print "&#x2195;"];
  def "\\nwarrow" [Print "&#x2196;"];
  def "\\nearrow" [Print "&#x2197;"];
  def "\\searrow" [Print "&#x2198;"];
  def "\\swarrow" [Print "&#x2199;"];
  def "\\mapsto" [Print "&#x21a6;"];
  def "\\cup" [Print "&#x222a;"];
  def "\\infty" [Print "&#x221e;"];
  def "\\angle" [Print "&#x2220;"];
  def "\\Leftarrow" [Print "&#x21d0;"];
  def "\\Uparrow" [Print "&#x21d1;"];
  def "\\Rightarrow" [Print "&#x21d2;"];
  def "\\Downarrow" [Print "&#x21d3;"];
  def "\\Leftrightarrow" [Print "&#x21d4;"];
  def "\\Updownarrow" [Print "&#x21d5;"];
  def "\\propto" [Print "&#x221d;"];
  def "\\mid" [Print "&#x2223;"];
  def "\\parallel" [Print "&#x2225;"];
  def "\\sim" [Print "&#x223c;"];
  def "\\simeq" [Print "&#x2243;"];
  def "\\approx" [Print "&#x2248;"];
  def "\\asymp" [Print "&#x224d;"];
  def "\\ne" [Print "&#x2260;"];
  def "\\equiv" [Print "&#x2261;"];
  def "\\le" [Print "&#x2264;"];
  def "\\leq" [Print "&#x2264;"];
  def "\\ge" [Print "&#x2265;"];
  def "\\geq" [Print "&#x2265;"];
  def "\\ll" [Print "&#x226a;"];
  def "\\gg" [Print "&#x226b;"];
  def "\\ell" [Print "&#X2113;"];
  def "\\int" [Print "&#X222b;"];
  def "\\sum" [Print "&#X2211;"];
  def "\\prod" [Print "&#X220f;"];
  def "\\langle" [Print "&#X27e8;"];
  def "\\rangle" [Print "&#X27e9;"];
  def "\\prec" [Print "&#x227a;"];
  def "\\succ" [Print "&#x227b;"];
  def "\\subset" [Print "&#x2282;"];
  def "\\supset" [Print "&#x2283;"];
  def "\\subseteq" [Print "&#x2286;"];
  def "\\supseteq" [Print "&#x2287;"];
  def "\\sqsubseteq" [Print "&#x2291;"];
  def "\\sqsupseteq" [Print "&#x2292;"];
  def "\\vdash" [Print "&#x22a2;"];
  def "\\dashv" [Print "&#x22a3;"];
  def "\\bowtie" [Print "&#x22c8;"];
  def "\\vdots" [Print "&#x22ee;"];
  def "\\ddots" [Print "&#x22f1;"];
  def "\\frown" [Print "&#x2322;"];
  def "\\smile" [Print "&#x2323;"];
  def "\\perp" [Print "&#x27c2;"];
  def "\\longleftarrow" [Print "&#x27f5;"];
  def "\\longrightarrow" [Print "&#x27f6;"];
  def "\\longleftrightarrow" [Print "&#x27f7;"];
  def "\\Longleftarrow" [Print "&#x27f8;"];
  def "\\Longrightarrow" [Print "&#x27f9;"];
  def "\\Longleftrightarrow" [Print "&#x27fa;"];
  def "\\longmapsto" [Print "&#x27fc;"];
  def "\\preceq" [Print "&#x2aaf;"];
  def "\\succeq" [Print "&#x2ab0;"];
  def "\\Im" [Print "&#x2111;"];
  def "\\wp" [Print "&#x2118;"];
  def "\\Re" [Print "&#x211c;"];
  def "\\aleph" [Print "&#x2135;"];
  def "\\partial" [Print "&#x2202;"];
  def "\\nabla" [Print "&#x2207;"];
  def "\\not" [Raw_arg (function s -> print_s "not "; print_s s)];
  def "\\i" [Print "&#x0131;"];
  def "\\j" [Print "&#x0237;"];
  ()

let html_entities () =
  def "\\sqrt" [Print "&radic;("; Print_arg; Print ")"];
  def "\\copyright" [Print "&copy;"];
  def "\\tm" [Print "&trade;"];
  def "\\lang" [Print "&lang;"];
  def "\\rang" [Print "&rang;"];
  def "\\lceil" [Print "&lceil;"];
  def "\\rceil" [Print "&rceil;"];
  def "\\lfloor" [Print "&lfloor;"];
  def "\\rfloor" [Print "&rfloor;"];
  def "\\le" [Print "&le;"];
  def "\\leq" [Print "&le;"];
  def "\\ge" [Print "&ge;"];
  def "\\geq" [Print "&ge;"];
  def "\\ll" [Print "&NestedLessLess;"];
  def "\\gg" [Print "&NestedGreaterGreater;"];
  def "\\prec" [Print "&prec;"];
  def "\\succ" [Print "&succ;"];
  def "\\neq" [Print "&ne;"];
  def "\\approx" [Print "&asymp;"];
  def "\\asymp" [Print "&CupCap;"];
  def "\\cong" [Print "&cong;"];
  def "\\equiv" [Print "&equiv;"];
  def "\\propto" [Print "&prop;"];
  def "\\subset" [Print "&sub;"];
  def "\\subseteq" [Print "&sube;"];
  def "\\supset" [Print "&sup;"];
  def "\\supseteq" [Print "&supe;"];
  def "\\sqsubseteq" [Print "&sqsubseteq;"];
  def "\\sqsupseteq" [Print "&sqsupseteq;"];
  def "\\vdash" [Print "&vdash;"];
  def "\\dashv" [Print "&dashv;"];
  def "\\bowtie" [Print "&bowtie;"];
  def "\\vdots" [Print "&#x22ee;"]; (* fails: def "\\vdots" [Print "&velip;"]; *)
  def "\\ddots" [Print "&dtdot;"];
  def "\\frown" [Print "&frown;"];
  def "\\smile" [Print "&smile;"];
  def "\\ang" [Print "&ang;"];
  def "\\perp" [Print "&perp;"];
  def "\\therefore" [Print "&there4;"];
  def "\\sim" [Print "&sim;"];
  def "\\div" [Print "&divide;"];
  def "\\times" [Print "&times;"];
  def "\\ast" [Print "&lowast;"];
  def "\\otimes" [Print "&otimes;"];
  def "\\oplus" [Print "&oplus;"];
  def "\\lozenge" [Print "&loz;"];
  def "\\diamond" [Print "&loz;"];
  def "\\neg" [Print "&not;"];
  def "\\pm" [Print "&plusmn;"];
  def "\\dagger" [Print "&dagger;"];
  def "\\ddagger" [Print "&Dagger;"];
  def "\\ne" [Print "&ne;"];
  def "\\in" [Print "&isin;"];
  def "\\notin" [Print "&notin;"];
  def "\\ni" [Print "&ni;"];
  def "\\forall" [Print "&forall;"];
  def "\\exists" [Print "&exist;"];
  def "\\Re" [Print "&real;"];
  def "\\Im" [Print "&image;"];
  def "\\aleph" [Print "&alefsym;"];
  def "\\wp" [Print "&weierp;"];
  def "\\emptyset" [Print "&empty;"];
  def "\\nabla" [Print "&nabla;"];
  def "\\to" [Print "&rarr;"];
  def "\\longleftarrow" [Print "&longleftarrow;"];
  def "\\longrightarrow" [Print "&longrightarrow;"];
  def "\\longleftrightarrow" [Print "&longleftrightarrow;"];
  def "\\Longleftarrow" [Print "&#x27f8;"];
  def "\\Longrightarrow" [Print "&#x27f9;"];
  def "\\Longleftrightarrow" [Print "&#x27fa;"];
  def "\\longmapsto" [Print "&longmapsto;"];
  def "\\preceq" [Print "&PrecedesEqual;"];
  def "\\succeq" [Print "&SucceedsEqual;"];
  def "\\leftarrow" [Print "&larr;"];
  def "\\uparrow"[Print "&uarr;"];
  def "\\rightarrow" [Print "&rarr;"];
  def "\\downarrow"[Print "&darr;"];
  def "\\leftrightarrow" [Print "&harr;"];
  def "\\updownarrow" [Print "&varr;"];
  def "\\nwarrow" [Print "&nwarr;"];
  def "\\nearrow" [Print "&nearr;"];
  def "\\searrow" [Print "&searr;"];
  def "\\swarrow" [Print "&swarr;"];
  def "\\mapsto" [Print "&RightTeeArrow;"];
  def "\\Leftarrow" [Print "&lArr;"];
  def "\\Uparrow" [Print "&DoubleUpArrow;"];
  def "\\Rightarrow" [Print "&rArr;"];
  def "\\Downarrow" [Print "&DoubleDownArrow;"];
  def "\\sum" [Print "&sum;"];
  def "\\prod" [Print "&prod;"];
  def "\\int" [Print "&int;"];
  def "\\partial" [Print "&part;"];
  def "\\vee" [Print "&or;"];
  def "\\lor" [Print "&or;"];
  def "\\wedge" [Print "&and;"];
  def "\\land" [Print "&and;"];
  def "\\cup" [Print "&cup;"];
  def "\\infty" [Print "&infin;"];
  def "\\simeq" [Print "&cong;"];
  def "\\cdot" [Print "&sdot;"];
  def "\\cdots" [Print "&sdot;&sdot;&sdot;"];
  def "\\vartheta" [Print "&thetasym;"];
  def "\\angle" [Print "&ang;"];
  def "--" [Print "&ndash;"];
  def "---" [Print "&mdash;"];
  def "\\ll" [Print "&ll;"];
  def "\\gg" [Print "&gg;"];
  def "\\ell" [Print "&ell;"];
  def "\\langle" [Print "&langle;"];
  def "\\rangle" [Print "&rangle;"];
  def "\\i" [Print "&imath;"];
  def "\\j" [Print "&#x0237;"];
  ()

(*s Macros for German BibTeX style. *)

let is_german_style = function
  | "gerabbrv" | "geralpha" | "gerapali" | "gerplain" | "gerunsrt" -> true
  | _ -> false

let init_style_macros st =
  if is_german_style st then begin
    List.iter (fun (m,s) -> def m [ Print s; Print_arg ])
      [ "\\btxetalshort", "et al" ;
	"\\btxeditorshort", "Hrsg";
	"\\Btxeditorshort", "Hrsg";
	"\\btxeditorsshort", "Hrsg";
	"\\Btxeditorsshort", "Hrsg";
	"\\btxvolumeshort", "Bd";
	"\\Btxvolumeshort", "Bd";
	"\\btxnumbershort", "Nr";
	"\\Btxnumbershort", "Nr";
	"\\btxeditionshort", "Aufl";
	"\\Btxeditionshort", "Aufl";
	"\\btxchaptershort", "Kap";
	"\\Btxchaptershort", "Kap";
	"\\btxpageshort", "S";
	"\\Btxpageshort", "S";
	"\\btxpagesshort", "S";
	"\\Btxpagesshort", "S";
	"\\btxtechrepshort", "Techn. Ber";
	"\\Btxtechrepshort", "Techn. Ber";
	"\\btxmonjanshort", "Jan";
	"\\btxmonfebshort", "Feb";
	"\\btxmonaprshort", "Apr";
	"\\btxmonaugshort", "Aug";
	"\\btxmonsepshort", "Sep";
	"\\btxmonoctshort", "Okt";
	"\\btxmonnovshort", "Nov";
	"\\btxmondecshort", "Dez";
      ];
    List.iter (fun (m,s) -> def m [ Skip_arg; Print s])
      [ "\\btxetallong", "et alii";
	"\\btxandshort", "und";
	"\\btxandlong", "und";
	"\\btxinlong", "in:";
	"\\btxinshort", "in:";
	"\\btxofseriesshort", "d. Reihe";
	"\\btxinseriesshort", "in";
	"\\btxofserieslong", "der Reihe";
	"\\btxinserieslong", "in";
	"\\btxeditorlong", "Herausgeber";
	"\\Btxeditorlong", "Herausgeber";
	"\\btxeditorslong", "Herausgeber";
	"\\Btxeditorslong", "Herausgeber";
	"\\btxvolumelong", "Band";
	"\\Btxvolumelong", "Band";
	"\\btxnumberlong", "Nummer";
	"\\Btxnumberlong", "Nummer";
	"\\btxeditionlong", "Auflage";
	"\\Btxeditionlong", "Auflage";
	"\\btxchapterlong", "Kapitel";
	"\\Btxchapterlong", "Kapitel";
	"\\btxpagelong", "Seite";
	"\\Btxpagelong", "Seite";
	"\\btxpageslong", "Seiten";
	"\\Btxpageslong", "Seiten";
	"\\btxmastthesis", "Diplomarbeit";
	"\\btxphdthesis", "Doktorarbeit";
	"\\btxtechreplong", "Technischer Bericht";
	"\\Btxtechreplong", "Technischer Bericht";
	"\\btxmonjanlong", "Januar";
	"\\btxmonfeblong", "Februar";
	"\\btxmonmarlong", "März";
	"\\btxmonaprlong", "April";
	"\\btxmonmaylong", "Mai";
	"\\btxmonjunlong", "Juni";
	"\\btxmonjullong", "Juli";
	"\\btxmonauglong", "August";
	"\\btxmonseplong", "September";
	"\\btxmonoctlong", "Oktober";
	"\\btxmonnovlong", "November";
	"\\btxmondeclong", "Dezember";
	"\\btxmonmarshort", "März";
	"\\btxmonmayshort", "Mai";
	"\\btxmonjunshort", "Juni";
	"\\btxmonjulshort", "Juli";
	"\\Btxinlong", "In:";
	"\\Btxinshort", "In:";
      ]
  end
