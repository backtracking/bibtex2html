(*
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
 *)

(* $Id: latexmacros.ml,v 1.16 1999-01-21 15:10:12 filliatr Exp $ *)

(* This code is Copyright (C) 1997  Xavier Leroy. *)

(* output *)

let out_channel = ref stdout
let print_s s = output_string !out_channel s
let print_c c = output_char !out_channel c;;


type action =
    Print of string
  | Print_arg
  | Skip_arg
  | Raw_arg of (string -> unit)
  | Recursive of string  (* phrase LaTeX à analyser récursivement *)

let cmdtable = (Hashtbl.create 19 : (string, action list) Hashtbl.t);;

let def name action =
  Hashtbl.add cmdtable name action;;

let find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
    prerr_string "Unknown macro: "; prerr_endline name; [];;

(* General LaTeX macros *)

(* sectioning *)
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

(* text formatting *)
def "\\begin{alltt}" [Print "<pre>"];
def "\\end{alltt}" [Print "</pre>"];
def "\\texttt" [Print "<tt>" ; Print_arg ; Print "</tt>"];
def "\\textem" [Print "<em>" ; Print_arg ; Print "</em>"];
def "\\textbf" [Print "<b>" ; Print_arg ; Print "</b>"];
def "\\emph" [Print "<em>" ; Print_arg ; Print "</em>"];
def "\\mbox" [Print_arg];
def "\\footnotesize" [];

(* environments *)
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

(* special characters *)
def "\\ " [Print " "];
def "\\\n" [Print " "];
def "\\{" [Print "{"];
def "\\}" [Print "}"];
def "\\l" [Print "l"];
def "\\oe" [Print "oe"];       (* Il n'y a pas de oe liés en HTML *)
def "\\o" [Print "&oslash;"];
def "\\O" [Print "&Oslash;"];
def "\\ae" [Print "&aelig;"];
def "\\AE" [Print "&AElig;"];
def "\\aa" [Print "&aring;"];
def "\\AA" [Print "&Aring;"];
def "\\&" [Print "&amp;"];
def "\\_" [Print "_"];
def "\\copyright" [Print "(c)"];
def "\\TH" [Print "\222"];
def "\\dh" [Print "\240"];
def "\\'" [Raw_arg(function "e" -> print_c 'é'
                          | "E" -> print_c 'É'
			  | "a" -> print_c 'á'
			  | "A" -> print_c 'Á'
			  | "o" -> print_c 'ó'
			  | "O" -> print_c 'Ó'
			  | "i" -> print_c 'í'
                          | "\\i" -> print_c 'í'
			  | "I" -> print_c 'Í'
			  | "u" -> print_c 'ú'
			  | "U" -> print_c 'Ú'
                          | s   -> print_s s)];
def "\\`" [Raw_arg(function "e" -> print_c 'è'
                          | "E" -> print_c 'È'
                          | "a" -> print_c 'à'
                          | "A" -> print_c 'À'
			  | "o" -> print_c 'ò'
			  | "O" -> print_c 'Ò'
			  | "i" -> print_c 'ì'
                          | "\\i" -> print_c 'ì'
			  | "I" -> print_c 'Ì'
                          | "u" -> print_c 'ù'
                          | "U" -> print_c 'Ù'
                          | s   -> print_s s)];
def "\\~" [Raw_arg(function "n" -> print_c 'ñ'
                          | s   -> print_s s)];
def "\\c" [Raw_arg(function "c" -> print_c 'ç'
                          | s   -> print_s s)];
def "\\^" [Raw_arg(function "a" -> print_c 'â'
                          | "A" -> print_c 'Â'
                          | "e" -> print_c 'ê'
                          | "E" -> print_c 'Ê'
                          | "i" -> print_c 'î'
                          | "\\i" -> print_c 'î'
                          | "I" -> print_c 'Î'
                          | "o" -> print_c 'ô'
                          | "O" -> print_c 'Ô'
                          | "u" -> print_c 'û'
                          | "U" -> print_c 'Û'
                          | s   -> print_s s)];
def "\\\"" [Raw_arg(function "e" -> print_c 'ë'
                          | "E" -> print_c 'Ë'
                          | "a" -> print_c 'ä'
                          | "A" -> print_c 'Ä'
                          | "\\i" -> print_c 'ï'
                          | "i" -> print_c 'ï'
                          | "I" -> print_c 'Ï'
                          | "o" -> print_c 'ö'
                          | "O" -> print_c 'Ö'
                          | "u" -> print_c 'ü'
                          | "U" -> print_c 'Ü'
                          | s   -> print_s s)];

(* math macros *)
def "\\leq" [Print "&lt;="];
def "\\log" [Print "log"];
def "\\geq" [Print "&gt;="];
def "\\circ" [Print "o"];
def "\\(" [Print "<I>"];
def "\\)" [Print "</I>"];
def "\\mapsto" [Print "<tt>|-&gt;</tt>"];

(* misc. macros *)
def "\\/" [];
def "\\-" [];
def "\\smallskip" [];
def "\\medskip" [];
def "\\bigskip" [];
def "\\hskip" [];
def "\\markboth" [Skip_arg; Skip_arg];
def "\\dots" [Print "..."];
def "\\ldots" [Print "..."];
def "\\cdots" [Print "..."];
def "\\newpage" [];
def "\\hbox" [Print_arg];
def "\\noindent" [];
def "\\label" [Print "<A name=\""; Print_arg; Print "\"></A>"];
def "\\ref" [Print "<A href=\"#"; Print_arg; Print "\">(ref)</A>"];
def "\\index" [Skip_arg];
def "\\\\" [Print "<br>"];
def "\\," [];

(* Bibliography *)
def "\\begin{thebibliography}" [Print "<H2>References</H2>\n<dl>\n"; Skip_arg];
def "\\end{thebibliography}" [Print "</dl>"];
def "\\bibitem" [Raw_arg (function r ->
  print_s "<dt><A name=\""; print_s r; print_s "\">[";
  print_s r; print_s "]</A>\n";
  print_s "<dd>")];

();;

(* greek letters *)
List.iter (fun symbol -> def ("\\" ^ symbol) [Print ("<I>" ^ symbol ^ "</I>")])
  ["alpha";"beta";"gamma";"delta";"epsilon";"varepsilon";"zeta";"eta";
   "theta";"vartheta";"iota";"kappa";"lambda";"mu";"nu";"xi";"pi";"varpi";
   "rho";"varrho";"sigma";"varsigma";"tau";"upsilon";"phi";"varphi";
   "chi";"psi";"omega";"Gamma";"Delta";"Theta";"Lambda";"Xi";"Pi";
   "Sigma";"Upsilon";"Phi";"Psi";"Omega"];
();;


