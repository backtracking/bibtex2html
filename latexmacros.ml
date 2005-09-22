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

(*i $Id: latexmacros.ml,v 1.54 2005-09-22 08:44:23 filliatr Exp $ i*)

(*s This code is Copyright (C) 1997  Xavier Leroy. *)

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

(* Basic color support. *)

def "\\textcolor" [ Parameterized (function name ->
  match String.lowercase name with
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
def "\\oe" [Print "oe"];       (*r There is no \oe{} in HTML. *)
def "\\o" [Print "&oslash;"];
def "\\O" [Print "&Oslash;"];
def "\\ae" [Print "&aelig;"];
def "\\AE" [Print "&AElig;"];
def "\\aa" [Print "&aring;"];
def "\\AA" [Print "&Aring;"];
def "\\&" [Print "&amp;"];
def "\\$" [Print "$"];
def "\\%" [Print "%"];
def "\\_" [Print "_"];
def "\\copyright" [Print "(c)"];
def "\\th" [Print "&thorn;"];
def "\\TH" [Print "&THORN;"];
def "\\dh" [Print "&eth;"];
def "\\DH" [Print "&ETH;"];
def "\\ss" [Print "&szlig;"];
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
			  | "c" -> print_s "&#263;"
			  | ""  -> print_c '\''
                          | s   -> print_s s)];
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
                          | s   -> print_s s)];
def "\\~" [Raw_arg(function "n" -> print_s "&ntilde;"
		          | ""  -> print_s "&tilde;"
                          | s   -> print_s s)];
def "\\c" [Raw_arg(function "c" -> print_s "&ccedil;"
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
			  | ""  -> print_c '^'
                          | s   -> print_s s)];
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
                          | s   -> print_s s)];
def "\\u" [Raw_arg print_s ];
def "\\v" [Raw_arg(function "r" -> print_s "&#X0159;"
                          | "R" -> print_s "&#X0158;"
                          | "s" -> print_s "&#X0161;"
                          | "S" -> print_s "&#X0160;"
                          | "\\i" -> print_s "&#X012D;"
                          | "i" -> print_s "&#X012D;"
                          | "I" -> print_s "&#X012C;"
			  | s   -> print_s s)];

(* Math macros *)
def "\\[" [Print "<blockquote>"];
def "\\]" [Print "\n</blockquote>"];
def "\\le" [Print "&lt;="];
def "\\leq" [Print "&lt;="];
def "\\log" [Print "log"];
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
def "\\parallel" [Print "||"];
def "\\mid" [Print "|"];
def "\\cup" [Print "U"];
def "\\inf" [Print "inf"];

(* Misc. macros. *)
def "\\TeX" [Print "T<sub>E</sub>X"];
def "\\LaTeX" [Print "L<sup>A</sup>T<sub>E</sub>X"];
def "\\LaTeXe" 
  [Print "L<sup>A</sup>T<sub>E</sub>X&nbsp;2<FONT FACE=symbol>e</FONT>"];
def "\\tm" [Print "<sup><font size=-1>TM</font></sup>"];
def "\\par" [Print "<p>"];
def "\\@" [];
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
def "\\dot" [Print "."];
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

(* hyperref *)
def "\\href" 
  [Print "<a href=\""; Raw_arg (fun s -> print_s (Html.quote_amp s)); 
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
	      print_s "<a href=\"http://www.ams.org/mathscinet-getitem?mr=";
	      print_s s; print_s "\">MR "; print_s s; print_s "</a>")];
def "\\MRhref" 
  [Print "<a href=\"http://www.ams.org/mathscinet-getitem?mr=";
   Print_arg; Print "\">"; Print_arg; Print "</a>"];

(* macros for the aaai-named style *)

def "\\em" [];
def "\\protect" [];
def "\\bgroup" []; (* should go into latexscan? *)
def "\\egroup" []; (* should go into latexscan? *)
def "\\citename" [];

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
