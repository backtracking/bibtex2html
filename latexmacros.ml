(*
 * latexmacros.ml
 *
 * Code initially written by Xavier Leroy.
 *)

(* output *)

let out_channel = ref stdout
let print_s s = output_string !out_channel s
let print_c c = output_char !out_channel c;;


type action =
    Print of string
  | Print_arg
  | Skip_arg
  | Raw_arg of (string -> unit)

let cmdtable = (Hashtbl.create 19 : (string, action list) Hashtbl.t);;

let def name action =
  Hashtbl.add cmdtable name action;;

let find_macro name =
  try
    Hashtbl.find cmdtable name
  with Not_found ->
    prerr_string "Unknown macro: "; prerr_endline name; [];;

(* General LaTeX macros *)

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
def "\\begin{alltt}" [Print "<pre>"];
def "\\end{alltt}" [Print "</pre>"];
def "\\begin{itemize}" [Print "<p><ul>"];
def "\\end{itemize}" [Print "</ul>"];
def "\\begin{enumerate}" [Print "<p><ol>"];
def "\\end{enumerate}" [Print "</ol>"];
def "\\begin{description}" [Print "<p><dl>"];
def "\\end{description}" [Print "</dl>"];
def "\\begin{center}" [Print "<blockquote>"];
def "\\end{center}" [Print "</blockquote>"];
def "\\smallskip" [];
def "\\medskip" [];
def "\\bigskip" [];
def "\\markboth" [Skip_arg; Skip_arg];
def "\\ldots" [Print "..."];
def "\\ " [Print " "];
def "\\{" [Print "{"];
def "\\}" [Print "}"];
def "\\/" [];
def "\\newpage" [];
def "\\label" [Print "<A name=\""; Print_arg; Print "\"></A>"];
def "\\ref" [Print "<A href=\"#"; Print_arg; Print "\">(ref)</A>"];
def "\\index" [Skip_arg];
def "\\oe" [Print "oe"];
def "\\&" [Print "&amp;"];
def "\\_" [Print "_"];
def "\\leq" [Print "&lt;="];
def "\\geq" [Print "&gt;="];
def "\\hbox" [Print_arg];
def "\\copyright" [Print "(c)"];
def "\\noindent" [];
def "\\begin{flushleft}" [Print "<blockquote>"];
def "\\end{flushleft}" [Print "</blockquote>"];
def "\\\\" [Print "<br>"];
def "\\(" [Print "<I>"];
def "\\)" [Print "</I>"];
def "\\cite" [Raw_arg(function r -> 
  print_s ("<A HREF=\"biblio.html#" ^ r ^ "\">[" ^ r ^ "]</A>"))];
def "\\begin{htmlonly}" [];
def "\\end{htmlonly}" [];
def "\\begin{thebibliography}" [Print "<H2>References</H2>\n<dl>\n"; Skip_arg];
def "\\end{thebibliography}" [Print "</dl>"];
def "\\bibitem" [Raw_arg(function r ->
  print_s "<dt><A name=\""; print_s r; print_s "\">[";
  print_s r; print_s "]</A>\n";
  print_s "<dd>")];
def "\\newblock" [Print "<br>"];
def "\\'" [Raw_arg(function "e" -> print_c 'é'
                          | "E" -> print_c 'É'
                          | s   -> print_s s)];
def "\\`" [Raw_arg(function "e" -> print_c 'è'
                          | "E" -> print_c 'È'
                          | "a" -> print_c 'à'
                          | "A" -> print_c 'À'
                          | "u" -> print_c 'ù'
                          | "U" -> print_c 'Ù'
                          | s   -> print_s s)];
def "\\~" [Raw_arg(function "n" -> print_c 'ñ'
                          | s   -> print_s s)];
def "\\^" [Raw_arg(function "a" -> print_c 'â'
                          | "A" -> print_c 'Â'
                          | "e" -> print_c 'ê'
                          | "E" -> print_c 'Ê'
                          | "i" -> print_c 'î'
                          | "I" -> print_c 'Î'
                          | "o" -> print_c 'ô'
                          | "O" -> print_c 'Ô'
                          | "u" -> print_c 'û'
                          | "U" -> print_c 'Û'
                          | s   -> print_s s)];
def "\\\"" [Raw_arg(function "e" -> print_c 'ë'
                          | "E" -> print_c 'Ë'
                          | "\\i" -> print_c 'ï'
                          | "i" -> print_c 'ï'
                          | "I" -> print_c 'Ï'
                          | "o" -> print_c 'ü'
                          | "O" -> print_c 'Ü'
                          | s   -> print_s s)];
();;

(* Pseudo-math mode *)

List.iter (fun symbol -> def ("\\" ^ symbol) [Print ("<I>" ^ symbol ^ "</I>")])
  ["alpha";"beta";"gamma";"delta";"epsilon";"varepsilon";"zeta";"eta";
   "theta";"vartheta";"iota";"kappa";"lambda";"mu";"nu";"xi";"pi";"varpi";
   "rho";"varrho";"sigma";"varsigma";"tau";"upsilon";"phi";"varphi";
   "chi";"psi";"omega";"Gamma";"Delta";"Theta";"Lambda";"Xi";"Pi";
   "Sigma";"Upsilon";"Phi";"Psi";"Omega"];
def "\\," [];
def "\\mapsto" [Print "<tt>|-&gt;</tt>"];
();;

(* My personal macros *)

def "\\th" [Print "-th"];
def "\\st" [Print "st"];
def "\\nd" [Print "nd"];
def "\\rd" [Print "rd"];
def "\\ier" [Print "ier"];
def "\\iere" [Print "ière"];
def "\\ieme" [Print "ième"];
def "\\eme" [Print "e"];
def "\\eg" [Print "e.g. "];
def "\\ie" [Print "i.e.. "];
def "\\etal" [Print "<I>et al.</I> "];
def "\\begin{prop}" [Print "<p><strong>Proposition.</strong> <I>"];
def "\\end{prop}" [Print "</I><p>"];
def "\\comment" [Print "<I>(*"; Print_arg; Print "*)</I>"];
def "\\stringlit" [Print "\"<B>"; Print_arg; Print "</B>\""];
def "\\becomes" [Print "<tt>&lt;-</tt>"];
def "\\biling" [Print_arg; Skip_arg];
def "\\triling" [Print_arg; Skip_arg; Skip_arg];
def "\\abbrev" [Print_arg; Skip_arg];
def "\\citeurl" [Print "<A href=\""; Raw_arg print_s; Print "\">(url)</A>"];
def "\\Cplusplus" [Print "C++"];
();;

