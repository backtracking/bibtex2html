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

(*i $Id: latexscan.mll,v 1.21 2001-10-17 13:26:23 filliatr Exp $ i*)

(*s This code is Copyright (C) 1997 Xavier Leroy. *)

{
  open Printf
  open Latexmacros

  let brace_nesting = ref 0
  let math_mode = ref false
  let hevea_url = ref false

  let save_nesting f arg =
    let n = !brace_nesting in 
    brace_nesting := 0;
    f arg;
    brace_nesting := n

  let save_state f arg =
    let n = !brace_nesting and m = !math_mode in
    brace_nesting := 0;
    math_mode := false;
    f arg;
    brace_nesting := n;
    math_mode := m

  let verb_delim = ref (Char.chr 0)

  let r = Str.regexp "[ \t\n]+"
  let remove_whitespace u = Str.global_replace r "" u

  let print_latex_url u =
    let u = remove_whitespace u in
    print_s (sprintf "<A HREF=\"%s\">%s</A>" u u)
  
  let print_hevea_url u t = 
    let u = remove_whitespace u in
    print_s (sprintf "<A HREF=\"%s\">%s</A>" u t)

}

rule main = parse
(* Comments *)
    '%' [^ '\n'] * '\n' { main lexbuf }
(* Paragraphs *)
  | "\n\n" '\n' *
                { print_s "<P>\n"; main lexbuf }
(* Font changes *)
  | "{\\it" " "* | "{\\em" " "* | "{\\sl" " "*
  | "{\\itshape" " "* | "{\\slshape" " "*
                  { print_s "<i>";
                    save_state main lexbuf;
                    print_s "</i>"; main lexbuf }
  | "{\\bf" " "* | "{\\sf" " "* | "{\\bfseries" " "* | "{\\sffamily" " "*
                  { print_s "<b>";
                    save_state main lexbuf;
                    print_s "</b>"; main lexbuf }
  | "{\\sc" " "*  | "{\\scshape" " "* | "{\\normalfont" " "* 
  | "{\\upshape" " "* | "{\\mdseries" " "* | "{\\rmfamily" " "* 
                  { save_state main lexbuf; main lexbuf }
  | "{\\tt" " "* | "{\\ttfamily" " "* 
                  { print_s "<tt>";
                    save_state main lexbuf;
                    print_s "</tt>"; main lexbuf }
  | '"'           { print_s "<tt>"; indoublequote lexbuf;
                    print_s "</tt>"; main lexbuf }
(* Verb, verbatim *)
  | ("\\verb" | "\\path") _  
                { verb_delim := Lexing.lexeme_char lexbuf 5;
                  print_s "<tt>"; inverb lexbuf; print_s "</tt>";
                  main lexbuf }
  | "\\begin{verbatim}"
                { print_s "<pre>"; inverbatim lexbuf;
                  print_s "</pre>"; main lexbuf }
(* Raw html, latex only *)
  | "\\begin{rawhtml}"
                { rawhtml lexbuf; main lexbuf }
  | "\\begin{latexonly}"
                { latexonly lexbuf; main lexbuf }
(* Itemize and similar environments *)
  | "\\item[" [^ ']']* "]"
                { print_s "<dt>";
                  let s = Lexing.lexeme lexbuf in
                  print_s (String.sub s 6 (String.length s - 7));
                  print_s "<dd>"; main lexbuf }
  | "\\item"    { print_s "<li>"; main lexbuf }
(* Math mode (hmph) *)
  | "$"         { math_mode := not !math_mode; main lexbuf }
  | "$$"        { math_mode := not !math_mode;
                  if !math_mode
                  then print_s "<blockquote>"
                  else print_s "\n</blockquote>";
                  main lexbuf }
(* Special characters *)
  | "\\char" ['0'-'9']+
                { let lxm = Lexing.lexeme lexbuf in
                  let code = String.sub lxm 5 (String.length lxm - 5) in
                  print_c(Char.chr(int_of_string code));
                  main lexbuf }
  | "--" | "---"
                { print_s "-"; main lexbuf }
  | "<"         { print_s "&lt;"; main lexbuf }
  | ">"         { print_s "&gt;"; main lexbuf }
  | "~"         { print_s " "; main lexbuf }
  | "^"         { if !math_mode then begin
		    let buf = Lexing.from_string (raw_arg lexbuf) in
		    print_s "<sup>";
		    save_state main buf;
		    print_s"</sup>"
		  end else
		    print_s "^"; 
		  main lexbuf }
  | "_"         { if !math_mode then begin
		    let buf = Lexing.from_string (raw_arg lexbuf) in
		    print_s "<sub>";
		    save_state main buf;
		    print_s"</sub>"
		  end else
		    print_s "_"; 
		  main lexbuf }
(* URLs *)
  | "\\url" { let url = raw_arg lexbuf in
	      if !hevea_url then
		let text = raw_arg lexbuf in print_hevea_url url text
	      else
		print_latex_url url;
	      main lexbuf }
(* General case for environments and commands *)
  | ("\\begin{" | "\\end{") ['A'-'Z' 'a'-'z']+ "}" |
    "\\" (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
                { let rec exec_action = function
                    | Print str -> print_s str
                    | Print_arg -> print_arg lexbuf
                    | Raw_arg f -> f (raw_arg lexbuf)
                    | Skip_arg -> save_nesting skip_arg lexbuf
		    | Recursive s -> main (Lexing.from_string s)
		    | Parameterized f ->
			List.iter exec_action (f (raw_arg lexbuf))
		  in
                  List.iter exec_action (find_macro(Lexing.lexeme lexbuf));
                  main lexbuf }
(* Nesting of braces *)
  | '{'         { incr brace_nesting; main lexbuf }
  | '}'         { if !brace_nesting <= 0
                  then ()
                  else begin decr brace_nesting; main lexbuf end }
(* Default rule for other characters *)
  | eof         { () }
  | ['A'-'Z' 'a'-'z']+
                { if !math_mode then print_s "<EM>";
                  print_s(Lexing.lexeme lexbuf);
                  if !math_mode then print_s "</EM>";
                  main lexbuf }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); main lexbuf }

and indoublequote = parse
    '"'         { () }
  | "<"         { print_s "&lt;"; indoublequote lexbuf }
  | ">"         { print_s "&gt;"; indoublequote lexbuf }
  | "&"         { print_s "&amp;"; indoublequote lexbuf }
  | "\\\""      { print_s "\""; indoublequote lexbuf }
  | "\\\\"      { print_s "\\"; indoublequote lexbuf }
  | eof         { () }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); indoublequote lexbuf }

and inverb = parse
    "<"         { print_s "&lt;"; inverb lexbuf }
  | ">"         { print_s "&gt;"; inverb lexbuf }
  | "&"         { print_s "&amp;"; inverb lexbuf }
  | eof         { () }
  | _           { let c = Lexing.lexeme_char lexbuf 0 in
                  if c == !verb_delim then ()
                                      else (print_c c; inverb lexbuf) }
and inverbatim = parse
    "<"         { print_s "&lt;"; inverbatim lexbuf }
  | ">"         { print_s "&gt;"; inverbatim lexbuf }
  | "&"         { print_s "&amp;"; inverbatim lexbuf }
  | "\\end{verbatim}" { () }
  | eof         { () }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); inverbatim lexbuf }
  
and rawhtml = parse
    "\\end{rawhtml}" { () }
  | eof         { () }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); rawhtml lexbuf }

and latexonly = parse
    "\\end{latexonly}" { () }
  | eof         { () }
  | _           { latexonly lexbuf }

and print_arg = parse
    "{"         { save_nesting main lexbuf }
  | "["         { skip_optional_arg lexbuf; print_arg lexbuf }
  | " "         { print_arg lexbuf }
  | eof         { () }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); main lexbuf }

and skip_arg = parse
    "{"         { incr brace_nesting; skip_arg lexbuf }
  | "}"         { decr brace_nesting;
                  if !brace_nesting > 0 then skip_arg lexbuf }
  | "["         { if !brace_nesting = 0 then skip_optional_arg lexbuf;
                  skip_arg lexbuf }
  | " "         { skip_arg lexbuf }
  | eof         { () }
  | _           { if !brace_nesting > 0 then skip_arg lexbuf }

and raw_arg = parse
    " "         { raw_arg lexbuf }
  | '{'         { nested_arg lexbuf }
  | "["         { skip_optional_arg lexbuf; raw_arg lexbuf }
  | '\\' ['A'-'Z' 'a'-'z']+
                { Lexing.lexeme lexbuf }
  | eof         { "" }
  | _           { Lexing.lexeme lexbuf }

and nested_arg = parse
    '}'         { "" }
  | '{'         { let l = nested_arg lexbuf in
		  "{" ^ l ^ "}" ^ (nested_arg lexbuf) }
  | eof         { "" }
  | [^ '{' '}']+{ let x = Lexing.lexeme lexbuf in
		  x ^ (nested_arg lexbuf)   }

and skip_optional_arg = parse
    "]"         { () }
  | eof         { () }
  | _           { skip_optional_arg lexbuf }


(* ajout personnel: [read_macros] pour lire les macros (La)TeX *)

and read_macros = parse
    "\\def" | "\\newcommand"
        { read_def lexbuf; read_macros lexbuf }
  | eof { () }
  | _   { read_macros lexbuf }

and read_def = parse
    '\\' ['a'-'z' 'A'-'Z']+
      { let s = Lexing.lexeme lexbuf in
	let b = raw_arg lexbuf in
	if not !Options.quiet then begin
	  eprintf "macro: %s = %s\n" s b; 
	  flush stderr
	end;
	def s [Recursive b] }
  | "{\\" ['a'-'z' 'A'-'Z']+ "}"
      { let l = Lexing.lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
	let b = raw_arg lexbuf in
	if not !Options.quiet then begin
	  eprintf "macro: %s = %s\n" s b; 
	  flush stderr
	end;
	def s [Recursive b] }
  | [' ' '\t' '\n']* 
      { read_def lexbuf }
  | _ { () }

