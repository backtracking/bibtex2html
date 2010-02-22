(**************************************************************************)
(*  bibtex2html - A BibTeX to HTML translator                             *)
(*  Copyright (C) 1997-2010 Jean-Christophe Filliâtre and Claude Marché   *)
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

(*i $Id: latexscan.mll,v 1.40 2010-02-22 07:38:19 filliatr Exp $ i*)

(*s This code is Copyright (C) 1997 Xavier Leroy. *)

{
  open Printf
  open Latexmacros

  type math_mode = MathNone | MathDisplay | MathNoDisplay

  let brace_nesting = ref 0
  let math_mode = ref MathNone

  let is_math_mode () = 
    match !math_mode with
      | MathNone -> false
      | MathDisplay | MathNoDisplay -> true

  let hevea_url = ref false
  let html_entities = ref false

  let save_nesting f arg =
    let n = !brace_nesting in 
    brace_nesting := 0;
    f arg;
    brace_nesting := n

  let save_state f arg =
    let n = !brace_nesting and m = !math_mode in
    brace_nesting := 0;
    math_mode := MathNone;
    f arg;
    brace_nesting := n;
    math_mode := m

  let verb_delim = ref (Char.chr 0)

  let r = Str.regexp "[ \t\n]+"
  let remove_whitespace u = Str.global_replace r "" u

  let print_latex_url u =
    let u = remove_whitespace u in
    print_s (sprintf "<a href=\"%s\">%s</a>" u u)
  
  let print_hevea_url u t = 
    let u = remove_whitespace u in
    print_s (sprintf "<a href=\"%s\">%s</a>" u t)

  let chop_last_space s =
    let n = String.length s in
    if s.[n-1] = ' ' then String.sub s 0 (n-1) else s

  let def_macro s n b =
    if not !Options.quiet then begin
      eprintf "macro: %s = %s\n" s b; 
      flush stderr
    end;
    let n = match n with None -> 0 | Some n -> int_of_string n in
    let rec code i subst = 
      if i <= n then
	let r = Str.regexp ("#" ^ string_of_int i) in
	[Parameterized 
	    (fun arg -> 
	      let subst s = Str.global_replace r (subst s) arg in
	      code (i+1) subst)]
      else begin
	let _s = subst b in
	(* eprintf "subst b = %s\n" s; flush stderr; *)
	[Recursive (subst b)]
      end
    in
    def s (code 1 (fun s -> s))

}

let space = [' ' '\t' '\n' '\r']
let float = '-'? (['0'-'9']+ | ['0'-'9']* '.' ['0'-'9']*)
let dimension = float ("sp" | "pt" | "bp" | "dd" | "mm" | "pc" |
		       "cc" | "cm" | "in" | "ex" | "em" | "mu")

rule main = parse
(* Comments *)
    '%' [^ '\n'] * '\n' { main lexbuf }
(* Paragraphs *)
  | "\n\n" '\n' *
                { print_s "<p>\n"; main lexbuf }
(* Font changes *)
  | "{\\it" " "* | "{\\itshape" " "*
                  { print_s "<i>";
                    save_state main lexbuf;
                    print_s "</i>"; main lexbuf }
  | "{\\em" " "* | "{\\sl" " "* | "{\\slshape" " "*
                  { print_s "<em>";
                    save_state main lexbuf;
                    print_s "</em>"; main lexbuf }
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
  | "{\\small" " "*
                  { print_s "<font size=\"-1\">";
                    save_state main lexbuf;
                    print_s "</font>"; main lexbuf }
  | "{\\rm" " "*
                  { print_s "<span style=\"font-style: normal\">";
                    save_state main lexbuf;
                    print_s "</span>"; main lexbuf }
  | "{\\cal" " "*
                  { save_state main lexbuf; main lexbuf }
  | "\\cal" " "*  { main lexbuf }
(* Double quotes *)
(***
  | '"'           { print_s "<tt>"; indoublequote lexbuf;
                    print_s "</tt>"; main lexbuf }
***)
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
  | "$"         { math_mode := 
		    begin
		      match !math_mode with
			| MathNone -> MathNoDisplay
			| MathNoDisplay -> MathNone
			| MathDisplay -> (* syntax error *) MathNone
		    end; 
		  main lexbuf }
  | "$$"        { math_mode := 
		    begin
		      match !math_mode with
			| MathNone -> 
			    print_s "<blockquote>";
			    MathDisplay
			| MathNoDisplay -> MathNoDisplay
			| MathDisplay -> 
			    print_s "\n</blockquote>";
			    MathNone
		    end;
                  main lexbuf }
(* \hkip *)
  | "\\hskip" space* dimension 
    (space* "plus" space* dimension)? (space* "minus" space* dimension)?
                { print_s " "; main lexbuf }
(* Special characters *)
  | "\\char" ['0'-'9']+
                { let lxm = Lexing.lexeme lexbuf in
                  let code = String.sub lxm 5 (String.length lxm - 5) in
                  print_c(Char.chr(int_of_string code));
                  main lexbuf }
  | "<"         { print_s "&lt;"; main lexbuf }
  | ">"         { print_s "&gt;"; main lexbuf }
  | "~"         { print_s "&nbsp;"; main lexbuf }
  | "``"        { print_s "&ldquo;"; main lexbuf }
  | "''"        { print_s "&rdquo;"; main lexbuf }
  | "--"        { print_s (if !html_entities then "&ndash;" else "-"); 
		  main lexbuf }
  | "---"       { print_s (if !html_entities then "&mdash;" else "-"); 
		  main lexbuf }
  | "^"         { if is_math_mode() then begin
		    let buf = Lexing.from_string (raw_arg lexbuf) in
		    print_s "<sup>";
		    save_state main buf;
		    print_s"</sup>"
		  end else
		    print_s "^"; 
		  main lexbuf }
  | "_"         { if is_math_mode() then begin
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
  | "\\" " "
      { print_s " "; main lexbuf }
(* General case for environments and commands *)
  | ("\\begin{" | "\\end{") ['A'-'Z' 'a'-'z' '@']+ "}" |
    "\\" (['A'-'Z' 'a'-'z' '@']+ '*'? " "? | [^ 'A'-'Z' 'a'-'z'])
                { let rec exec_action = function
                    | Print str -> print_s str
                    | Print_arg -> print_arg lexbuf
                    | Raw_arg f -> f (raw_arg lexbuf)
                    | Skip_arg -> save_nesting skip_arg lexbuf
		    | Recursive s -> main (Lexing.from_string s)
		    | Parameterized f ->
			List.iter exec_action (f (raw_arg lexbuf))
		  in
		  let m = chop_last_space (Lexing.lexeme lexbuf) in
                  List.iter exec_action (find_macro m);
                  main lexbuf }
(* Nesting of braces *)
  | '{'         { incr brace_nesting; main lexbuf }
  | '}'         { if !brace_nesting <= 0
                  then ()
                  else begin decr brace_nesting; main lexbuf end }
(* Default rule for other characters *)
  | eof         { () }
  | ['A'-'Z' 'a'-'z']+
                { if is_math_mode() then print_s "<em>";
                  print_s(Lexing.lexeme lexbuf);
                  if is_math_mode() then print_s "</em>";
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
  | "\\def" ('\\' ['a'-'z' 'A'-'Z' '@']+ as s) ("#" (['0'-'9']+ as n))?
      { let b = raw_arg lexbuf in
	def_macro s n b;
        read_macros lexbuf }
  | "\\newcommand" space* 
    "{" ("\\" ['a'-'z' 'A'-'Z']+ as s) "}" ("[" (['0'-'9']+ as n) "]")?
      { let b = raw_arg lexbuf in
	def_macro s n b;
        read_macros lexbuf }
  | "\\let" ('\\' ['a'-'z' 'A'-'Z' '@']+ as s) '='
      { let b = raw_arg lexbuf in
	def_macro s None b;
        read_macros lexbuf }
  | eof 
      { () }
  | _   
      { read_macros lexbuf }

