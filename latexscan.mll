{
  (* Code initially written by Xavier Leroy. *)

  open Latexmacros

  let brace_nesting = ref 0;;
  let math_mode = ref false;;
  let save_nesting f arg =
    let n = !brace_nesting in 
    brace_nesting := 0;
    f arg;
    brace_nesting := n;;
  let save_state f arg =
    let n = !brace_nesting and m = !math_mode in
    brace_nesting := 0;
    math_mode := false;
    f arg;
    brace_nesting := n;
    math_mode := m;;
  let verb_delim = ref (Char.chr 0);;
}

rule main = parse
(* Comments *)
    '%' [^ '\n'] * '\n' { main lexbuf }
(* Paragraphs *)
  | "\n\n" '\n' *
                { print_s "<P>\n"; main lexbuf }
(* Font changes *)
  | "{\\it" " "* | "{\\em" " "*
                  { print_s "<i>";
                    save_state main lexbuf;
                    print_s "</i>"; main lexbuf }
  | "{\\bf" " "*  { print_s "<b>";
                    save_state main lexbuf;
                    print_s "</b>"; main lexbuf }
  | "{\\sf" " "*  { print_s "<b>";
                    save_state main lexbuf;
                    print_s "</b>"; main lexbuf }
  | "{\\tt" " "*  { print_s "<tt>";
                    save_state main lexbuf;
                    print_s "</tt>"; main lexbuf }
  | '"'           { print_s "<tt>"; indoublequote lexbuf;
                    print_s "</tt>"; main lexbuf }
(* Verb, verbatim *)
  | "\\verb" _  { verb_delim := Lexing.lexeme_char lexbuf 5;
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
  | "<"         { print_s "&lt;"; main lexbuf }
  | ">"         { print_s "&gt;"; main lexbuf }
  | "~"         { print_s " "; main lexbuf }
(* General case for environments and commands *)
  | ("\\begin{" | "\\end{") ['A'-'Z' 'a'-'z']+ "}" |
    "\\" (['A'-'Z' 'a'-'z']+ '*'? | [^ 'A'-'Z' 'a'-'z'])
                { let exec_action = function
                      Print str -> print_s str
                    | Print_arg -> print_arg lexbuf
                    | Raw_arg f -> f(raw_arg lexbuf)
                    | Skip_arg -> save_nesting skip_arg lexbuf in
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
                { if !math_mode then print_s "<I>";
                  print_s(Lexing.lexeme lexbuf);
                  if !math_mode then print_s "</I>";
                  main lexbuf }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); main lexbuf }

and indoublequote = parse
    '"'         { () }
  | "<"         { print_s "&lt;"; indoublequote lexbuf }
  | ">"         { print_s "&gt;"; indoublequote lexbuf }
  | "&"         { print_s "&amp;"; indoublequote lexbuf }
  | "\\\""      { print_s "\""; indoublequote lexbuf }
  | "\\\\"      { print_s "\\"; indoublequote lexbuf }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); indoublequote lexbuf }

and inverb = parse
    "<"         { print_s "&lt;"; inverb lexbuf }
  | ">"         { print_s "&gt;"; inverb lexbuf }
  | "&"         { print_s "&amp;"; inverb lexbuf }
  | _           { let c = Lexing.lexeme_char lexbuf 0 in
                  if c == !verb_delim then ()
                                      else (print_c c; inverb lexbuf) }
and inverbatim = parse
    "<"         { print_s "&lt;"; inverbatim lexbuf }
  | ">"         { print_s "&gt;"; inverbatim lexbuf }
  | "&"         { print_s "&amp;"; inverbatim lexbuf }
  | "\\end{verbatim}" { () }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); inverbatim lexbuf }
  
and rawhtml = parse
    "\\end{rawhtml}" { () }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); rawhtml lexbuf }

and latexonly = parse
    "\\end{latexonly}" { () }
  | _           { latexonly lexbuf }

and print_arg = parse
    "{"         { save_nesting main lexbuf }
  | "["         { skip_optional_arg lexbuf; print_arg lexbuf }
  | " "         { print_arg lexbuf }
  | _           { print_c(Lexing.lexeme_char lexbuf 0); main lexbuf }

and skip_arg = parse
    "{"         { incr brace_nesting; skip_arg lexbuf }
  | "}"         { decr brace_nesting;
                  if !brace_nesting > 0 then skip_arg lexbuf }
  | "["         { if !brace_nesting = 0 then skip_optional_arg lexbuf;
                  skip_arg lexbuf }
  | " "         { skip_arg lexbuf }
  | _           { if !brace_nesting > 0 then skip_arg lexbuf }

and raw_arg = parse
    " "         { raw_arg lexbuf }
  | '{' [^ '}'] * '}'
                { let s = Lexing.lexeme lexbuf in
                  String.sub s 1 (String.length s - 2) }
  | "["         { skip_optional_arg lexbuf; raw_arg lexbuf }
  | _           { Lexing.lexeme lexbuf }

and skip_optional_arg = parse
    "]"         { () }
  | _           { skip_optional_arg lexbuf }

