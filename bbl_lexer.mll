(*
 * bbl_lexer.mll
 *)
{

open Lexing

exception End_of_biblio

let opt_ref = ref None

let key = ref ""

let brace_depth = ref 0

let buf = Buffer.create 1024

}
rule biblio_header = parse
    "\\begin{thebibliography}" '{' [^ '}']* '}'
      { biblio_name lexbuf }
  | eof
      { raise End_of_file }
  | _ 
      { biblio_header lexbuf }

and biblio_name = parse
    '[' [^ ']']* ']'
      { let l = lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
        Some s }
  | _
      { None } 

and bibitem = parse
    "\\end{thebibliography}"
      { raise End_of_biblio }
  | '\\' ['a'-'z']* "bibitem"
      { brace_depth := 0;
	begin try bibitem1 lexbuf 
	      with Failure "lexing: empty token" -> opt_ref := None end;
        bibitem2 lexbuf }
  | _ { bibitem lexbuf }

and bibitem1 = parse
    '[' [^']']* ']'
      { let l = lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
        opt_ref := Some s }
    
and bibitem2 = parse
    '{' [^'}']* '}'
      { let l = lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
        key := s;
	skip_end_of_line lexbuf;
	Buffer.reset buf;
	bibitem_body lexbuf }

and bibitem_body = parse
  | "\n\n"
      { let s = Buffer.contents buf in (!opt_ref, !key, s) }
  | eof
      { raise End_of_file }
  | "\\%" { Buffer.add_string buf "\\%"; bibitem_body lexbuf }
  | "%\n" { bibitem_body lexbuf }
  | _     { Buffer.add_char buf (lexeme_char lexbuf 0); bibitem_body lexbuf }

and skip_end_of_line = parse
    [' ' '\n' '\010' '\013' '\009' '\012'] +
      { () }
  | _ { skip_end_of_line lexbuf }
