(*
 * bbl_lexer.mll
 *)
{

exception End_of_biblio

let opt_ref = ref None

let key = ref ""

let brace_depth = ref 0

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
      { let l = Lexing.lexeme lexbuf in
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
      { let l = Lexing.lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
        opt_ref := Some s }
    
and bibitem2 = parse
    '{' [^'}']* '}'
      { let l = Lexing.lexeme lexbuf in
	let s = String.sub l 1 (String.length l - 2) in
        key := s;
	skip_end_of_line lexbuf;
	bibitem_body lexbuf }

and bibitem_body = parse
  | ( [^'\n']+ '\n' )+ '\n'
      { let s = Lexing.lexeme lexbuf in
        (!opt_ref, !key, String.sub s 0 (String.length s - 2)) }
  | eof
      { raise End_of_file }

and skip_end_of_line = parse
    [' ' '\n' '\010' '\013' '\009' '\012'] +
      { () }
  | _ { skip_end_of_line lexbuf }
