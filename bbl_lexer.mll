(*
 * bbl_lexer.mll
 *)
{

exception End_of_biblio

let opt_ref = ref None

let key = ref ""

let brace_depth = ref 0

(* To buffer string literals *)
 
let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0
 
let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
 
let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index
 
let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

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
	reset_string_buffer();
	skip_end_of_line lexbuf;
	bibitem_body lexbuf }

and bibitem_body = parse
  | "\n\n"   (* une ligne vide marque la fin *)
      { (!opt_ref,!key,get_stored_string()) }
  | eof
      { raise End_of_file }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        bibitem_body lexbuf }

and skip_end_of_line = parse
    [' ' '\n' '\010' '\013' '\009' '\012'] +
      { () }
  | _ { store_string_char(Lexing.lexeme_char lexbuf 0) }
