(*
 * bibtex_lex.mll
 *)
{
open Bibtex_parser

let serious = ref false    (* if we are inside a command or not *) 

let brace_depth = ref 0

let line = ref 0

let reset () = line := 0

(* To buffer string literals *)

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0
 
let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0
 
let store_string_char c =
  if c = '\n' then incr line;
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
rule token = parse
    [' ' '\t'] +
      { token lexbuf }
  | '\n' { incr line; token lexbuf }
  | '@' { serious := true ; token lexbuf }
  | '=' { if !serious then Tequal else token lexbuf }
  | '#' { if !serious then Tsharp else token lexbuf }
  | ',' { if !serious then Tcomma else token lexbuf }
  | '{' | '('
        { if !serious then begin
	    incr brace_depth ; 
            if !brace_depth = 1 then
	      Tlbrace 
	    else begin
	      reset_string_buffer();
	      brace lexbuf;
	      Tstring (get_stored_string())
	    end 
          end else
	    token lexbuf }
  | '}' | ')'
        { if !serious then begin
	    if !brace_depth > 0 then decr brace_depth ;
	    if !brace_depth = 0 then serious := false ;
	    Trbrace
	  end else
	    token lexbuf }
  | (['A'-'Z' 'a'-'z' '_' '\'' '0'-'9' ':' '-' '?' '.' '*'
      '\192'-'\214' '\216'-'\246' '\248'-'\255']) +
      { if !serious then
	  let s = Lexing.lexeme lexbuf in 
          match String.uppercase s with
              "STRING" -> Tabbrev
	    | "COMMENT" -> Tcomment
	    | "PREAMBLE" -> Tpreamble
	    | _ -> Tident s 
      	else
          token lexbuf }
  | "\""
      { if !serious then begin
	  reset_string_buffer();
          string lexbuf;
          Tstring (get_stored_string())
	end else
	  token lexbuf }
  | eof { EOF }
  | _   { token lexbuf }

and string = parse
    '"'
      { () }
  | "\\\""
      { store_string_char '\\';
        store_string_char '"';
	string lexbuf}
  | eof
      { failwith "unterminated string" }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }

and brace = parse
    '{'
      { incr brace_depth;
        store_string_char '{';
      	brace lexbuf
      }
  | '}'
      {  decr brace_depth;
	 if !brace_depth > 1 then begin
	   store_string_char '}';
	   brace lexbuf
	 end
      }
  | eof
      { failwith "unterminated string" }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        brace lexbuf }
