 
{

  let string_buf = Buffer.create 79
                     
  let add_string s = Buffer.add_string string_buf s

  let add lexbuf = Buffer.add_string string_buf (Lexing.lexeme lexbuf)

}

let space = [ '\t']

rule next_char = parse
  '\\'                          { control lexbuf }
| _                             { add lexbuf ; next_char lexbuf }
| eof                           { () }


(* called when we have seen  "\\"  *)

and control = parse
  '"'                { quote_char lexbuf }
| '\''               { right_accent lexbuf }
| '`'                { left_accent lexbuf }
| '^'                { hat lexbuf }
| "c{c}"             { add_string "ç" ; next_char lexbuf }
| ("~n"|"~{n}")      { add_string "ñ"; next_char lexbuf  }
|  _                 { add_string "\\" ; add lexbuf ; next_char lexbuf  }
| eof                { add_string "\\" }

(* called when we have seen  "\\\""  *)
and quote_char = parse
  ('a'|"{a}")                   { add_string "ä" ; next_char lexbuf }
| ('o'|"{o}")                   { add_string "ö" ; next_char lexbuf }
| ('u'|"{u}")                   { add_string "ü" ; next_char lexbuf }
| ('e'|"{e}")                   { add_string "ë" ; next_char lexbuf }
| ('A'|"{A}")                   { add_string "Ä" ; next_char lexbuf }
| ('O'|"{O}")                   { add_string "Ö" ; next_char lexbuf }
| ('U'|"{U}")                   { add_string "Ü" ; next_char lexbuf }
| ('E'|"{E}")                   { add_string "Ë" ; next_char lexbuf }
| ("\\i" space+|"{\\i}")        { add_string "ï" ; next_char lexbuf }
| _                             { add_string "\\\"" ; add lexbuf }
| eof                           { add_string "\\\"" }

(* called when we have seen  "\\'"  *)
and right_accent = parse
| ('a'|"{a}")   { add_string "á" ; next_char lexbuf }
| ('o'|"{o}")   { add_string "ó" ; next_char lexbuf }
| ('u'|"{u}")   { add_string "ú" ; next_char lexbuf }
| ('e'|"{e}")   { add_string "é" ; next_char lexbuf }
| ('A'|"{A}")   { add_string "Á" ; next_char lexbuf }
| ('O'|"{O}")   { add_string "Ó" ; next_char lexbuf }
| ('U'|"{U}")   { add_string "Ú" ; next_char lexbuf }
| ('E'|"{E}")   { add_string "É" ; next_char lexbuf }
| ('i'|"\\i" space+|"{\\i}") { add_string "í" ; next_char lexbuf }
| ('I'|"\\I" space+|"{\\I}") { add_string "Í" ; next_char lexbuf }
| _             { add_string "\\'" ; add lexbuf ; next_char lexbuf }
| eof           { add_string "\\'" }

(* called when we have seen "\\`"  *)
and left_accent = parse
  ('a'|"{a}")   { add_string "à" ; next_char lexbuf }
| ('o'|"{o}")   { add_string "ò" ; next_char lexbuf }
| ('u'|"{u}")   { add_string "ù" ; next_char lexbuf }
| ('e'|"{e}")   { add_string "é" ; next_char lexbuf }
| ('A'|"{A}")   { add_string "À" ; next_char lexbuf }
| ('O'|"{O}")   { add_string "Ò" ; next_char lexbuf }
| ('U'|"{U}")   { add_string "Ù" ; next_char lexbuf }
| ('E'|"{E}")   { add_string "È" ; next_char lexbuf }
| ('i'|"\\i" space+ |"{\\i}") { add_string "ì" ; next_char lexbuf }
| ('I'|"\\I" space+ |"{\\I}") { add_string "Ì" ; next_char lexbuf }
| _             { add_string "\\`" ; add lexbuf ; next_char lexbuf }
| eof           { add_string "\\`" }

and hat = parse
  ('a'|"{a}")   { add_string "â" ; next_char lexbuf }
| ('o'|"{o}")   { add_string "ô" ; next_char lexbuf }
| ('u'|"{u}")   { add_string "û" ; next_char lexbuf }
| ('e'|"{e}")   { add_string "ê" ; next_char lexbuf }
| ('A'|"{A}")   { add_string "Â" ; next_char lexbuf }
| ('O'|"{O}")   { add_string "Ô" ; next_char lexbuf }
| ('U'|"{U}")   { add_string "Û" ; next_char lexbuf }
| ('E'|"{E}")   { add_string "Ê" ; next_char lexbuf }
| ('i'|"\\i" space+ |"{\\i}") { add_string "î" ; next_char lexbuf }
| ('I'|"\\I" space+ |"{\\I}") { add_string "Î" ; next_char lexbuf }
| _             { add_string "\\^" ; add lexbuf ; next_char lexbuf }
|  eof          { add_string "\\^" }

{

let normalize s = 
  Buffer.clear string_buf;
  next_char (Lexing.from_string s);
  Buffer.contents string_buf
;;

}
