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

(*i $Id: latex_accents.mll,v 1.9 2006-02-22 10:05:44 filliatr Exp $ i*)

(* Normalize both ISO-latin characters and LaTeX accents to HTML entities *)

{

  let string_buf = Buffer.create 79
                     
  let add_string s = Buffer.add_string string_buf s

  let add lexbuf = Buffer.add_string string_buf (Lexing.lexeme lexbuf)

  let produce_regexp = ref false

}

let space = [ '\t']

rule next_char = parse
    '\\'                          { control lexbuf }
  | '{'                           { next_char lexbuf }
  | '}'                           { next_char lexbuf }
  | 'ç' { add_string "&ccedil;" ; next_char lexbuf }
  | 'ñ' { add_string "&ntilde;"; next_char lexbuf }
  | 'ä' { add_string "&auml;"; next_char lexbuf }
  | 'ö' { add_string "&ouml;"; next_char lexbuf }
  | 'ü' { add_string "&uuml;"; next_char lexbuf }
  | 'ë' { add_string "&euml;"; next_char lexbuf }
  | 'Ä' { add_string "&Auml;"; next_char lexbuf }
  | 'Ö' { add_string "&Ouml;"; next_char lexbuf }
  | 'Ü' { add_string "&Uuml;"; next_char lexbuf }
  | 'Ë' { add_string "&Euml;"; next_char lexbuf }
  | 'ï' { add_string "&iuml;"; next_char lexbuf }
  | 'Ï' { add_string "&Iuml;"; next_char lexbuf }
  | 'á' { add_string "&aacute;"; next_char lexbuf }
  | 'ó' { add_string "&oacute;"; next_char lexbuf }
  | 'ú' { add_string "&uacute;"; next_char lexbuf }
  | 'é' { add_string "&eacute;"; next_char lexbuf }
  | 'Á' { add_string "&Aacute;"; next_char lexbuf }
  | 'Ó' { add_string "&Oacute;"; next_char lexbuf }
  | 'Ú' { add_string "&Uacute;"; next_char lexbuf }
  | 'É' { add_string "&Eacute;"; next_char lexbuf }
  | 'í' { add_string "&iacute;"; next_char lexbuf }
  | 'Í' { add_string "&Iacute;"; next_char lexbuf }
  | 'à' { add_string "&agrave;"; next_char lexbuf }
  | 'ò' { add_string "&ograve;"; next_char lexbuf }
  | 'ù' { add_string "&ugrave;"; next_char lexbuf }
  | 'è' { add_string "&egrave;"; next_char lexbuf }
  | 'À' { add_string "&Agrave;"; next_char lexbuf }
  | 'Ò' { add_string "&Ograve;"; next_char lexbuf }
  | 'Ù' { add_string "&Ugrave;"; next_char lexbuf }
  | 'È' { add_string "&Egrave;"; next_char lexbuf }
  | 'ì' { add_string "&igrave;"; next_char lexbuf }
  | 'Ì' { add_string "&Igrave;"; next_char lexbuf }
  | 'â' { add_string "&acirc;"; next_char lexbuf }
  | 'ô' { add_string "&ocirc;"; next_char lexbuf }
  | 'û' { add_string "&ucirc;"; next_char lexbuf }
  | 'ê' { add_string "&ecirc;"; next_char lexbuf }
  | 'î' { add_string "&icirc;"; next_char lexbuf }
  | 'Â' { add_string "&Acirc;"; next_char lexbuf }
  | 'Ô' { add_string "&Ocirc;"; next_char lexbuf }
  | 'Û' { add_string "&Ucirc;"; next_char lexbuf }
  | 'Ê' { add_string "&Ecirc;"; next_char lexbuf }
  | 'Î' { add_string "&Icirc;"; next_char lexbuf }
  | _                             { add lexbuf ; next_char lexbuf }
  | eof                           { () }


(* called when we have seen  "\\"  *)

and control = parse
  '"'                { quote_char lexbuf }
| '\''               { right_accent lexbuf }
| '`'                { left_accent lexbuf }
| '^'                { hat lexbuf }
| "c{c}"             { add_string "&ccedil;" ; next_char lexbuf }
| 'v'                { czech lexbuf }
| ("~n"|"~{n}")      { add_string "&ntilde;"; next_char lexbuf  }
|  _                 { add_string "\\" ; add lexbuf ; next_char lexbuf  }
| eof                { add_string "\\" }

(* called when we have seen  "\\\""  *)
and quote_char = parse
  ('a'|"{a}")                   { add_string "&auml;" ; next_char lexbuf }
| ('o'|"{o}")                   { add_string "&ouml;" ; next_char lexbuf }
| ('u'|"{u}")                   { add_string "&uuml;" ; next_char lexbuf }
| ('e'|"{e}")                   { add_string "&euml;" ; next_char lexbuf }
| ('A'|"{A}")                   { add_string "&Auml;" ; next_char lexbuf }
| ('O'|"{O}")                   { add_string "&Ouml;" ; next_char lexbuf }
| ('U'|"{U}")                   { add_string "&Uuml;" ; next_char lexbuf }
| ('E'|"{E}")                   { add_string "&Euml;" ; next_char lexbuf }
| ("\\i" space+|"{\\i}")        { add_string "&iuml;" ; next_char lexbuf }
| ('I'|"\\I" space+|"{\\I}")    { add_string "&Iuml;" ; next_char lexbuf }
| _                             { add_string "\\\"" ; add lexbuf }
| eof                           { add_string "\\\"" }

(* called when we have seen  "\\'"  *)
and right_accent = parse
| ('a'|"{a}")   { add_string "&aacute;" ; next_char lexbuf }
| ('o'|"{o}")   { add_string "&oacute;" ; next_char lexbuf }
| ('u'|"{u}")   { add_string "&uacute;" ; next_char lexbuf }
| ('e'|"{e}")   { add_string "&eacute;" ; next_char lexbuf }
| ('A'|"{A}")   { add_string "&Aacute;" ; next_char lexbuf }
| ('O'|"{O}")   { add_string "&Oacute;" ; next_char lexbuf }
| ('U'|"{U}")   { add_string "&Uacute;" ; next_char lexbuf }
| ('E'|"{E}")   { add_string "&Eacute;" ; next_char lexbuf }
| ('\'')   { add_string "&rdquo;" ; next_char lexbuf }
| ('i'|"\\i" space+|"{\\i}") { add_string "&iacute;" ; next_char lexbuf }
| ('I'|"\\I" space+|"{\\I}") { add_string "&Iacute;" ; next_char lexbuf }
| _             { add_string "\\'" ; add lexbuf ; next_char lexbuf }
| eof           { add_string "\\'" }

(* called when we have seen "\\`"  *)
and left_accent = parse
  ('a'|"{a}")   { add_string "&agrave;" ; next_char lexbuf }
| ('o'|"{o}")   { add_string "&ograve;" ; next_char lexbuf }
| ('u'|"{u}")   { add_string "&ugrave;" ; next_char lexbuf }
| ('e'|"{e}")   { add_string "&egrave;" ; next_char lexbuf }
| ('A'|"{A}")   { add_string "&Agrave;" ; next_char lexbuf }
| ('O'|"{O}")   { add_string "&Ograve;" ; next_char lexbuf }
| ('U'|"{U}")   { add_string "&Ugrave;" ; next_char lexbuf }
| ('E'|"{E}")   { add_string "&Egrave;" ; next_char lexbuf }
| ('`')   { add_string "&ldquo;" ; next_char lexbuf }
| ('i'|"\\i" space+ |"{\\i}") { add_string "&igrave;" ; next_char lexbuf }
| ('I'|"\\I" space+ |"{\\I}") { add_string "&Igrave;" ; next_char lexbuf }
| _             { add_string "\\`" ; add lexbuf ; next_char lexbuf }
| eof           { add_string "\\`" }

and hat = parse
  ('a'|"{a}")   { add_string "&acirc;" ; next_char lexbuf }
| ('o'|"{o}")   { add_string "&ocirc;" ; next_char lexbuf }
| ('u'|"{u}")   { add_string "&ucirc;" ; next_char lexbuf }
| ('e'|"{e}")   { add_string "&ecirc;" ; next_char lexbuf }
| ('A'|"{A}")   { add_string "&Acirc;" ; next_char lexbuf }
| ('O'|"{O}")   { add_string "&Ocirc;" ; next_char lexbuf }
| ('U'|"{U}")   { add_string "&Ucirc;" ; next_char lexbuf }
| ('E'|"{E}")   { add_string "&Ecirc;" ; next_char lexbuf }
| ('i'|"\\i" space+ |"{\\i}") { add_string "&icirc;" ; next_char lexbuf }
| ('I'|"\\I" space+ |"{\\I}") { add_string "&Icirc;" ; next_char lexbuf }
| _             { add_string "\\^" ; add lexbuf ; next_char lexbuf }
|  eof          { add_string "\\^" }

and czech = parse
  ('r'|"{r}")   { add_string "&#X0159;" ; next_char lexbuf }
| ('R'|"{R}")   { add_string "&#X0158;" ; next_char lexbuf }
| ('s'|"{s}")   { add_string "&#X0161;" ; next_char lexbuf }
| ('S'|"{S}")   { add_string "&#X0160;" ; next_char lexbuf }
| ('i'|"\\i" space+ |"{\\i}") { add_string "&#X012D;" ; next_char lexbuf }
| ('I'|"\\I" space+ |"{\\I}") { add_string "&#X012C;" ; next_char lexbuf }
| _             { add_string "\\^" ; add lexbuf ; next_char lexbuf }
|  eof          { add_string "\\^" }

{

let normalize to_regexp s = 
  Buffer.clear string_buf;
  produce_regexp := to_regexp;
  next_char (Lexing.from_string s);
  Buffer.contents string_buf
;;

}
