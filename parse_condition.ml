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

(* $Id: parse_condition.ml,v 1.3 2000-06-02 19:37:36 filliatr Exp $ *)

open Condition

(*
let condition s =
  try
    let n = String.index s ':' in 
    let field = String.uppercase (String.sub s 0 n)
    and filter = String.sub s (succ n) (String.length s - n - 1)
    in
      Printf.printf "champ = %s\n" field;
      Printf.printf "filter = %s\n" filter;
      Match (Field field,Str.regexp_case_fold filter)
  with 
      Not_found ->
	raise Syntax_error
;;
*)

let condition s =
  let b = Lexing.from_string s in
  Condition_parser.condition_start Condition_lexer.token b
