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

(* $Id: condition.ml,v 1.6 2000-06-30 02:36:41 filliatr Exp $ *)

open Printf;;

type constante =
  | Key
  | Field of string
  | Cte of string
;;
   
type condition =
  | True 
  | False 
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | Comp of constante * string * constante
  | Match of constante * Str.regexp
;;

exception Unavailable;;

let evaluate_constante key fields = function
    Key -> key
  | Field(f) ->
      begin
	try
	  match List.assoc f fields with
	    | [Bibtex.String(v)] -> 
		let v' = Latex_accents.normalize v in
		(*
		  Printf.eprintf "normalize(%s) -> %s\n" v v';
		*)
		v'
	    | [Bibtex.Id(v)] -> v
	    | _ -> raise Unavailable
	with
	    Not_found -> raise Unavailable
      end
  | Cte(x) -> Latex_accents.normalize x
;;

let eval_comp v1 op v2 = 
  match op with
      "=" -> v1 = v2
    | "<>" -> v2 <> v2
    | _ ->
	let n1 = int_of_string v1
	and n2 = int_of_string v2 in
	  match op with
	      ">" -> n1 > n2
	    | "<" -> n1 < n2
	    | ">=" -> n1 >= n2
	    | "<=" -> n1 <= n2
	    | _ -> assert false
;;

let rec evaluate_rec key fields = function
  | True -> true
  | False -> false
  | And(c1,c2) ->
      if evaluate_rec key fields c1
      then evaluate_rec key fields c2
      else false
  | Or(c1,c2) ->
      if evaluate_rec key fields c1
      then true
      else evaluate_rec key fields c2
  | Not(c) -> not (evaluate_rec key fields c)
  | Comp(c1,op,c2) ->
      let v1 = evaluate_constante key fields c1 
      and v2 = evaluate_constante key fields c2 in
	begin
	  try 
	    eval_comp v1 op v2
	  with
	      Failure "int_of_string" -> 
		if not !Options.quiet then begin
		  eprintf "Warning: cannot compare non-numeric values ";
		  eprintf "%s and %s in entry %s\n" v1 v2 key
		end;
		raise Unavailable
	end

  | Match(c,r) ->
      let v = evaluate_constante key fields c in
      try
	let _ = Str.search_forward r v 0 in true
      with Not_found -> false

;;

let evaluate_cond key fields c =
  try
    evaluate_rec key fields c
  with
      Unavailable -> false
    | Not_found -> assert false
;;
    
let string_of_constante = function
    Key -> "(key)"
  | Field(f) -> f
  | Cte(x) -> "\"" ^ x ^ "\""
;;

let rec print = function
  | True -> printf "true"
  | False -> printf "false"
  | And(c1,c2) -> printf "("; print c1; printf " and "; print c2; printf ")"
  | Or(c1,c2) -> printf "("; print c1; printf " or "; print c2; printf ")"
  | Not(c) -> printf "(not "; print c; printf ")"
  | Comp(c1,op,c2) -> 
      printf "%s %s %s" (string_of_constante c1) op (string_of_constante c2)
  | Match(c,s) -> printf "%s : (regexp)" (string_of_constante c)
;;

