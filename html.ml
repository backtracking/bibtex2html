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

(*i $Id: html.ml,v 1.14 2001-10-10 13:06:19 filliatr Exp $ i*)

(*s Production of HTML syntax. *)

let bgcolor =
  ref None

let open_document ch ftitle =
  output_string ch "<html>\n\n<head>\n";
  output_string ch "<title>"; ftitle(); output_string ch "</title>\n</head>\n\n";
  let body = match !bgcolor with
  | None ->
      "<body>\n"
  | Some color ->
      Printf.sprintf "<body bgcolor=%s>\n" color in
  output_string ch body;
  flush ch
  

let close_document ch =
  output_string ch "</body>\n</html>\n";
  flush ch


let open_balise ch s =
  output_string ch ("<" ^ s ^ ">");
  flush ch

let close_balise ch s =
  output_string ch ("</" ^ s ^ ">");
  flush ch


let open_anchor ch s =
  open_balise ch ("A NAME=\"" ^ s ^ "\"")
    
let close_anchor ch = 
  close_balise ch "A";
  output_string ch "\n"


let absolute_url_regexp = Str.regexp "\\(.+://\\)\\|#\\|mailto:"

let is_absolute_url u =
  try Str.search_forward absolute_url_regexp u 0 = 0 with Not_found -> false
  
let is_relative_url u = not (is_absolute_url u)

let normalize_url u = 
  if is_relative_url u && Filename.is_implicit u then
    "./" ^ u
  else
    u

let open_href ch s =
  open_balise ch ("A HREF=\"" ^ (normalize_url s) ^ "\"")

let close_href ch =
  close_balise ch "A"

let open_h ch i =
  open_balise ch (Printf.sprintf "H%d" i)

let close_h ch i =
  close_balise ch (Printf.sprintf "H%d" i)

let open_em ch =
  open_balise ch "EM"

let close_em ch =
  close_balise ch "EM"

let open_b ch =
  open_balise ch "b"

let close_b ch =
  close_balise ch "b"

let paragraph ch =
  open_balise ch "p"

let h_title ch n title =
  let s = Printf.sprintf "H%d" n in
  open_balise ch s;
  output_string ch title;
  close_balise ch s

let h1_title ch s = h_title ch 1 s
let h2_title ch s = h_title ch 2 s


