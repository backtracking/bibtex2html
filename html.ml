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

(* $Id: html.ml,v 1.10 2000-06-02 19:37:34 filliatr Exp $ *)

let open_document ch ftitle =
  output_string ch "<html>\n\n<head>\n";
  output_string ch "<title>"; ftitle(); output_string ch "</title>\n";
  output_string ch "</head>\n\n<body>\n";
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

let open_href ch s =
  open_balise ch ("A HREF=\"" ^ s ^ "\"")

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


