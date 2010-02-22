(**************************************************************************)
(*  bibtex2html - A BibTeX to HTML translator                             *)
(*  Copyright (C) 1997-2010 Jean-Christophe Filliâtre and Claude Marché   *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU General Public                   *)
(*  License version 2, as published by the Free Software Foundation.      *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(*  See the GNU General Public License version 2 for more details         *)
(*  (enclosed in the file GPL).                                           *)
(**************************************************************************)

(*s Production of HTML syntax. *)

open Printf

let bgcolor = ref None
let css = ref None

let open_document ch ftitle =
  output_string ch
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n\n";
  output_string ch "<html>\n\n<head>\n";
  output_string ch "<title>"; ftitle(); output_string ch "</title>\n";
  begin match !css with
    | None -> ()
    | Some f -> 
	fprintf ch "<link rel=stylesheet type=\"text/css\" href=\"%s\">\n" f
  end;
  output_string ch "</head>\n\n";
  begin match !bgcolor with
    | None -> output_string ch "<body>\n"
    | Some color -> fprintf ch "<body bgcolor=%s>\n" color
  end;
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
  open_balise ch ("a name=\"" ^ s ^ "\"")
    
let close_anchor ch = 
  close_balise ch "a"


let absolute_url_regexp = Str.regexp "\\(.+://\\)\\|#\\|mailto:"

let is_absolute_url u =
  try Str.search_forward absolute_url_regexp u 0 = 0 with Not_found -> false
  
let is_relative_url u = not (is_absolute_url u)

let open_href ch s =
  open_balise ch ("a href=\"" ^ s ^ "\"")

let close_href ch =
  close_balise ch "a"

let open_h ch i =
  open_balise ch (sprintf "h%d" i)

let close_h ch i =
  close_balise ch (sprintf "h%d" i)

let open_em ch =
  open_balise ch "em"

let close_em ch =
  close_balise ch "em"

let open_b ch =
  open_balise ch "b"

let close_b ch =
  close_balise ch "b"

let paragraph ch =
  open_balise ch "p"

let h_title ch n title =
  let s = sprintf "h%d" n in
  open_balise ch s;
  output_string ch title;
  close_balise ch s

let h1_title ch s = h_title ch 1 s
let h2_title ch s = h_title ch 2 s


