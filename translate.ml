(**************************************************************************)
(*  bibtex2html - A BibTeX to HTML translator                             *)
(*  Copyright (C) 1997-2014 Jean-Christophe Filliâtre and Claude Marché   *)
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

(*s Production of the HTML documents from the BibTeX bibliographies. *)

open Printf

(*s Options. *)

let nodoc = ref false
let nokeys = ref false
let file_suffix = ref ".html"
let link_suffix = ref ".html"
let raw_url = ref false
let title = ref ""
let title_spec = ref false
let print_abstract = ref true
let print_keywords = ref true
let print_links = ref true
let print_header = ref true
let print_footer = ref true
let multiple = ref false
let single = ref false
let both = ref false
let user_header = ref ""
let user_footer = ref ""
let bib_entries = ref true
let links_in_bib_file = ref true
let input_file = ref ""
let output_file = ref ""
let bibentries_file = ref ""
let title_url = ref false
let use_label_name = ref false
let use_keys = ref false
let linebreak = ref false
type note_kind = NKlatex | NKhtml
let note_fields = ref ([] : (string * note_kind) list)
let abstract_name = ref "Abstract"
let doi = ref true
let doi_prefix = ref "https://doi.org/"
let eprint = ref true
let eprint_prefix = ref "http://arxiv.org/abs/"
let revkeys = ref false

type table_kind = Table | DL | NoTable
let table = ref Table

(* internal name, plus optional external name *)
type field_info = string * (string option)

let default_fields =
  List.map (fun x -> x, None)
    ["ftp"; "http"; "url"; "dvi"; "ps"; "postscript"; "pdf";
     "documenturl"; "urlps"; "urldvi"; "urlpdf"]

let (fields : field_info list ref) = ref default_fields

let add_field s =
  let u = String.lowercase_ascii s in
  Biboutput.add_link_field u;
  fields := (u, None) :: (List.remove_assoc u !fields)

let add_named_field s name =
  let u = String.lowercase_ascii s in
  Biboutput.add_link_field u;
  if u = "abstract" then abstract_name := name;
  if not !both || u <> "abstract" then
    fields := (u, Some name) :: (List.remove_assoc u !fields)

let add_note_field s =
  let u = String.lowercase_ascii s in
  note_fields := !note_fields @ [u, NKlatex]

let add_note_html_field s =
  let u = String.lowercase_ascii s in
  note_fields := !note_fields @ [u, NKhtml]

(* first pass to get the crossrefs *)

let (cite_tab : (string,string) Hashtbl.t) = Hashtbl.create 17

let cpt = ref 0

let first_pass bl =
  let rec pass = function
    | [] -> ()
    | (None,_,(_,k,_)) :: rem ->
	incr cpt;
	Hashtbl.add cite_tab k (string_of_int !cpt);
	pass rem
    | (Some c,_,(_,k,_)) :: rem ->
	Hashtbl.add cite_tab k c;
	pass rem
  in
  cpt := 0;
  Hashtbl.clear cite_tab;
  List.iter (fun (_,items) -> pass items) bl


(* latex2html : to print LaTeX strings in HTML format *)

let latex2html ch s =
  Latexmacros.out_channel := ch;
  Latexscan.brace_nesting := 0;
  Latexscan.main (Lexing.from_string s)

open Latexmacros

let in_summary = ref false

let cite k =
  try
    let url =
      if !in_summary then
	sprintf "#%s" k
      else
	sprintf "%s%s#%s" !output_file !link_suffix k
    in
    let c = if !use_keys then k else Hashtbl.find cite_tab k in
    print_s (sprintf "[<a href=\"%s\">" url);
    latex2html !out_channel c;
    print_s "</a>]"
  with
      Not_found -> print_s "[?]"

let _ = def "\\cite" [ Raw_arg cite ]

let safe_title e =
  try Expand.get_title e with Not_found -> "No title"


(* header and footer of HTML files *)

let own_address = "http://www.lri.fr/~filliatr/bibtex2html/"

let header ch =
  let print_arg s =
    if String.contains s ' ' then
      fprintf ch "\"%s\" " s
    else
      fprintf ch "%s " s
  in
  fprintf ch "
<!-- This document was automatically generated with bibtex2html %s
     (see http://www.lri.fr/~filliatr/bibtex2html/),
     with the following command:
     " Version.version;
  let argv = Sys.argv in
  let filename = Array.get argv 0 in
  Array.set argv 0 (Filename.basename filename);
  Array.iter print_arg Sys.argv;
  output_string ch " -->\n\n"

let footer ch =
  Html.open_balise ch "hr";
  Html.open_balise ch "p";
  Html.open_balise ch "em";
  output_string ch "This file was generated by\n";
  Html.open_href ch own_address;
  output_string ch "bibtex2html";
  Html.close_href ch;
  output_string ch " "; output_string ch Version.version; output_string ch ".";
  Html.close_balise ch "em";
  Html.close_balise ch "p";
  output_string ch "\n";
  output_string ch !user_footer

(* links (other than BibTeX entry, when available) *)

let compression_suffixes = [ ".gz"; ".Z"; ".zip" ]

let file_suffixes =
  List.flatten
    (List.map (fun s -> s :: List.map ((^) s) compression_suffixes)
       [ ".dvi"; ".DVI"; ".ps"; ".PS"; ".pdf"; ".PDF";
	 ".rtf"; ".RTF"; ".txt"; ".TXT"; ".html"; ".HTML" ])

let is_https s =
  String.length s > 4 && String.lowercase_ascii (String.sub s 0 5) = "https"

let is_http s =
  String.length s > 3 && String.lowercase_ascii (String.sub s 0 4) = "http"

let is_ftp s =
  String.length s > 2 && String.lowercase_ascii (String.sub s 0 3) = "ftp"

let is_www s =
  String.length s > 3 && String.lowercase_ascii (String.sub s 0 4) = "www:"

let is_url s = is_http s || is_ftp s || is_www s

let file_type f =
  try
    List.find (Filename.check_suffix f) file_suffixes
  with Not_found ->
    if is_https f then "https"
    else if is_http f then "http"
    else if is_ftp f then "ftp" else "www:"

let get_url s =
  if String.length s > 3 && String.lowercase_ascii (String.sub s 0 3) = "www"
  then
    String.sub s 4 (String.length s - 4)
  else
    s

let link_name (u, name) url s = match name with
  | Some name ->
      name
  | None ->
      if !raw_url then
	url
      else if !use_label_name then
	String.capitalize_ascii (String.lowercase_ascii u)
      else
	s

type link = { l_url : string; l_name : string }

let display_links ch links =
  let rec display = function
    | [] ->
	output_string ch "&nbsp;]\n"
    | l :: r ->
	Html.open_href ch l.l_url;
	output_string ch l.l_name;
	Html.close_href ch;
	if r <> [] then output_string ch "&nbsp;| \n";
	display r
  in
  if !print_links && links <> [] then begin
    output_string ch "[&nbsp;"; display links
  end

exception Caught

let rec map_succeed f = function
  | [] ->
      []
  | x :: l ->
      try let y = f x in y :: map_succeed f l with Caught -> map_succeed f l

let make_links ((t,k,_) as e) =
  (* URL's *)
  map_succeed
    (fun ((f, _) as info) ->
       try
	 let u = Expand.get_lowercase_field e f in
	 let s = file_type u in
	 let url = get_url u in
	 { l_url = url; l_name = link_name info url s }
       with Not_found -> raise Caught)
    !fields

type abstract =
  | Alink of link
  | Atext of string
  | No_abstract

let make_abstract ((t,k,_) as e) =
  try
    let a = Expand.get_lowercase_field e "abstract" in
    if is_url a then begin
      (* 1. it is an URL *)
      Alink { l_url = get_url a; l_name = !abstract_name }
    end else if !print_abstract then begin
      (* 2. we have to print it right here *)
      Atext a
    end else if !both then begin
      (* 3. we have to insert a link to the file f-abstracts *)
      let url = sprintf "%s_abstracts%s#%s" !output_file !link_suffix k in
      Alink { l_url = url; l_name = !abstract_name }
    end else
      No_abstract
  with Not_found ->
    No_abstract

let blockquote ch f =
(* JK  Html.paragraph ch; output_string ch "\n"; *)
  Html.open_balise ch "blockquote";
  let font_size = not !multiple && !Html.css = None in
  if font_size then Html.open_balise ch "font size=\"-1\"";
  output_string ch "\n";
  f (); output_string ch "\n";
  if font_size then Html.close_balise ch "font";
  Html.close_balise ch "blockquote";
  output_string ch "\n"

let display_abstract ch a = blockquote ch (fun () -> latex2html ch a)

let display_notes ch e =
  List.iter
    (fun (f, k) ->
       try
	 let a = Expand.get_lowercase_field e f in
	 match k with
	   | NKlatex -> display_abstract ch a (* JK Html.paragraph ch *)
	   | NKhtml -> output_string ch a
       with Not_found -> ())
    !note_fields

let display_keywords ch e =
  try
    let k = Expand.get_lowercase_field e "keywords" in
    blockquote ch (fun () -> output_string ch "Keywords: "; latex2html ch k)
  with Not_found ->
    ()

let doi_link e =
  if !doi then begin
    try
      let k = Expand.get_lowercase_field e "doi" in
      let url = if is_url k then k else !doi_prefix ^ k in
      [{ l_url = url; l_name = "DOI" }]
    with Not_found -> []
  end else
    []

let eprint_link e =
  if !eprint then begin
    try
      let k = Expand.get_lowercase_field e "eprint" in
      [{ l_url = !eprint_prefix ^ k; l_name = "arXiv" }]
    with Not_found -> []
  end else
    []

(* Printing of one entry *)

let bibtex_entry k =
  { l_url =
      sprintf "%s%s#%s" !bibentries_file !link_suffix k;
    l_name =
      "bib" }

let separate_file (b,((_,k,f) as e)) =
  in_summary := false;
  let file = k ^ !file_suffix in
  let ch = open_out file in
  if not !nodoc then begin
    let f = if !title_spec then !title else !output_file in
    let title = sprintf "%s : %s" f k in
    Html.open_document ch (fun () -> output_string ch title)
  end;
  if !print_header then header ch;
  Html.open_balise ch "h2";
  latex2html ch b;
  Html.close_balise ch "h2";
  if !print_header then output_string ch !user_header;
  (* JK Html.paragraph ch; *)
  let labs = match make_abstract e with
    | Atext a -> display_abstract ch a; []
    | Alink l -> [l]
    | No_abstract -> []
  in
  Html.paragraph ch;
  display_notes ch e;
  if !print_keywords then display_keywords ch e;
  display_links ch
    (labs
      @ (if !bib_entries then [bibtex_entry k] else [])
      @ doi_link e
      @ eprint_link e
      @ make_links e);
  (* JK Html.paragraph ch; *)
  Html.open_href ch (!output_file ^ !link_suffix);
  output_string ch "Back";
  Html.close_href ch;
  if !print_footer then footer ch;
  if not !nodoc then Html.close_document ch;
  close_out ch;
  in_summary := true

let open_table ch = match !table with
  | Table -> Html.open_balise ch "table"
  | DL -> Html.open_balise ch "dl"
  | NoTable -> ()

let close_table ch = match !table with
  | Table -> Html.close_balise ch "table"
  | DL -> Html.close_balise ch "dl"
  | NoTable -> ()

let open_row ch = match !table with
  | Table ->
      Html.open_balise ch "tr valign=\"top\""; output_string ch "\n";
      Html.open_balise ch "td align=\"right\" class=\"bibtexnumber\"";
      output_string ch "\n"
  | DL ->
      Html.open_balise ch "dt"; output_string ch "\n"
  | NoTable ->
      Html.open_balise ch "p"

let new_column ch = match !table with
  | Table ->
      Html.close_balise ch "td"; output_string ch "\n";
      Html.open_balise ch "td class=\"bibtexitem\""; output_string ch "\n"
  | DL ->
      Html.close_balise ch "dt"; output_string ch "\n";
      Html.open_balise ch "dd"; output_string ch "\n"
  | NoTable ->
      output_string ch "\n"

let close_row ch = match !table with
  | Table ->
      Html.close_balise ch "td"; output_string ch "\n";
      Html.close_balise ch "tr"; output_string ch "\n"
  | DL ->
      (* JK Html.paragraph ch; output_string ch "\n"; *)
      Html.close_balise ch "dd"; output_string ch "\n"
  | NoTable ->
      Html.close_balise ch "p"

let one_entry_summary ch biblio (_,b,((_,k,f) as e)) =
  if !Options.debug then begin
    eprintf "[%s]" k; flush stderr
  end;
  output_string ch "\n\n";
  open_row ch;
  (* JK changes *)
  if (not !nokeys) || !multiple then output_string ch "[";
  Html.open_anchor ch k;
  if (not !nokeys) || !multiple then begin
    if !multiple then Html.open_href ch (k ^ !link_suffix);
    latex2html ch (if !use_keys then k else Hashtbl.find cite_tab k);
    if !multiple then Html.close_href ch;
  end else
    if !table <> NoTable then output_string ch "&nbsp;";
  Html.close_anchor ch;
  if (not !nokeys) || !multiple then output_string ch "]";
  (* end of JK changes *)
  output_string ch "\n";
  new_column ch;
  latex2html ch b;
  if !linebreak then Html.open_balise ch "br /";
  output_string ch "\n";

  if !multiple then
    separate_file (b,e)
  else if !single then begin
    let ks = Bibtex.KeySet.singleton k in
    let ks = Bibfilter.saturate biblio ks in
    Biboutput.output_bib ~html:true ch biblio (Some ks);
  end else begin
    let links = doi_link e @ eprint_link e @ make_links e in
    let links = if !bib_entries then bibtex_entry k :: links else links in
    match make_abstract e with
      | Atext a ->
	  display_links ch links; display_abstract ch a (*; Html.paragraph ch*)
      | Alink l -> display_links ch (links @ [l])
      | No_abstract -> display_links ch links
  end;
  display_notes ch e;
  if !print_keywords then display_keywords ch e;
  output_string ch "\n";
  close_row ch

(* summary file f.html *)

let summary biblio bl =
  let (ch,filename) =
    if !output_file = "" then
      (stdout, "standard output")
    else
      let filename = !output_file ^ !file_suffix in
      (open_out filename, filename)
  in
  if not !Options.quiet then begin
    eprintf "Making HTML document (%s)..." filename; flush stderr
  end;
  if not !nodoc then
    Html.open_document ch (fun () -> output_string ch !title);
  if !print_header then header ch;
  if !title_spec then Html.h1_title ch !title;
  output_string ch "\n";
  if !print_header then output_string ch !user_header;

  in_summary := true;
  List.iter
    (fun (name,el) ->
       begin match name with
	 | None -> ()
	 | Some s ->
	     Html.open_balise ch "h2";
	     latex2html ch s;
	     Html.close_balise ch "h2";
	     output_string ch "\n"
       end;
       open_table ch;
       let el = if !revkeys then List.rev el else el in
       List.iter (one_entry_summary ch biblio) el;
       close_table ch)
    bl;
  in_summary := false;
  if !print_footer then footer ch;
  if not !nodoc then Html.close_document ch;
  close_out ch;
  if not !Options.quiet then begin eprintf "ok\n"; flush stderr end


(* HTML file with BibTeX entries f_bib.html *)

let print_list print sep l =
  let rec print_rec = function
    | [] -> ()
    | [x] -> print x
    | x::r -> print x; sep(); print_rec r
  in
  print_rec l


let bib_file bl keys =
  let fn = !bibentries_file ^ !file_suffix in
  if not !Options.quiet then begin
    eprintf "Making HTML list of BibTeX entries (%s)..." fn;
    flush stderr
  end;
  let ch = open_out fn in

  if not !nodoc then begin
    let t = if !title_spec then !title else !input_file in
    Html.open_document ch (fun _ -> output_string ch t)
  end;

  Html.open_balise ch "h1";
  output_string ch !input_file;
  Html.close_balise ch "h1";

  let output_bib =
    if !links_in_bib_file then
      let html_file = !output_file ^ !link_suffix in
      Biboutput.output_bib ~html:true ~html_file ch
    else
      Biboutput.output_bib ~html:true ch
  in
  output_bib bl keys;

  if !print_footer then footer ch;
  if not !nodoc then Html.close_document ch;
  flush ch;
  close_out ch;
  if not !Options.quiet then begin eprintf "ok\n"; flush stderr end


(* main function *)

let format_list biblio sorted_bl keys =
  first_pass sorted_bl;
  bibentries_file := !output_file ^ "_bib";
  if !both then begin
    let old_print_keywords = !print_keywords in
    (* short version *)
    print_abstract := false;
    print_keywords := false;
    summary biblio sorted_bl;
    (* long version with abstracts and keywords *)
    print_abstract := true;
    print_keywords := old_print_keywords;
    let old_output = !output_file in
    output_file := !output_file ^ "_abstracts";
    summary biblio sorted_bl;
    output_file := old_output
  end else
    summary biblio sorted_bl;
  (* BibTeX entries file *)
  if !bib_entries then bib_file biblio keys

