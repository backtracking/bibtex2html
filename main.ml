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

(*s Main module of bibtex2html. *)

open Printf
open Translate

(* Options. *)

let excluded = ref ([] : string list)
let add_exclude k = excluded := k :: !excluded
let style = ref "plain"
let command = ref "bibtex -min-crossrefs=1000"

type sort = Unsorted | By_date | By_author
let sort = ref Unsorted
let reverse_sort = ref false

let ignore_bibtex_errors = ref false

let expand_abbrev_in_bib_output = ref true

(* Optional citation file. *)

let use_cite_file = ref false
let citations = ref ([] : string list)

let add_citations file =
  try
    let chan = open_in file
    and buf = Buffer.create 1024 in
    try
      while true do Buffer.add_char buf (input_char chan) done
    with End_of_file ->
      close_in chan;
      citations :=
        (Str.split (Str.regexp "[ \t\n]+") (Buffer.contents buf)) @
        !citations
  with Sys_error msg ->
    prerr_endline ("Cannot open citation file (" ^ msg ^ ")");
    exit 1
  
(*s Sorting the entries. *)

module KeyMap = Map.Make(struct type t = string let compare = compare end)

let keep_combine combine l1 l2 =
  let map = 
    List.fold_left (fun m ((_,k,_) as e) -> KeyMap.add k e m) KeyMap.empty l2 
  in
  let rec keep_rec = function
    | [] ->
	[]
    | ((_,k,_) as x)::rem ->
	if not (List.mem k !excluded) then
	  try
	    let y = KeyMap.find k map in (combine x y) :: (keep_rec rem)
	  with Not_found -> keep_rec rem
	else
	  keep_rec rem
  in
  keep_rec l1

let combine_f (c,_,b) e = c,b,e

let rev_combine_f x y = combine_f y x

let sort_entries entries bibitems =
  if not !Options.quiet then begin eprintf "Sorting..."; flush stderr end;
  let el =
    if !sort = By_author then 
      keep_combine combine_f bibitems entries
    else
      keep_combine rev_combine_f entries bibitems
  in
  let sl = 
    if !sort = By_date then
      Sort.list (fun (_,_,e1) (_,_,e2) -> Expand.date_order entries e1 e2) el
    else
      el 
  in
  if not !Options.quiet then begin eprintf "ok.\n"; flush stderr end;
  if !reverse_sort then List.rev sl else sl


(* We use BibTeX itself to format the entries. Operations:
   \begin{enumerate}
   \item create an auxiliary file tmp.aux
   \item call bibtex on it
   \item read the resulting tmp.bbl file to get the formatted entries
   \end{enumerate} *)

let create_aux_file fbib tmp =
  let ch = open_out (tmp ^ ".aux") in
  output_string ch "\\relax\n\\bibstyle{";
  output_string ch !style;
  output_string ch "}\n";
  if !use_cite_file then
    List.iter 
      (fun k -> output_string ch ("\\citation{" ^ k ^ "}\n"))
      !citations
  else
    output_string ch "\\citation{*}\n";
  output_string ch "\\bibdata{";
  output_string ch (Filename.chop_suffix fbib ".bib");
  output_string ch "}\n";
  close_out ch

let rm f = try Sys.remove f with _ -> ()

let clean tmp =
  if not !Options.debug then begin
    rm (tmp ^ ".aux");
    rm (tmp ^ ".blg");
    rm (tmp ^ ".bbl");
    rm tmp
  end

let call_bibtex tmp =
  if not !Options.quiet then begin 
    eprintf "calling BibTeX..."; flush stderr 
  end;
  match 
    let redir = 
      if !output_file = "" || !Options.quiet then 
	match Sys.os_type with 
	  | "Win32" -> "> nul 2>&1"
	  | _ -> "> /dev/null 2>&1" 
      else 
	"" 
    in
    let cmd = sprintf "%s %s %s" !command tmp redir in
    if !Options.debug then begin 
      eprintf "\nbibtex command: %s\n" cmd; flush stderr
    end;
    Sys.command cmd
  with
    | 0 -> 
	if not !Options.quiet then begin eprintf "\n"; flush stderr end
    | n ->
	if !ignore_bibtex_errors then begin
	  if not !Options.quiet then begin
	    eprintf "error %d (ignored)\n" n;
	    flush stderr
	  end
      	end else begin
	  eprintf "error %d while running bibtex\n" n;
	  exit n
	end

let read_one_biblio lb =
  let rec read_items acc lb =
    try
      let (_,k,_) as item = Bbl_lexer.bibitem lb in
      if !Options.debug then begin eprintf "[%s]" k; flush stderr end;
      read_items (item::acc) lb
    with Bbl_lexer.End_of_biblio -> 
      List.rev acc 
  in
  let name = Bbl_lexer.biblio_header lb in
  let items = read_items [] lb in
  (name,items)

let read_biblios lb =
  let rec read acc lb =
    try
      let b = read_one_biblio lb in
	read (b::acc) lb
    with
	End_of_file -> List.rev acc
  in
    read [] lb

let read_bbl tmp =
  let fbbl = tmp ^ ".bbl" in
  if not !Options.quiet then begin 
    eprintf "Reading %s..." fbbl; flush stderr 
  end;
  let ch = open_in fbbl in
  let lexbuf = Lexing.from_channel ch in
  let biblios = read_biblios lexbuf in
  close_in ch;
  clean tmp;
  if not !Options.quiet then begin
    eprintf "ok ";
    List.iter 
      (fun (_,items) -> eprintf "(%d entries)" (List.length items))
      biblios;
    eprintf "\n"; flush stderr
  end;
  biblios

let get_biblios fbib =
  let tmp = Filename.temp_file "bib2html" "" in
  try
    create_aux_file fbib tmp;
    call_bibtex tmp;
    read_bbl tmp
  with
    e -> clean tmp; raise e

(*i
let insert_title_url bib = 
  let rec remove_assoc x = function
    | [] ->
	raise Not_found
    | ((y,v) as p) :: l -> 
	if x = y then 
	  (v,l) 
	else 
	  let (v',l') = remove_assoc x l in (v', p :: l')
  in
  let url_value = function
    | [Bibtex.Id u] -> u
    | [Bibtex.String u] -> u
    | _ -> raise Not_found
  in
  let modify_entry f =
    try
      let t,f' = remove_assoc "title" f in
      let u,f'' = remove_assoc "url" f' in
      let u' = Html.normalize_url (url_value u) in 
      let nt = 
	(Bibtex.String 
	   (sprintf "\\begin{rawhtml}<A HREF=\"%s\">\\end{rawhtml}" u'))
	:: t @ [Bibtex.String "\\begin{rawhtml}</A>\\end{rawhtml}"]
      in
      ("TITLE",nt) :: f''
    with Not_found -> 
      f
  in
  Bibtex.fold 
    (fun com bib' -> match com with 
       | Bibtex.Entry (ty,k,f) -> 
	   Bibtex.add_new_entry (Bibtex.Entry (ty,k,modify_entry f)) bib'
       | _ -> 
	   Bibtex.add_new_entry com bib')
    bib Bibtex.empty_biblio
i*)

let parse_only = ref false
let print_keys = ref false

let translate fullname =
  let input_bib = Readbib.read_entries_from_file fullname in
  if !parse_only then exit 0;
  let entries = List.rev (Expand.expand input_bib) in
  let biblios = 
    if fullname = "" then begin
      let tmp = Filename.temp_file "bibtex2htmlinput" ".bib" in
      let ch = open_out tmp in
      Biboutput.output_bib ~html:false ch input_bib None;
      close_out ch;
      let bbl = get_biblios tmp in
      Sys.remove tmp;
      bbl
    end else
      get_biblios fullname 
  in
  let sb =
    List.map 
      (fun (name,bibitems) -> (name,sort_entries entries bibitems))
      biblios 
  in
  if !print_keys then begin
    List.iter
      (fun (_,bibitems) -> 
	 List.iter (fun (_,_,(_,k,_)) -> printf "%s\n" k) bibitems)
      sb;
    flush stdout;
    exit 0
  end;
  format_list 
    (if !expand_abbrev_in_bib_output then 
       Bibtex.expand_abbrevs input_bib 
     else input_bib) 
    sb 
    (if !use_cite_file then
       let keys = 
	 List.fold_right 
	   (fun s e -> Bibtex.KeySet.add s e) !citations Bibtex.KeySet.empty in
       let keys =
	 List.fold_right 
	   (fun s e -> Bibtex.KeySet.remove s e) !excluded keys in
       Some (Bibfilter.saturate input_bib keys)
     else None)


(*s Reading macros in a file. *)

let read_macros f =
  let chan = open_in f in
  let lb = Lexing.from_channel chan in
    Latexscan.read_macros lb;
    close_in chan


(*s Command line parsing. *)

let usage ?(error=true) () =
  if error then prerr_endline "bibtex2html: bad command line syntax";
  (if error then prerr_endline else print_endline) "
Usage: bibtex2html <options> [filename]
  -s style   BibTeX style (plain, alpha, ...)
  -c command BibTeX command (otherwise bibtex is searched in your path)
  -d         sort by date
  -a         sort as BibTeX (usually by author)
  -u         unsorted i.e. same order as in .bib file (default)
  -r         reverse the sort
  -revkeys   entries numbered in reverse order
  -t title   title of the HTML file (default is the filename)
  -bg color  background color of the HTML file (default is none)
  -css file  specify a style sheet file
  -o file    redirect the output
  -header    additional header in the HTML file
  -footer    additional footer in the HTML file
  -i         ignore BibTeX errors
  -both      produce versions with and without abstracts
  -multiple  produce one file per entry
  -single    produce a single page (with BibTeX input and output)
  -nodoc     only produces the body of the HTML documents
  -nokeys    do not print the BibTeX keys
  -nolinks   do not print any web link
  -nobiblinks
             do not add web links in the BibTeX output
  -rawurl    print URL instead of file type
  -heveaurl  use HeVeA's \\url macro
  -noabstract
             do not print the abstracts (if any)
  -nokeywords
             do not print the keywords (if any)
  -nodoi     do not insert the DOI links
  -doi-prefix url
             set the DOI links prefix (default is http://dx.doi.org/)
  -noeprint  do not insert the eprint links
  -eprint-prefix url
             set the eprint links prefix (default is http://arxiv.org/abs/)
  -linebreak add a linebreak between an entry and its links
  -use-table enforce the use of HTML tables (to be used after -nokeys)
  -noheader  do not print the header (bibtex2html command)
  -nofooter  do not print the footer (bibtex2html web link)
  -noexpand  do not expand abbreviations in the BibTeX output
  -nobibsource
             do not produce the BibTeX entries file
  -fsuffix   give an alternate suffix for HTML files
  -lsuffix   give an alternate suffix for HTML links
  -suffix s  give an alternate suffix for HTML files and links
  -citefile f
             read keys to include from file f
  -e key     exclude an entry
  -m file    read (La)TeX macros in file
  -f field   add a web link for that BibTeX field
  -nf field name
             add a web link for that BibTeX field, with the supplied name
  -note field
             declare a note field
  -dl        use DL lists instead of TABLEs
  -unicode   use Unicode characters for some LaTeX macros (as HTML entities) 
  -html-entities
             use HTML entities for some LaTeX macros
  -labelname use the label name when inserting a link
  --print-keys
             print the sorted bibtex keys and exit
  -debug     verbose mode (to find incorrect BibTeX entries)
  -q         quiet mode
  -w         stop on warning
  -v         print version and exit

On-line documentation at http://www.lri.fr/~filliatr/bibtex2html/
";
  exit (if error then 1 else 0)

let parse () =
  let rec parse_rec = function

    (* General aspect of the web page *)
    | ("-t" | "-title" | "--title") :: s :: rem ->
	title := s; title_spec := true; parse_rec rem
    | ("-t" | "-title" | "--title") :: [] ->
	usage()
    | ("-bg" | "-background" | "--background") :: s :: rem ->
	Html.bgcolor := Some s; parse_rec rem
    | ("-bg" | "-background" | "--background") :: [] ->
	usage()
    | ("-css" | "-style-sheet" | "--style-sheet") :: f :: rem ->
	Html.css := Some f; parse_rec rem
    | ("-css" | "-style-sheet" | "--style-sheet") :: [] ->
	usage()
    | ("-header" | "--header") :: s :: rem ->
	user_header := s; parse_rec rem
    | ("-header" | "--header") :: [] ->
	usage()
    | ("-footer" | "--footer") :: s :: rem ->
	user_footer := s; parse_rec rem
    | ("-footer" | "--footer") :: [] ->
	usage()
    | ("-s" | "-style" | "--style") :: s :: rem ->
	style := s; parse_rec rem
    | ("-s" | "-style" | "--style") :: [] ->
	usage()
    | ("-noabstract" | "-no-abstract" | "--no-abstract") :: rem ->
	print_abstract := false; parse_rec rem
    | ("-nodoi" | "-no-doi" | "--no-doi") :: rem ->
	doi := false; parse_rec rem
    | ("-doi-prefix" | "--doi-prefix") :: s :: rem ->
	doi_prefix := s; parse_rec rem
    | ("-doi-prefix" | "--doi-prefix") :: [] ->
	usage ()
    | ("-noeprint" | "-no-eprint" | "--no-eprint") :: rem ->
	eprint := false; parse_rec rem
    | ("-eprint-prefix" | "--eprint-prefix") :: s :: rem ->
	eprint_prefix := s; parse_rec rem
    | ("-eprint-prefix" | "--eprint-prefix") :: [] ->
	usage ()
    | ("-nokeywords" | "-no-keywords" | "--no-keywords") :: rem ->
	print_keywords := false; parse_rec rem
    | ("-nolinks" | "-no-links" | "--no-links") :: rem -> 
	print_links := false; parse_rec rem
    | ("-nobiblinks" | "-no-bib-links" | "--no-bib-links") :: rem -> 
	links_in_bib_file := false; parse_rec rem
    | ("-nokeys" | "-no-keys" | "--no-keys") :: rem -> 
	nokeys := true; table := NoTable; parse_rec rem
    | ("-use-table" | "--use-table") :: rem -> 
	table := Table; parse_rec rem
    | ("-usekeys" | "-use-keys" | "--use-keys") :: rem ->
	use_keys := true; parse_rec rem
    | ("-rawurl" | "-raw-url" | "--raw-url") :: rem -> 
	raw_url := true; parse_rec rem
(*i
    | ("-tu" | "-titleurl" | "--title-url") :: rem -> 
	title_url := true; parse_rec rem
i*)
    | ("-heveaurl" | "-hevea-url" | "--hevea-url") :: rem -> 
	Latexscan.hevea_url := true; parse_rec rem
    | ("-linebreak" | "--linebreak") :: rem ->
	linebreak := true; parse_rec rem
    | ("-noheader" | "-no-header" | "--no-header") :: rem ->
	print_header := false; parse_rec rem
    | ("-nofooter" | "-no-footer" | "--no-footer") :: rem ->
	print_footer := false; parse_rec rem
    | ("-f" | "-field" | "--field") :: s :: rem ->
	add_field s; parse_rec rem
    | ("-f" | "-field" | "--field") :: [] ->
	usage()
    | ("-nf" | "-named-field" | "--named-field") :: s :: name :: rem ->
	add_named_field s name; parse_rec rem
    | ("-nf" | "-named-field" | "--named-field") :: ([_] | []) ->
	usage()
    | ("-note" | "--note") :: s :: rem ->
	add_note_field s; parse_rec rem
    | ("-note" | "--note") :: [] ->
	usage()
    | ("-note-html" | "--note-html") :: s :: rem ->
	add_note_html_field s; parse_rec rem
    | ("-note-html" | "--note-html") :: [] ->
	usage()
    | ("-ln" | "-labelname" | "--labelname" | "--label-name") :: rem ->
	use_label_name := true; parse_rec rem
    | ("-multiple" | "--multiple") :: rem ->
	multiple := true; parse_rec rem
    | ("-single" | "--single") :: rem ->
	multiple := false; both := false; print_keywords := false;
	bib_entries := false; single := true; parse_rec rem
    | ("-both" | "--both") :: rem ->
	both := true; parse_rec rem
    | ("-dl" | "--dl") :: rem ->
	table := DL; parse_rec rem
    | ("-unicode" | "--unicode") :: rem ->
	Latexmacros.unicode_entities (); parse_rec rem
    | ("-html-entities" | "--html-entities") :: rem ->
	Latexscan.html_entities := true;
	Latexmacros.html_entities (); parse_rec rem

    (* Controlling the translation *)
    | ("-m" | "-macros-from" | "--macros-from") :: f :: rem ->
	read_macros f; parse_rec rem
    | ("-m" | "-macros-from" | "--macros-from") :: [] ->
	usage()
 
    (* Sorting the entries *)
    | ("-d" | "-sort-by-date" | "--sort-by-date") :: rem ->
	sort := By_date; parse_rec rem
    | ("-a" | "-sort-as-bibtex" | "--sort-as-bibtex") :: rem ->
	sort := By_author; parse_rec rem
    | ("-u" | "-unsorted" | "--unsorted") :: rem ->
	sort := Unsorted; parse_rec rem
    | ("-r" | "-reverse-sort" | "--reverse-sort") :: rem ->
	reverse_sort := not !reverse_sort; parse_rec rem
    | ("-revkeys" | "--revkeys") :: rem ->
	reverse_sort := not !reverse_sort; revkeys := true; parse_rec rem

    (* Options for selecting keys *)
    | ("-citefile" | "--citefile") :: f :: rem ->
	use_cite_file := true;
	add_citations f;
	parse_rec rem
    | ("-citefile" | "--citefile") :: [] ->
	usage()
    | ("-e" | "-exclude" | "--exclude") :: k :: rem ->
	add_exclude k; parse_rec rem
    | ("-e" | "-exclude" | "--exclude") :: [] ->
	usage()
 
    (* Miscellaneous options *)
    | ("-o" | "-output" | "--output") :: f :: rem ->
	output_file := f;
	parse_rec rem
    | ("-o" | "-output" | "--output") :: [] ->
	usage()
    | ("-nobibsource" | "--nobibsource") :: rem ->
	bib_entries := false; parse_rec rem
    | ("-nodoc" | "--nodoc" | "-no-doc" | "--no-doc") :: rem -> 
	nodoc := true; parse_rec rem
    | ("-noexpand" | "-no-expand" | "--no-expand") :: rem -> 
	expand_abbrev_in_bib_output := false; parse_rec rem
    | ("-i" | "-ignore-errors" | "--ignore-errors") :: rem ->
	ignore_bibtex_errors := true; parse_rec rem

    | ("-suffix" | "--suffix") :: s :: rem ->
	file_suffix := s; link_suffix := s; parse_rec rem
    | ("-fsuffix" | "-file-suffix" | "--file-suffix") :: s :: rem ->
	file_suffix := s; parse_rec rem
    | ("-lsuffix" | "-link-suffix" | "--link-suffix") :: s :: rem ->
	link_suffix := s; parse_rec rem
    | ("-suffix" | "--suffix" | "-fsuffix" | "--file-suffix" | "-file-suffix" |
       "-lsuffix" | "-link-suffix" | "--link-suffix") :: [] ->
	usage()

    | ("-c" | "-command" | "--command") :: s :: rem ->
	command := s; parse_rec rem
    | ("-c" | "-command" | "--command") :: [] ->
	usage()
    | ("-h" | "-help" | "-?" | "--help") :: rem ->
	usage ~error:false ()
    | ("-v" | "-version" | "--version") :: _ ->
	Copying.banner "bibtex2html"; exit 0
    | ("-warranty" | "--warranty") :: _ ->
	Copying.banner "bibtex2html"; Copying.copying(); exit 0

    | ("-w" | "-warn-error" | "--warn-error") :: rem ->
	Options.warn_error := true; parse_rec rem
    | ("-q" | "-quiet" | "--quiet") :: rem ->
	Options.quiet := true; parse_rec rem
    | ("-debug" | "--debug") :: rem ->
	Options.debug := true; parse_rec rem
    | "-parse-only" :: rem ->
	parse_only := true; parse_rec rem
    | ("-print-keys" | "--print-keys") :: rem ->
	print_keys := true; parse_rec rem

    | [fbib] -> 
	if not (Sys.file_exists fbib) then begin
	  eprintf "%s: no such file\n" fbib;
	  exit 1
	end;
	let basename = Filename.basename fbib in
	if Filename.check_suffix basename ".bib" then
	  (fbib, Filename.chop_suffix basename ".bib")
	else begin
	  prerr_endline "bibtex2html: BibTeX file must have suffix .bib";
	  exit 1
	end
    | [] ->
	("","")
    | _ -> usage ()
  in 
    parse_rec (List.tl (Array.to_list Sys.argv))


(*s Main function. *)

let main () =
  let (fbib,f) = parse () in
  Copying.banner "bibtex2html";
  if fbib = "" then begin
    if not !title_spec then title := "bibtex2html output";
    begin match !output_file with
      | "" -> bib_entries := false
      | "-" -> output_file := ""; bib_entries := false
      | _ -> ()
    end
  end else begin
    input_file := f ^ ".bib";
    begin match !output_file with
      | "" -> output_file := f;
      | "-" -> output_file := ""; bib_entries := false
      | _ -> ()
    end;
    if not !title_spec then title := f
  end;
  Latexmacros.init_style_macros !style;
  (* producing the documents *)
  translate fbib

let _ = Printexc.catch main ()
