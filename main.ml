(*
 * bibtex2html
 *)

(* options *)

let excluded = ref ([] : string list)
let add_exclude k = excluded := k :: !excluded
let style = ref "plain"

let combine_and_keep entries bibitems = 
  let findbibitem k = 
    let rec find = function
	[] -> 
	  failwith (Printf.sprintf "entry %s not found after BibTeX call !" k)
      | (_,k',b) :: rem ->
	  if k=k' then b else find rem
    in find bibitems 
  in
  let rec keep = function
    [] ->
      []
  | ((t,k,f) as e)::rem ->
      if List.mem k !excluded then 
	keep rem 
      else (t,k,("BIBITEM",findbibitem k)::f)::(keep rem)
  in
    keep entries

(* we use BibTeX itself to format the entries. Operations:
 *
 * 1. create an auxiliary file tmp.aux
 * 2. call bibtex on it
 * 3. read to resulting tmp.bbl file to get the formatted entries
 *)

let create_aux_file fbib tmp =
  let ch = open_out (tmp ^ ".aux") in
  output_string ch "\\relax\n\\bibstyle{";
  output_string ch !style;
  output_string ch "}\n\\citation{*}\n\\bibdata{";
  output_string ch (Filename.chop_suffix fbib ".bib");
  output_string ch "}\n";
  close_out ch

let clean tmp =
  begin try Sys.remove (tmp ^ ".aux") with _ -> () end;
  begin try Sys.remove (tmp ^ ".blg") with _ -> () end;
  begin try Sys.remove (tmp ^ ".bbl") with _ -> () end;
  begin try Sys.remove tmp            with _ -> () end
  
let call_bibtex tmp =
  match 
    Sys.command ("bibtex " ^ tmp)
  with
      0 -> ()
    | _ -> failwith "error while running bibtex"

let read_bbl tmp =
  let rec read_items acc lb =
    try
      let item = Bbl_lexer.bibitem lb in
      read_items (item::acc) lb
    with
	End_of_file -> List.rev acc 
  in
  let fbbl = tmp ^ ".bbl" in
  Printf.printf "Reading %s...\n" fbbl; flush stdout;
  let ch = open_in fbbl in
  let lexbuf = Lexing.from_channel ch in
  Bbl_lexer.skip_header lexbuf;
  let items = read_items [] lexbuf in
  close_in ch;
  clean tmp;
  items

let get_bibitems fbib =
  let tmp = Filename.temp_file "bib2html" "" in
  try
    create_aux_file fbib tmp;
    call_bibtex tmp;
    read_bbl tmp
  with
    e -> clean tmp ; raise e

let get_bibtex_entries fbib =
  Printf.printf "Reading %s...\n" fbib; flush stdout;
  let chan = open_in fbib in
  let el =
    try
      Bibtex_parser.entry_list Bibtex_lexer.token (Lexing.from_channel chan)
    with
	Parsing.Parse_error ->
	  close_in chan;
	  print_string "Parse error.\n";
	  flush stdout;
	  exit 1 in
  close_in chan;
  el

let translate fbib f =
  let entries = get_bibtex_entries fbib in
  let bibitems = get_bibitems fbib in
  let kel = combine_and_keep entries bibitems in
  Translate.format_list f kel

let usage () =
  prerr_endline "Usage: bibtex2html <options> filename";
  prerr_endline "  -s style   BibTeX style (plain, alpha, ...)";
  prerr_endline "  -d         sort by date";
  prerr_endline "  -nodoc     only produces the body of the HTML documents";
  prerr_endline "  -suffix s  give an alternate suffix for HTML files";
  prerr_endline "  -e key     exclude an entry";
  exit 1

let parse () =
  let rec parse_rec = function
      "-nodoc" :: rem -> 
	Translate.nodoc := true ; parse_rec rem
    | "-d" :: rem ->
	Translate.sort_by_date := true ; parse_rec rem
    | "-suffix" :: s :: rem ->
	Translate.suffix := s ; parse_rec rem
    | "-suffix" :: [] ->
	usage()
    | "-s" :: s :: rem ->
	style := s ; parse_rec rem
    | "-s" :: [] ->
	usage()
    | "-e" :: k :: rem ->
	add_exclude k ; parse_rec rem
    | "-e" :: [] ->
	usage()
    | [f] -> f
    | _ -> usage ()
  in 
    parse_rec (List.tl (Array.to_list Sys.argv))

let main () =
  let fbib = parse () in
  let f =
    let basename = Filename.basename fbib in
    if Filename.check_suffix basename ".bib" then
      Filename.chop_suffix basename ".bib"
    else
      basename in

  (* Creating directory for html and bib files *)
  Sys.command ("mkdir " ^ f);

  (* producing the documents *)
  translate fbib f
;;

Printexc.catch main ()
