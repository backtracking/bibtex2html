(*
 * bibtex2html
 *)

(* options *)

let excluded = ref ([] : string list)
let add_exclude k = excluded := k :: !excluded
let style = ref "plain"

type sort = Unsorted | By_date | By_author
let sort = ref Unsorted
let reverse_sort = ref false


(* sort of entries *)

let first p l =
  let rec first_rec = function
      [] -> raise Not_found
    | x::rem -> if p x then x else first_rec rem
  in
    first_rec l

let keep_combine keep find combine l1 l2 =
  let rec keep_rec = function
      [] ->
	[]
    | x::rem ->
	if keep x then
	  let y = first (find x) l2 in
	  (combine x y) :: (keep_rec rem)
	else
	  keep_rec rem
  in
    keep_rec l1

let keep_f (_,k,_) = not (List.mem k !excluded)

let find_f (_,k1,_) (_,k2,_) = k1=k2

let combine_f (c,_,b) e = c,b,e

let rev_combine_f x y = combine_f y x

let sort_entries entries bibitems =
  let el =
    if !sort = By_author then 
      keep_combine keep_f find_f combine_f bibitems entries
    else
      keep_combine keep_f find_f rev_combine_f entries bibitems
  in
  let sl = 
    if !sort = By_date then
      Sort.list (fun (_,_,e1) (_,_,e2) -> Bibtex.date_order e1 e2) el
    else
      el in
  if !reverse_sort then List.rev sl else sl


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
  let sl = sort_entries entries bibitems in
  Translate.format_list f sl


(* command line parsing *)

let usage () =
  prerr_endline "Usage: bibtex2html <options> filename";
  prerr_endline "  -s style   BibTeX style (plain, alpha, ...)";
  prerr_endline "  -d         sort by date";
  prerr_endline "  -a         sort as BibTeX (usually by author)";
  prerr_endline "  -u         unsorted i.e. same order as in .bib file (default)";
  prerr_endline "  -r         reverse the sort";
  prerr_endline "  -nodoc     only produces the body of the HTML documents";
  prerr_endline "  -suffix s  give an alternate suffix for HTML files";
  prerr_endline "  -e key     exclude an entry";
  exit 1

let parse () =
  let rec parse_rec = function
      "-nodoc" :: rem -> 
	Translate.nodoc := true ; parse_rec rem
    | "-d" :: rem ->
	sort := By_date ; parse_rec rem
    | "-a" :: rem ->
	sort := By_author ; parse_rec rem
    | "-u" :: rem ->
	sort := Unsorted ; parse_rec rem
    | "-r" :: rem ->
	reverse_sort := true ; parse_rec rem
    | "-h" :: rem ->
	usage ()
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


(* main *)

let main () =
  let fbib = parse () in
  let f =
    let basename = Filename.basename fbib in
    if Filename.check_suffix basename ".bib" then
      Filename.chop_suffix basename ".bib"
    else begin
      prerr_endline "BibTeX file must have suffix .bib !";
      exit 1
    end
      in

  (* Creating directory for html and bib files *)
  Sys.command ("mkdir " ^ f);

  (* producing the documents *)
  translate fbib f
;;

Printexc.catch main ()
