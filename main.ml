(*
 * bibtex2html
 *)

let excluded = ref ([] : string list)
let add_exclude k = excluded := k :: !excluded

let rec keep = function
    [] ->
      []
  | ((_,k,_) as e)::rem ->
      if List.mem k !excluded then keep rem else e::(keep rem)

let translate fname f =
  print_string ("Reading " ^ f ^ "...\n"); flush stdout;
  let chan = open_in fname in
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
  let kel = keep el in
  Translate.format_list f kel

let usage () =
  prerr_endline "Usage: bibtex2html <options> filename";
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
    | "-s" :: s :: rem ->
	Translate.suffix := s ; parse_rec rem
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
