
(* [(read_entries_from_file f)] returns the BibTeX entries of the
   BibTeX file [f].  *)

let read_entries_from_file f =

  Printf.printf "Reading %s..." f; flush stdout;
  Bibtex_lexer.reset();
  let chan = open_in f in
  try
    let el =
      Bibtex_parser.command_list Bibtex_lexer.token (Lexing.from_channel chan)
    in
      close_in chan;
      Printf.printf "ok (%d entries).\n" (List.length el); flush stdout;
      el

  with
      Parsing.Parse_error | Failure "unterminated string" ->
	close_in chan;
	Printf.printf "Parse error line %d.\n" !Bibtex_lexer.line;
	flush stdout;
	exit 1 
;;

	
