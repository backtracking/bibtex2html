
(* [(read_entries_from_file f)] returns the BibTeX entries of the
   BibTeX file [f].  *)

let read_entries_from_file f =

  Printf.eprintf "Reading %s..." f; flush stderr;
  Bibtex_lexer.reset();
  let chan = open_in f in
  try
    let el =
      Bibtex_parser.command_list Bibtex_lexer.token (Lexing.from_channel chan)
    in
      close_in chan;
      Printf.eprintf "ok (%d entries).\n" (List.length el); flush stderr;
      el

  with
      Parsing.Parse_error | Failure "unterminated string" ->
	close_in chan;
	Printf.eprintf "Parse error line %d.\n" !Bibtex_lexer.line;
	flush stderr;
	exit 1 
;;

	
