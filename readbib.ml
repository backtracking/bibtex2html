
(* [(read_entries_from_file f)] returns the BibTeX entries of the
   BibTeX file [f] (from standard input if [f=""]).  *)

let read_entries_from_file f =
  if f = "" then 
    Printf.eprintf "Reading from standard input...\n"
  else
    Printf.eprintf "Reading %s..." f; 
  flush stderr;
  Bibtex_lexer.reset();
  let chan = if f = "" then stdin else open_in f in
  try
    let el =
      Bibtex_parser.command_list Bibtex_lexer.token (Lexing.from_channel chan)
    in
    if f <> "" then close_in chan;
    Printf.eprintf "ok (%d entries).\n" (Bibtex.size el); flush stderr;
    el
  with
      Parsing.Parse_error | Failure "unterminated string" ->
	if f<>"" then close_in chan;
	Printf.eprintf "Parse error line %d.\n" !Bibtex_lexer.line;
	flush stderr;
	exit 1 
