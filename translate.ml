(*
 * Translate BibTeX entries into HTML documents.
 *)

let nodoc = ref false
let sort_by_date = ref false
let suffix = ref ".html"

let latex2html ch s =
  Latexmacros.out_channel := ch;
  Latexscan.main (Lexing.from_string s)

let format_date ch e =
  output_string ch
    (try Bibtex.get_month e with Not_found -> "");
  output_string ch " ";
  output_string ch
    (try Bibtex.get_year e with Not_found -> "");
  flush ch

(* summary file f.html *)

let one_entry_summary basen ch ((_,k,f) as e) =
  output_string ch "<li> ";
  let url = Filename.concat basen (k ^ !suffix) in
  Html.open_href ch url;
  latex2html ch
    (try Bibtex.get_title e with Not_found -> "");
  Html.close_href ch;
  output_string ch ", ";
  format_date ch e;
  output_string ch ".";
  output_string ch "\n<p>\n"

let summary basen el =
  let ch = open_out (basen ^ !suffix) in
    if not !nodoc then
      Html.open_document ch (fun () -> output_string ch basen);
    output_string ch "<ul>";
    List.iter (one_entry_summary basen ch) el;
    output_string ch "</ul>";
    if not !nodoc then Html.close_document ch;
    close_out ch
;;


(* BibTeX file for one entry f/key.bib *)

let bib_file f (t,k,fs) =
  let fn = Filename.concat f (k ^ ".bib") in
  let ch = open_out fn in

    output_string ch ("@" ^ t ^ "{" ^ k ^ ",\n");
    List.iter
      (fun (a,v) ->
	 output_string ch "  ";
	 output_string ch (String.lowercase a);
	 output_string ch " = ";
	 output_string ch ("{" ^ v ^ "},\n");
      ) fs;
    output_string ch "}\n";
    
    flush ch;
    close_out ch

(* HTML file for one entry f/key.html *)

let file_type f =
  if List.exists (fun s -> Filename.check_suffix f s) 
    [ ".dvi" ; ".dvi.gz" ; ".dvi.Z" ] then 
    "DVI"
  else if List.exists (fun s -> Filename.check_suffix f s) 
    [ ".ps" ; ".ps.gz" ; ".ps.Z" ] then
    "PS"
  else
    "Available here"

let html_file f ((t,k,_) as e) =
  let fn = Filename.concat f (k ^ !suffix) in
  let ch = open_out fn in
  let title = try Bibtex.get_title e with Not_found -> "No title" in
  
    if not !nodoc then 
      Html.open_document ch (fun () -> latex2html ch title);

    (* title *)
    Html.open_h ch 1;
    latex2html ch title;
    Html.close_h ch 1;
    output_string ch "\n";
    output_string ch "<font size=4>\n\n";

    let author = try Bibtex.get_author e with Not_found -> "No author" in
    latex2html ch author;
    output_string ch "<p>\n";

    format_date ch e; 
    output_string ch "\n<p>\n";

    (* abstract *)
    begin
      try
      	let a = Bibtex.get_field e "abstract" in
	  output_string ch "<b>Abstract:</b><p>\n";
	  output_string ch "<quote>\n";
	  latex2html ch a;
	  output_string ch "</quote>\n<p>\n"
      with Not_found -> ()
    end;

    (* URL's *)
    List.iter (fun u -> 
		   try
		     let u = Bibtex.get_field e u in
		     let s = file_type u in
		       Html.open_href ch u;
		       output_string ch s;
		       Html.close_href ch;
		       output_string ch "&nbsp;&nbsp;"
		   with Not_found -> ())
      [ "URL" ; "URL1" ; "URL2" ; "URL3"; "URL4" ; "URL5" ];

    (* link to the BibTeX file *)
    let bibfile = k ^ ".bib" in
    Html.open_href ch bibfile;
    output_string ch "BibTeX reference";
    Html.close_href ch;

    output_string ch "</dl>\n";
    if not !nodoc then Html.close_document ch;
    flush ch;
    close_out ch

(* main function *)

let format_list f el =
  let l = if !sort_by_date then Bibtex.sort el else el in
    summary f l;
    List.iter (bib_file f) l;
    List.iter (html_file f) l

