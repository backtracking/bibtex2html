(*
 * Translate BibTeX entries into HTML documents.
 *)

(* options *)

let nodoc = ref false
let suffix = ref ".html"


(* first pass to get the crossrefs *)

let (cite_tab : (string,string) Hashtbl.t) = Hashtbl.create 17

let cpt = ref 0

let first_pass l =
  let rec pass = function
      [] -> ()
    | (None,_,(_,k,_))::rem ->
	incr cpt;
	Hashtbl.add cite_tab k (string_of_int !cpt);
	pass rem
    | (Some c,_,(_,k,_))::rem ->
	Hashtbl.add cite_tab k c;
	pass rem
  in
    cpt := 0;
    Hashtbl.clear cite_tab;
    pass l


(* latex2html : to print LaTeX strings in HTML format *)

open Latexmacros

let in_summary = ref false
let directory = ref ""

let cite k =
  try
    let url =
      if !in_summary then 
	!directory ^ "/" ^ k ^ !suffix 
      else
	k ^ !suffix in
    let c = Hashtbl.find cite_tab k in
      print_s ("<A HREF=\"" ^ url ^ "\">[" ^ c ^ "]</A>")
  with
      Not_found -> print_s "[?]"
;;

def "\\cite" [ Raw_arg cite ];;
def "\\etalchar" [ Print "<sup>" ; Raw_arg print_s ; Print "</sup>" ];;

let latex2html ch s =
  Latexmacros.out_channel := ch;
  Latexscan.main (Lexing.from_string s)

let safe_title e =
  try Bibtex.get_title e with Not_found -> "No title"


(* summary file f.html *)

let one_entry_summary basen ch (_,b,((_,k,f) as e)) =
  let url = Filename.concat basen (k ^ !suffix) in
  output_string ch "\n\n";
  Html.anchor ch k;
  Html.open_balise ch "tr valign=top";

  output_string ch "\n";
  Html.open_balise ch "td";
  Html.open_href ch url;
  latex2html ch ("[" ^ (Hashtbl.find cite_tab k) ^ "]");
  Html.close_href ch;
  Html.close_balise ch "td";

  output_string ch "\n";
  Html.open_balise ch "td";
  latex2html ch b;
  Html.close_balise ch "td";

  output_string ch "\n";
  Html.paragraph ch;
  Html.close_balise ch "tr";
  output_string ch "\n"

let summary basen el =
  let ch = open_out (basen ^ !suffix) in
    if not !nodoc then
      Html.open_document ch (fun () -> output_string ch basen);
    output_string ch "\n";
    Html.open_balise ch "table";
    in_summary := true;
    List.iter (one_entry_summary basen ch) el;
    in_summary := false;
    Html.close_balise ch "table";
    if not !nodoc then Html.close_document ch;
    close_out ch
;;


(* BibTeX file for one entry f/key.bib *)

let bib_file f (_,_,(t,k,fs)) =
  let fn = Filename.concat f (k ^ ".bib") in
  let ch = open_out fn in

    output_string ch ("@" ^ t ^ "{" ^ k ^ ",\n");
    List.iter
      (fun (a,v) ->
	 output_string ch "  ";
	 output_string ch (String.lowercase a);
	 output_string ch " = ";
	 output_string ch ("{" ^ v ^ "},\n")
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

let html_file f (_,b,((t,k,_) as e)) =
  let fn = Filename.concat f (k ^ !suffix) in
  let ch = open_out fn in
  let title = safe_title e in
  
    if not !nodoc then 
      Html.open_document ch (fun () -> latex2html ch title);

    Html.open_balise ch "font size=4";
    output_string ch "\n\n";
    latex2html ch b;
    Html.paragraph ch;
    output_string ch "\n";

    (* abstract *)
    begin
      try
      	let a = Bibtex.get_field e "abstract" in
	  Html.open_b ch; output_string ch "Abstract:"; Html.close_b ch;
	  Html.paragraph ch; output_string ch "\n";
	  Html.open_balise ch "blockquote"; output_string ch "\n";
	  latex2html ch a;
	  Html.close_balise ch "blockquote"; output_string ch "\n";
	  Html.paragraph ch; output_string ch "\n"
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
      [ "URL" ; "URL0" ; "URL1" ; "URL2" ; "URL3"; "URL4" ; "URL5" ;
        "DVI" ; "PS" ; "DOCUMENTURL" ; "URLPS" ; "URLDVI" ];

    (* link to the BibTeX file *)
    let bibfile = k ^ ".bib" in
    Html.open_href ch bibfile;
    output_string ch "BibTeX reference";
    Html.close_href ch;

    output_string ch "\n";
    if not !nodoc then Html.close_document ch;
    flush ch;
    close_out ch


(* main function *)

let format_list f l =
  first_pass l;
  directory := f;
  summary f l;
  List.iter (bib_file f) l;
  List.iter (html_file f) l

