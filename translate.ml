(*
 * Translate BibTeX entries into HTML documents.
 *)

(* options *)

let nodoc = ref false
let sort_by_date = ref false
let suffix = ref ".html"

(* latex2html : to print LaTeX strings in HTML format *)

let latex2html ch s =
  Latexmacros.out_channel := ch;
  Latexscan.main (Lexing.from_string s)

let safe_title e =
  try Bibtex.get_title e with Not_found -> "No title"

let safe_author e =
  try Bibtex.get_author e with Not_found -> "No author"

let format_date ch e =
  output_string ch
    (try Bibtex.get_month e with Not_found -> "");
  output_string ch " ";
  output_string ch
    (try Bibtex.get_year e with Not_found -> "");
  flush ch

type command = 
    Comma
  | Dot
  | String of string * (out_channel -> string -> unit)
  | Field  of string * (out_channel -> string -> unit) * bool

let normal = latex2html

let emphasize ch s =
  Html.open_em ch;
  latex2html ch s;
  Html.close_em ch

let date_dot = [ Field ("month",normal,false) ; 
		 String (" ",output_string);
		 Field ("year",normal,true) ; 
		 Dot ]

let get_fields = function
    "BOOK"          -> 
      [ Field ("publisher",normal,true) ; Comma ] @ date_dot
	
  | "INPROCEEDINGS" -> 
      [ String ("In ",normal) ; Field ("booktitle",emphasize,true) ; Dot ]
      @ date_dot

  | "ARTICLE" -> 
      [ Field ("journal",emphasize,true) ; Comma ]
      @ date_dot

  | "TECHREPORT" ->
      [ Field ("institution",normal,true) ; Comma ]
      @ date_dot

  | "UNPUBLISHED" ->
      [ String ("Unpublished",normal) ; Comma ]
      @ date_dot

  | "INCOLLECTION" ->
      [ String ("In ",normal) ; Field ("booktitle",emphasize,true) ; Dot ]
      @ date_dot

  | "PHDTHESIS" ->
      [ String ("Ph. D. Thesis",normal) ; Comma ;
	Field ("school",normal,true) ; Comma ]
      @ date_dot

  | _               ->
      date_dot

let warning k name =
  Printf.fprintf stderr "Warning: field %s is missing in entry %s\n" name k

let print_commands ch ((_,k,f) as e) fi =
  let rec print = function
      [] -> ()
    | Comma :: rem -> 
	output_string ch ", " ; print rem 
    | Dot :: rem -> 
	output_string ch ". " ; print rem
    | String (s,f) :: rem -> 
	f ch s; print rem
    | Field (n,f,nec) :: rem ->
	begin
	  try  let s = Bibtex.get_field e n in f ch s
	  with Not_found -> if nec then warning k n
	end;
	print rem
  in
    print fi

let format_type ch ((t,_,_) as e) =
  print_commands ch e (get_fields t)

(* summary file f.html *)

let one_entry_summary basen ch ((_,k,f) as e) =
  Html.open_balise ch "li"; output_string ch " ";
  let url = Filename.concat basen (k ^ !suffix) in
  Html.open_href ch url;
  latex2html ch (Bibtex.get_field e "BIBITEM");
  Html.close_href ch;
  Html.paragraph ch;
  output_string ch "\n"

let summary basen el =
  let ch = open_out (basen ^ !suffix) in
    if not !nodoc then
      Html.open_document ch (fun () -> output_string ch basen);
    Html.open_balise ch "ul";
    List.iter (one_entry_summary basen ch) el;
    Html.close_balise ch "ul";
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
	 if a <> "BIBITEM" then begin
	   output_string ch "  ";
	   output_string ch (String.lowercase a);
	   output_string ch " = ";
	   output_string ch ("{" ^ v ^ "},\n")
	 end
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
  let title = safe_title e in
  
    if not !nodoc then 
      Html.open_document ch (fun () -> latex2html ch title);

    Html.open_balise ch "font size=4";
    output_string ch "\n\n";
    latex2html ch (Bibtex.get_field e "BIBITEM");
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
      [ "URL" ; "URL1" ; "URL2" ; "URL3"; "URL4" ; "URL5" ];

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

let format_list f el =
  let l = if !sort_by_date then Bibtex.sort el else el in
    summary f l;
    List.iter (bib_file f) l;
    List.iter (html_file f) l

