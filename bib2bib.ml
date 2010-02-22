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

open Printf
open Bibtex


(* command-line arguments *)

let input_file_names = ref ([] : string list)

let bib_output_file_name = ref ""

let php_output_file_name = ref ""

let cite_output_file_name = ref ""

let get_input_file_name f =
  input_file_names := f :: !input_file_names

let condition = ref Condition.True

let add_condition c = 
  try
    let c = Parse_condition.condition c in
    condition := if !condition = Condition.True then c 
    else Condition.And(!condition,c)
  with
      Condition_lexer.Lex_error msg ->
	prerr_endline ("Lexical error in condition: "^msg);
	exit 1
    | Parsing.Parse_error ->
	prerr_endline "Syntax error in condition";
	exit 1


let expand_abbrevs = ref false

let expand_xrefs = ref false

let no_comment = ref false

let sort_criteria = ref []

let reverse_sort = ref false

let remove_fields = ref []

let rename_field = ref ""

let rename_fields = ref []

let args_spec =
  [
    ("-ob", Arg.String (fun f -> bib_output_file_name := f),
     "<f> uses <f> as name for output bibliography file");
    ("-oc", Arg.String (fun f -> cite_output_file_name := f),
     "<f> uses <f> as name for output citations file");
    ("--php-output", Arg.String (fun f -> php_output_file_name := f),
     "<f> outputs resulting bibliography in PHP syntax in file <f>");
    ("-c", Arg.String (add_condition),"<c> adds <c> as a filter condition");
    ("-w", Arg.Set Options.warn_error, "stop on warning");
    ("--warn-error", Arg.Set Options.warn_error, "stop on warning");
    ("-d", Arg.Set Options.debug, "debug flag");
    ("-q", Arg.Set Options.quiet, "quiet flag");
    ("--quiet", Arg.Set Options.quiet, "quiet flag");
    ("-s", Arg.String 
       (fun s -> sort_criteria := (String.lowercase s):: !sort_criteria),
     "<c> sort with respect to keys (if c=$keys) or a given field <c>");
    ("-r", Arg.Set reverse_sort,
     "reverse the sort order");
    ("--no-comment", Arg.Unit (fun () -> no_comment := true), 
     "do not add extra comments at beginning");
    ("--remove", Arg.String 
       (fun s -> remove_fields := (String.lowercase s):: !remove_fields),
          "<f> removes the field <f>");
    ("--rename", 
     Arg.Tuple 
       [ Arg.Set_string rename_field ;
         Arg.String (fun s -> rename_fields := 
                       (String.lowercase !rename_field,
                        String.lowercase s):: !rename_fields)], 
       "<f1> <f2> rename field <f1> into <f2>");
    ("--expand", Arg.Unit (fun () -> expand_abbrevs := true), 
     "expand the abbreviations");
    ("--expand-xrefs", Arg.Unit (fun () -> expand_xrefs := true), 
     "expand the cross-references");
    ("--version", Arg.Unit (fun () -> Copying.banner "bib2bib"; exit 0), 
     "print version and exit");
    ("--warranty", 
     Arg.Unit (fun () -> Copying.banner "bib2bib"; Copying.copying(); exit 0),
     "display software warranty")
  ]

let output_cite_file keys = 
  if !cite_output_file_name = "" then
    prerr_endline "No citation file output (no file name specified)" 
  else 
    try
      let ch = open_out !cite_output_file_name in
      KeySet.iter (fun k -> output_string ch (k ^ "\n")) keys;
      close_out ch
    with 
	Sys_error msg ->
	  prerr_endline ("Cannot write output citations file (" ^ msg ^ ")");
	  exit 1



let output_bib_file remove rename biblio keys = 
  try 
    let ch = 
      if !bib_output_file_name = "" 
      then stdout 
      else open_out !bib_output_file_name 
    in 
    let cmd = 
      List.fold_right 
	(fun s t -> 
	   if String.contains s ' ' 
	   then 
	     if String.contains s '\'' 
	     then " \"" ^ s ^ "\"" ^ t 
	     else " '" ^ s ^ "'" ^ t 
	   else " " ^ s ^ t) 
	(Array.to_list Sys.argv) 
	"" 
    in 
    let comments =
      if !no_comment then empty_biblio else
      add_new_entry
	(Comment ("Command line:" ^ cmd))
	(add_new_entry 
	   (Comment 
	      ("This file has been generated by bib2bib "
	       ^ Version.version))
	   empty_biblio)
    in
    let biblio = merge_biblios comments biblio in
    Biboutput.output_bib ~remove ~rename ~html:false ch biblio keys;     
    if !bib_output_file_name <> "" then close_out ch
  with Sys_error msg ->  
    prerr_endline ("Cannot write output bib file (" ^ msg ^ ")"); 
    exit 1 


let output_php_file remove rename biblio keys = 
  if !php_output_file_name <> "" then
    try
      let ch = open_out !php_output_file_name in
      output_string ch "<?php
$parsed_bibfile = 
Array
(
";
    Biboutput.output_bib ~remove ~rename ~html:false ~php:true ch biblio keys;
    output_string ch "
 );
?>
";
    close_out ch
    with Biboutput.Bad_input_for_php msg ->
      eprintf "error while producing PHP output: %s\n" msg;
      exit 2
      

let rec make_compare_fun db criteria c1 c2 =
  match criteria with
    | [] -> 0	
    | field :: rem ->
	let comp = 
	  match field with
	    | "$key"  ->
		begin
		  match (c1,c2) with
		    | (Abbrev(s1,_),Abbrev(s2,_))
		    | (Entry(_,s1,_),Entry(_,s2,_)) ->
			compare s1 s2
		    | _ -> 0
		end
	    | "$type" ->
		begin
		  match (c1,c2) with
		    | (Entry(s1,_,_),Entry(s2,_,_)) ->
			compare s1 s2
		    | _ -> 0
		end
	    | "$date" -> 
		begin
		  match (c1,c2) with
		    | (Entry(s1,t1,l1),Entry(s2,t2,l2)) ->
			Expand.date_compare db 
			  (s1,t1,Expand.expand_fields l1) 
			  (s2,t2,Expand.expand_fields l2)
		    | _ -> 0
		end
	    | f ->
		begin
		  match (c1,c2) with
		    | (Entry(_,_,l1),Entry(_,_,l2)) ->
			let s1 = 
			  try 
			    match List.assoc field l1 with
			      | [Bibtex.String(s)] -> s
			      | [Bibtex.Id(s)] -> s
			      | _ -> ""
			  with
			      Not_found -> ""
			and s2 =
			  try 
			    match List.assoc field l2 with
			      | [Bibtex.String(s)] -> s
			      | [Bibtex.Id(s)] -> s
			      | _ -> ""
			  with
			      Not_found -> ""
			in
			compare s1 s2
		    | _ -> 0
		end
	in
	if comp = 0
	then make_compare_fun db rem c1 c2
	else 
	  if !reverse_sort then -comp else comp
;;
	

let usage = "Usage: bib2bib [options] <input file names>\nOptions are:"

let main () =
  Arg.parse args_spec get_input_file_name usage;
  Copying.banner "bib2bib";
  if !Options.debug then
    begin
      eprintf "command line:\n";
      for i = 0 to pred (Array.length Sys.argv) do
	eprintf "%s\n" Sys.argv.(i)
      done;
    end;
  if !input_file_names = [] then input_file_names := [""];
  if !Options.debug then begin 
    Condition.print !condition; printf "\n"
  end;
  let all_entries =
    List.fold_right
      (fun file accu -> 
	 merge_biblios accu (Readbib.read_entries_from_file file))
      !input_file_names
      empty_biblio
  in 
  let abbrv_expanded = Bibtex.expand_abbrevs all_entries in
  let xref_expanded = Bibtex.expand_crossrefs abbrv_expanded in
  let matching_keys =
    Bibfilter.filter xref_expanded
      (fun e k f -> Condition.evaluate_cond e k f !condition) 
  in
  if KeySet.cardinal matching_keys = 0 then
    begin
      eprintf "Warning: no matching reference found.\n";
      if !Options.warn_error then exit 2;
    end;
  
  let user_expanded = 
    if !expand_abbrevs then 
      if !expand_xrefs then xref_expanded else abbrv_expanded 
    else 
      if !expand_xrefs then Bibtex.expand_crossrefs all_entries 
      else all_entries
  in
  let needed_keys = Bibfilter.saturate user_expanded matching_keys in
  (* this should be to right place to sort the output bibliography *)
  let final_bib =
    if !sort_criteria = [] then user_expanded
    else      
      let comp = make_compare_fun (Expand.expand user_expanded) (List.rev !sort_criteria) in
      eprintf "Sorting...";
      let b = Bibtex.sort comp user_expanded in
      eprintf "done.\n";
      b
  in
  output_cite_file matching_keys;
  output_bib_file !remove_fields !rename_fields final_bib (Some needed_keys);
  output_php_file !remove_fields !rename_fields final_bib (Some needed_keys)


let _ = 
  Printexc.catch main ()



