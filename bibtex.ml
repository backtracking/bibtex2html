(*
 * bibtex.ml
 *)

type entry_type = string
		    
type key = string
	     
type atom =
    Id of string
  | String of string

type fields = (string * string) list

type entry = entry_type * key * fields
		
type command = 
    Comment
  | Preamble of string
  | Abbrev of string * atom list
  | Entry  of entry_type * key * (string * atom list) list


let abbrev_table  = ref ([] : (string * string) list)

let add_abbrev a =
  abbrev_table := a :: !abbrev_table

let find_abbrev s =
  List.assoc s !abbrev_table

let assoc_months = 
  [ "JAN", "January" ;
    "FEB", "February" ;
    "MAR", "March" ;
    "APR", "April" ;
    "MAY", "May" ;
    "JUN", "June" ;
    "JUL", "July" ;
    "AUG", "August" ;
    "SEP", "September" ;
    "OCT", "October" ;
    "NOV", "November" ;
    "DEC", "December" ]

let rec expand_list = function
    [] -> 
      ""
  | (Id s)::rem ->
      (try find_abbrev s with Not_found -> s) ^ (expand_list rem)

  | (String s)::rem ->
      s ^ (expand_list rem)

let rec expand_fields = function
    [] -> 
      []
  | ("MONTH" as n,l) :: rem ->
      let s = expand_list l in
      	(n,try List.assoc (String.uppercase s) assoc_months 
	   with Not_found -> s) 
      	:: (expand_fields rem)
  | (n,l) :: rem -> 
      (n,expand_list l) :: (expand_fields rem)

let macros_in_preamble s =
  try
    let lb = Lexing.from_string s in Latexscan.read_macros lb
  with _ -> ()

let rec expand = function
    [] ->
      []
  | (Abbrev (a,l)) :: rem ->
      let s = expand_list l in
      add_abbrev (a,s) ; expand rem
  | (Entry (t,k,f)) :: rem ->
      (t,k,expand_fields f) :: (expand rem)
  | (Preamble s) :: rem ->
      macros_in_preamble s;
      expand rem
  | Comment :: rem ->
      expand rem


(* sort BibTeX entries by decreasing dates *)

let int_of_month = function
  | "January" -> 0
  | "February" -> 1
  | "March" -> 2
  | "April" -> 3
  | "May" -> 4
  | "June" -> 5
  | "July" -> 6
  | "August" -> 7
  | "September" -> 8
  | "October" -> 9
  | "November" -> 10 
  | "December" -> 11
  | _ -> 0 (* TODO *)

let date_order (_,_,f1) (_,_,f2) =
  try
    let a1 = int_of_string (List.assoc "YEAR" f1) in
    let a2 = int_of_string (List.assoc "YEAR" f2) in
    (a1 < a2) or
    ((a1 = a2) & 
     (let m1 = try int_of_month (List.assoc "MONTH" f1) with Not_found -> 0 in
      let m2 = try int_of_month (List.assoc "MONTH" f2) with Not_found -> 0 in
      m1 < m2))
  with Not_found | Failure "int_of_string" -> 
    failwith "year not present or incorrect"


(* access to the fields *)

let get_field (_,_,f) s = List.assoc (String.uppercase s) f

let get_title e = get_field e "TITLE"

let get_year e = get_field e "YEAR"

let get_month e = get_field e "MONTH"

let get_author e = get_field e "AUTHOR"

