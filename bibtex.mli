(* 
 * bibtex.mli
 *)

type entry_type = string
		    
type key = string

type atom =
    Id of string
  | String of string

type fields = (string * string) list

type entry = entry_type * key * fields
		
type command = 
    Abbrev of string * string
  | Entry  of entry_type * key * (string * atom list) list

val expand : command list -> entry list

val sort : entry list -> entry list


(* access to the fields *)

val get_field : entry -> string -> string

val get_title : entry -> string
val get_year  : entry -> string
val get_month : entry -> string
val get_author : entry -> string

